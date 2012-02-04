%%  TODO: create new object from UI, add a toolbar ? 
%%  get two column, simliar to sys panel
%%  adding super class info to the list
%% 

-module(console_pool_wx).

-behaviour(wx_object).

-export([start_link/2]).

%% wx_object callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3, handle_call/3,
	 handle_event/2, handle_cast/2]).

-include_lib("wx/include/wx.hrl").
-include("console_backend.hrl").
-include("console_defs.hrl").
-include("etop_defs.hrl").

%% Defines
-define(COL_PID,  0).
-define(COL_NAME, ?COL_PID+1).
%%-define(COL_TIME, 2).
-define(COL_, ?COL_NAME+1).
-define(COL_MEM,  ?COL_+1).
-define(COL_MSG,  ?COL_MEM+1).
-define(COL_FUN,  ?COL_MSG+1).

-define(ID_KILL, 201).
-define(ID_ATTRIBUTE, 301).
-define(ID_PROC, 202).
-define(ID_REFRESH, 203).
-define(ID_REFRESH_INTERVAL, 204).
-define(ID_DUMP_TO_FILE, 205).
-define(ID_TRACE_PIDS, 206).
-define(ID_TRACE_NAMES, 207).
-define(ID_TRACE_NEW, 208).
-define(ID_TRACE_ALL, 209).
-define(ID_ACCUMULATE, 210).

-define(TRACE_PIDS_STR, "Trace selected process identifiers").
-define(TRACE_NAMES_STR, "Trace selected processes, "
	"if a process have a registered name "
	"processes with same name will be traced on all nodes").


%% Records

-record(sort,
	{
	  sort_key=?COL_,
	  sort_incr=false
	}).

-record(holder, {parent,
		 info,
		 sort=#sort{},
		 accum=[],
		 attrs,
		 node,
		 backend_pid
		}).

-record(state, {parent,
		grid,
		panel,
		popup_menu,
		parent_notebook,
		timer,
		procinfo_menu_pids=[],
		sel={[], []},
		holder}).

start_link(Notebook, Parent) ->
    wx_object:start_link(?MODULE, [Notebook, Parent], []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([Notebook, Parent]) ->
    Attrs = console_lib:create_attrs(),
    Self = self(),
    Holder = spawn_link(fun() -> init_table_holder(Self, Attrs) end),
    {PoolPanel, State} = setup(Notebook, Parent, Holder),
    {PoolPanel, State#state{holder=Holder}}.

setup(Notebook, Parent, Holder) ->
    PoolPanel = wxPanel:new(Notebook, []),

    Grid  = create_list_box(PoolPanel, Holder),
    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Grid, [{flag, ?wxEXPAND bor ?wxALL},
			      {proportion, 1},
			      {border,4}]),

    wxWindow:setSizer(PoolPanel, Sizer),

    State = #state{parent=Parent,
		   grid=Grid,
		   panel=PoolPanel,
		   parent_notebook=Notebook,
		   holder=Holder,
		   timer={false, 10}
		   },
    {PoolPanel, State}.


%% UI-creation

create_pro_menu(Parent, Holder) ->
    MenuEntries = [{"File",
		    [#create_menu{id=?ID_DUMP_TO_FILE, text="Dump to file"}]},
		   {"View",
		    [#create_menu{id=?ID_ACCUMULATE, text="Accumulate",
				  type=check,
				  check=call(Holder, {get_accum, self()})},
		     separator,
		     #create_menu{id=?ID_REFRESH, text="Refresh\tCtrl-R"},
		     #create_menu{id=?ID_REFRESH_INTERVAL, text="Refresh Interval"}]},
		   {"Trace",
		    [#create_menu{id=?ID_TRACE_PIDS, text="Trace processes"},
		     #create_menu{id=?ID_TRACE_NAMES, text="Trace named processes (all nodes)"},
		     #create_menu{id=?ID_TRACE_NEW, text="Trace new processes"}
		     %% , #create_menu{id=?ID_TRACE_ALL_MENU, text="Trace all processes"}
		    ]}
		  ],
    object_console:create_menus(Parent, MenuEntries).

create_list_box(Panel, Holder) ->
    Style = ?wxLC_REPORT bor ?wxLC_VIRTUAL bor ?wxLC_HRULES,
    ListCtrl = wxListCtrl:new(Panel, [{style, Style},
				      {onGetItemText,
				       fun(_, Row, Col) ->
					       call(Holder, {get_row, self(), Row, Col})
				       end},
				      {onGetItemAttr,
				       fun(_, Item) ->
					       call(Holder, {get_attr, self(), Item})
				       end}
				     ]),
    Li = wxListItem:new(),
    AddListEntry = fun({Name, Align, DefSize}, Col) ->
			   wxListItem:setText(Li, Name),
			   wxListItem:setAlign(Li, Align),
			   wxListCtrl:insertColumn(ListCtrl, Col, Li),
			   wxListCtrl:setColumnWidth(ListCtrl, Col, DefSize),
			   Col + 1
		   end,
    ListItems = [
%% 		 {"Name", ?wxLIST_FORMAT_CENTRE,  80},
		 {"Name", ?wxLIST_FORMAT_LEFT, 100},
%%		 {"Time", ?wxLIST_FORMAT_CENTRE, 50}, 
		 {"Counter", ?wxLIST_FORMAT_LEFT, 120},
		 {"Limits", ?wxLIST_FORMAT_RIGHT, 80},
		 {"Queue",  ?wxLIST_FORMAT_RIGHT, 350},
		 {"Current State", ?wxLIST_FORMAT_LEFT,  100}],
    lists:foldl(AddListEntry, 0, ListItems),
    wxListItem:destroy(Li),

    wxListCtrl:setItemCount(ListCtrl, 1),
    wxListCtrl:connect(ListCtrl, size, [{skip, true}]),
    wxListCtrl:connect(ListCtrl, command_list_item_activated),
    wxListCtrl:connect(ListCtrl, command_list_item_right_click),
    wxListCtrl:connect(ListCtrl, command_list_col_click),
    %% Use focused instead of selected, selected doesn't generate events
    %% for all multiple selections on Linux
    wxListCtrl:connect(ListCtrl, command_list_item_focused),
	Holder ! refresh,
    ListCtrl.

dump_to_file(Parent, FileName, Holder) ->
    case file:open(FileName, [write]) of
	{ok, Fd} ->
	    %% Holder closes the file when it's done
	    Holder ! {dump, Fd};
	{error, Reason} ->
	    FailMsg = file:format_error(Reason),
	    MD = wxMessageDialog:new(Parent, FailMsg),
	    wxDialog:showModal(MD),
	    wxDialog:destroy(MD)
    end.

start_objinfo(undefined, _Frame, Opened) ->
    Opened;
start_objinfo(Pid, Frame, Opened) ->
    case lists:member(Pid, Opened) of
	true ->
	    Opened;
	false ->
	    console_objinfo:start(Pid, Frame, self()),
	    [Pid | Opened]
    end.

start_attrinfo(undefined, _Frame, Opened) ->
    Opened;
start_attrinfo(Pid, Frame, Opened) ->
    case lists:member(Pid, Opened) of
	true ->
	    Opened;
	false ->
	    console_attrinfo:start(Pid, Frame, self()),
	    [Pid | Opened]
    end.


call(Holder, What) ->
    Ref = erlang:monitor(process, Holder),
    Holder ! What,
    receive
	{'DOWN', Ref, _, _, _} -> "";
	{Holder, Res} ->
	    erlang:demonitor(Ref),
	    Res
    after 2000 ->
	    io:format("Hanging call ~p~n",[What]),
	    ""
    end.

%%%%%%%%%%%%%%%%%%%%%%% Callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({holder_updated, Count}, State0=#state{grid=Grid}) ->
    State = update_selection(State0),

    wxListCtrl:setItemCount(Grid, Count),
    wxListCtrl:refreshItems(Grid, 0, Count-1),

    {noreply, State};

handle_info(refresh_interval, #state{holder=Holder}=State) ->
    Holder ! refresh,
    {noreply, State};

handle_info({procinfo_menu_closed, Pid},
	    #state{procinfo_menu_pids=Opened}=State) ->
    NewPids = lists:delete(Pid, Opened),
    {noreply, State#state{procinfo_menu_pids=NewPids}};

handle_info({active, Node},
	    #state{holder=Holder, timer=Timer, parent=Parent}=State) ->
    create_pro_menu(Parent, Holder),
    Holder ! {change_node, Node},
    {noreply, State#state{timer=console_lib:start_timer(Timer)}};

handle_info(not_active, #state{timer=Timer0}=State) ->
    Timer = console_lib:stop_timer(Timer0),
    {noreply, State#state{timer=Timer}};

handle_info(Info, State) ->
    io:format("~p:~p, Unexpected info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

terminate(_Reason, #state{holder=Holder}) ->
    Holder ! stop,
    etop:stop(),
    ok.

code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.


handle_call(Msg, _From, State) ->
    io:format("~p:~p: Unhandled call ~p~n",[?MODULE, ?LINE, Msg]),
    {reply, ok, State}.


handle_cast(Msg, State) ->
    io:format("~p:~p: Unhandled cast ~p~n", [?MODULE, ?LINE, Msg]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%LOOP%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_event(#wx{id=?ID_DUMP_TO_FILE}, #state{panel=Panel, holder=Holder}=State) ->
    FD  =  wxFileDialog:new(Panel,
			    [{style,?wxFD_SAVE bor ?wxFD_OVERWRITE_PROMPT}]),
    case wxFileDialog:showModal(FD) of
	?wxID_OK ->
	    Path = wxFileDialog:getPath(FD),
	    wxDialog:destroy(FD),
	    dump_to_file(Panel, Path, Holder);
	_ ->
	    wxDialog:destroy(FD)
    end,
    {noreply, State};

handle_event(#wx{id=?ID_ACCUMULATE,
		 event=#wxCommand{type=command_menu_selected, commandInt=CmdInt}},
	     #state{holder=Holder}=State) ->
    Holder ! {accum, CmdInt =:= 1},
    {noreply, State};

handle_event(#wx{id=?ID_REFRESH, event=#wxCommand{type=command_menu_selected}},
	     #state{holder=Holder}=State) ->
    Holder ! refresh,
    {noreply, State};

handle_event(#wx{id=?ID_REFRESH_INTERVAL},
	     #state{panel=Panel, timer=Timer0}=State) ->
    Timer = console_lib:interval_dialog(Panel, Timer0, 1, 5*60),
    {noreply, State#state{timer=Timer}};

handle_event(#wx{id=?ID_KILL}, #state{holder=Holder,sel={[_|Ids], [ToKill|Pids]}}=State) ->
%% 	io:format("Ids:~w, ToKill:~w, Pids:~w~n_", [Ids,ToKill,Pids]),
%%  add multiple selections support 
	object:delete(object:get_by_executor(ToKill)),	
    Holder ! refresh,
    {noreply, State#state{sel={Ids,Pids}}};


handle_event(#wx{id=?ID_PROC},
	     #state{panel=Panel, sel={_, [Pid|_]},procinfo_menu_pids=Opened}=State) ->
    Opened2 = start_objinfo(Pid, Panel, Opened),
    {noreply, State#state{procinfo_menu_pids=Opened2}};

handle_event(#wx{id=?ID_ATTRIBUTE},
	     #state{panel=Panel, sel={_, [Pid|_]},procinfo_menu_pids=Opened}=State) ->
	Object = object:get_by_executor(Pid),
    Opened2 = start_attrinfo(object:property_server_of(Object), Panel, Opened),
    {noreply, State#state{procinfo_menu_pids=Opened2}};

handle_event(#wx{id=?ID_TRACE_PIDS}, #state{sel={_, Pids}, panel=Panel}=State)  ->
    case Pids of
	[] ->
	    object_console:create_txt_dialog(Panel, "No selected processes", "Tracer", ?wxICON_EXCLAMATION),
	    {noreply, State};
	Pids ->
	    observer_trace_wx:add_processes(observer_wx:get_tracer(), Pids),
	    {noreply,  State}
    end;

handle_event(#wx{id=?ID_TRACE_NAMES}, #state{sel={SelIds,_Pids}, holder=Holder, panel=Panel}=State)  ->
    case SelIds of
	[] ->
	    object_console:create_txt_dialog(Panel, "No selected processes", "Tracer", ?wxICON_EXCLAMATION),
	    {noreply, State};
	_ ->
	    PidsOrReg = call(Holder, {get_name_or_pid, self(), SelIds}),
	    observer_trace_wx:add_processes(observer_wx:get_tracer(), PidsOrReg),
	    {noreply,  State}
    end;

handle_event(#wx{id=?ID_TRACE_NEW, event=#wxCommand{type=command_menu_selected}}, State) ->
    observer_trace_wx:add_processes(observer_wx:get_tracer(), [new]),
    {noreply,  State};

handle_event(#wx{event=#wxSize{size={W,_}}},
	     #state{grid=Grid}=State) ->
    console_lib:set_listctrl_col_size(Grid, W),
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_right_click,
			       itemIndex=Row}},
	     #state{panel=Panel, holder=Holder}=State) ->

    case call(Holder, {get_row, self(), Row, pid}) of
	{error, undefined} ->
	    undefined;
	{ok, _} ->
	    Menu = wxMenu:new(),
	    wxMenu:append(Menu, ?ID_PROC, "Object Info"),
	    wxMenu:append(Menu, ?ID_ATTRIBUTE, "Attributes Info"),
	    wxMenu:append(Menu, ?ID_KILL, "Delete Object"),
%% 	    wxMenu:append(Menu, ?ID_TRACE_PIDS, "Trace processes", [{help, ?TRACE_PIDS_STR}]),
%% 	    wxMenu:append(Menu, ?ID_TRACE_NAMES, "Trace named processes (all nodes)",
%% 			  [{help, ?TRACE_NAMES_STR}]),
	    wxWindow:popupMenu(Panel, Menu),
	    wxMenu:destroy(Menu)
    end,
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_focused,
			       itemIndex=Row}},
	     #state{grid=Grid,holder=Holder} = State) ->
    case Row >= 0 of
	true ->
	    SelIds = [Row|lists:delete(Row, get_selected_items(Grid))],
	    Pids = call(Holder, {get_pids, self(), SelIds}),
	    {noreply, State#state{sel={SelIds, Pids}}};
	false ->
	    {noreply, State}
    end;

handle_event(#wx{event=#wxList{type=command_list_col_click, col=Col}},
	     #state{holder=Holder}=State) ->
    Holder !  {change_sort, Col},
    {noreply, State};

handle_event(#wx{event=#wxList{type=command_list_item_activated}},
	     #state{panel=Panel, procinfo_menu_pids=Opened,
		    sel={_, [Pid|_]}}=State)
  when Pid =/= undefined ->
    Opened2 = start_objinfo(Pid, Panel, Opened),
    {noreply, State#state{procinfo_menu_pids=Opened2}};

handle_event(Event, State) ->
    io:format("~p:~p: handle event ~p\n", [?MODULE, ?LINE, Event]),
    {noreply, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

update_selection(State=#state{holder=Holder, grid=Grid,
			      sel={SelIds0, SelPids0}}) ->
    Sel = {SelIds,_SelPids} = call(Holder, {get_rows_from_pids, self(), SelPids0}),
    set_focus(SelIds0, SelIds, Grid),
    case SelIds =:= SelIds0 of
	true -> ok;
	false ->
	    wx:batch(fun() ->
			     [wxListCtrl:setItemState(Grid, I, 0, ?wxLIST_STATE_SELECTED) ||
				 I <- SelIds0],
			     [wxListCtrl:setItemState(Grid, I, 16#FFFF, ?wxLIST_STATE_SELECTED) ||
				 I <- SelIds]
		     end)
    end,
    %%io:format("Update ~p -> ~p~n",[{SelIds0, SelPids0}, Sel]),
    State#state{sel=Sel}.

get_selected_items(Grid) ->
    get_selected_items(Grid, -1, []).

get_selected_items(Grid, Index, ItemAcc) ->
    Item = wxListCtrl:getNextItem(Grid, Index, [{geometry, ?wxLIST_NEXT_ALL},
						{state, ?wxLIST_STATE_SELECTED}]),
    case Item of
	-1 ->
	    lists:reverse(ItemAcc);
	_ ->
	    get_selected_items(Grid, Item, [Item | ItemAcc])
    end.

set_focus([], [], _Grid) -> ok;
set_focus([Same|_], [Same|_], _Grid) -> ok;
set_focus([], [New|_], Grid) ->
    wxListCtrl:setItemState(Grid, New, 16#FFFF, ?wxLIST_STATE_FOCUSED);
set_focus([Old|_], [], Grid) ->
    wxListCtrl:setItemState(Grid, Old, 0, ?wxLIST_STATE_FOCUSED);
set_focus([Old|_], [New|_], Grid) ->
    wxListCtrl:setItemState(Grid, Old, 0, ?wxLIST_STATE_FOCUSED),
    wxListCtrl:setItemState(Grid, New, 16#FFFF, ?wxLIST_STATE_FOCUSED).
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%TABLE HOLDER%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_table_holder(Parent, Attrs) ->
    Backend = spawn_link(node(), console_backend,pool_collect,[self()]),
    table_holder(#holder{parent=Parent,
			 info=[],
			 node=node(),
			 backend_pid=Backend,
			 attrs=Attrs
			}).

table_holder(#holder{info=PoolList, attrs=Attrs,
		     node=Node, backend_pid=Backend}=S0) ->
    receive
	{get_row, From, Row, Col} ->
	    get_row(From, Row, Col, PoolList),
	    table_holder(S0);
	{get_attr, From, Row} ->
	    get_attr(From, Row, Attrs),
	    table_holder(S0);
	{Backend, PoolInfo} ->
	    State = handle_update(PoolList, S0),
	    table_holder(State#holder{backend_pid=undefined});
	refresh when is_pid(Backend)->
	    table_holder(S0); %% Already updating
	refresh ->
	    Pid = spawn_link(Node,console_backend,pool_collect,[self()]),
	    table_holder(S0#holder{backend_pid=Pid});
	{change_sort, Col} ->
	    State = change_sort(Col, S0),
	    table_holder(State);
	{get_pids, From, Indices} ->
	    get_pids(From, Indices, PoolList),
	    table_holder(S0);
	{get_rows_from_pids, From, Pids} ->
	    get_rows_from_pids(From, Pids, PoolList),
	    table_holder(S0);
	{get_name_or_pid, From, Indices} ->
	    get_name_or_pid(From, Indices, PoolList),
	    table_holder(S0);

	{get_node, From} ->
	    From ! {self(), Node},
	    table_holder(S0);
	{change_node, NewNode} ->
	    case Node == NewNode of
		true ->
		    table_holder(S0);
		false ->
		    self() ! refresh,
		    table_holder(S0#holder{node=NewNode})
	    end;
	{accum, Bool} ->
	    table_holder(change_accum(Bool,S0));
	{get_accum, From} ->
	    From ! {self(), S0#holder.accum == true},
	    table_holder(S0);
	{dump, Fd} ->
	    etop_txt:do_update(Fd, S0#holder.info, #opts{node=Node}),
	    file:close(Fd),
	    table_holder(S0);
	stop ->
	    ok;
	What ->
	    io:format("Table holder got ~p~n",[What]),
	    table_holder(S0)
    end.

change_sort(Col, S0=#holder{parent=Parent, info=EI=#etop_info{procinfo=Data}, sort=Sort0}) ->
    {Sort, PoolInfo}=sort(Col, Sort0, Data),
    Parent ! {holder_updated, length(Data)},
    S0#holder{info=EI#etop_info{procinfo=PoolInfo}, sort=Sort}.

change_accum(true, S0) ->
    S0#holder{accum=true};
change_accum(false, S0=#holder{info=#etop_info{procinfo=Info}}) ->
    self() ! refresh,
    S0#holder{accum=lists:sort(Info)}.

handle_update(PoolList,
	      S0=#holder{parent=Parent, sort=Sort=#sort{sort_key=KeyField}}) ->
%%     {Name1, S1} = accum(Name0, S0),
%%     {_SO, PoolInfo} = sort(KeyField, Sort#sort{sort_key=undefined}, Name1),
    Parent ! {holder_updated, length(PoolList)},
    S1#holder{info=PoolInfo}}.

accum(PoolInfo, State=#holder{accum=true}) ->
    {PoolInfo, State};
accum(PoolInfo0, State=#holder{accum=Previous}) ->
    PoolInfo = lists:sort(PoolInfo0),
    {accum2(PoolInfo,Previous,[]), State#holder{accum=PoolInfo}}.

accum2([PI=#etop_proc_info{pid=Pid, class=Class, runtime=RT}|PIs],
       [#etop_proc_info{pid=Pid, class=Old, runtime=OldRT}|Old], Acc) ->
    accum2(PIs, Old, [PI#etop_proc_info{runtime=RT-OldRT}|Acc]);
accum2(PIs=[#etop_proc_info{pid=Pid}|_], [#etop_proc_info{pid=OldPid}|Old], Acc)
  when Pid > OldPid ->
    accum2(PIs, Old, Acc);
accum2([PI|PIs], Old, Acc) ->
    accum2(PIs, Old, [PI|Acc]);
accum2([], _, Acc) -> Acc.

sort(Col, Opt=#sort{sort_key=Col, sort_incr=Bool}, Table) ->
    {Opt#sort{sort_incr=not Bool}, lists:reverse(Table)};
sort(Col, S=#sort{sort_incr=true}, Table) ->
    {S#sort{sort_key=Col}, lists:keysort(col_to_element(Col), Table)};
sort(Col, S=#sort{sort_incr=false}, Table) ->
    {S#sort{sort_key=Col}, lists:reverse(lists:keysort(col_to_element(Col), Table))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_poolinfo_data(Col, Info) ->
    element(col_to_element(Col), Info).

col_to_element(name) -> #pool_info.name;
col_to_element(counter)  -> #pool_info.counter;
col_to_element(max) -> #pool_info.max;
col_to_element(queue)  -> #pool_info.queue.

get_pids(From, Indices, PoolInfo) ->
    Processes = [(lists:nth(I+1, PoolInfo))#etop_proc_info.pid || I <- Indices],
    From ! {self(), Processes}.

get_name_or_pid(From, Indices, PoolInfo) ->
    Get = fun(#etop_proc_info{name=Name}) when is_atom(Name) -> Name;
	     (#etop_proc_info{pid=Pid}) -> Pid
	  end,
    Processes = [Get(lists:nth(I+1, PoolInfo)) || I <- Indices],
    From ! {self(), Processes}.


get_row(From, Row, pid, Info) ->
    Pid = case Row =:= -1 of
	      true ->  {error, undefined};
	      false -> {ok, get_poolinfo_data(?COL_PID, lists:nth(Row+1, Info))}
	  end,
    From ! {self(), Pid};
get_row(From, Row, Col, PoolList) ->
    Data = case Row+1 > length(PoolList) of
	       true ->
		   "";
	       false ->
		   PoolInfo = lists:nth(Row+1, PoolList),
		   get_poolinfo_data(Col, PoolInfo)
	   end,
    From ! {self(), console_lib:to_str(Data)}.

get_rows_from_pids(From, Pids0, Info) ->
    Res = lists:foldl(fun(Pid, Data = {Ids, Pids}) ->
			      case index(Pid, Info, 0) of
				  false -> Data;
				  Index -> {[Index|Ids], [Pid|Pids]}
			      end
		      end, {[],[]}, Pids0),
    From ! {self(), Res}.

get_attr(From, Row, Attrs) ->
    Attribute = case Row rem 2 =:= 0 of
		    true ->  Attrs#attrs.even;
		    false -> Attrs#attrs.odd
		end,
    From ! {self(), Attribute}.

index(Pid, [#etop_proc_info{pid=Pid}|_], Index) -> Index;
index(Pid, [_|PI], Index) -> index(Pid, PI, Index+1);
index(_, _, _) -> false.
