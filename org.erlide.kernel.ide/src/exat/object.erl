%%
%% @author SiteView
%% @copyright Dragonflow Networks, Inc.
%% @version 1.0.0
%% @title The rule-based object framework
%% @doc The object module implement a rule based object framework.   Each object is represented by two erlang processes: 
%% one for attribute access and one for method exection.  The executor pid is the object id.  The framework supports three main features:
%% 		1. attribute and method inheritance.  
%% 		2. integrate the rule engine ('mind'), can easily adding knowledge base (KB), e.g. if-then statement.
%% 		3. object store in the form of KB, allows flexible query
%% @end
%%
%%TODO: create monitors to monitor the health condition of object

-module(object).
-compile(export_all).
-include("object.hrl").

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Func: start_link/1
%% Arguments: none
%%====================================================================
start_link() ->
	eresye:start(object_store).
start()->start_link().

%% @spec new()->{ok,{parent,Parent}} | {error,Resean}
%% @doc Create a new object, will call the object constructor, the parameters must be a @type List 
%%
new(Class,P) ->
	io:format("[~w:~w]Class is ~w, Parameter is:~w~n", [?MODULE,?LINE,Class,P]),
    PropertyServerPid = spawn(object, property_server, [dict:new()]),
    server_call(PropertyServerPid,
                {self(), set, ?PROPERTY_STATUS, ?INIT_STATUS}),
    server_call(PropertyServerPid,
                {self(), set, ?TERMINATING, false}),
    server_call(PropertyServerPid,
                {self(), set, ?JOINING_SYNC, nil}),
    server_call(PropertyServerPid,
                {self(), set, ?MULTISYNC, nil}),
    server_call(PropertyServerPid,
                {self(), set, class, Class}),
    ExecutorPid = spawn(object, executor, []),
	
    server_call(PropertyServerPid,
                {self(), set, pid, ExecutorPid}),


	Object = #object {class=Class,
                      context=Class,
                      property_server = PropertyServerPid,
                      executor = ExecutorPid},
 	case Class of
		sync -> nil;
 		_ -> 	
			eresye:assert(object_store, {object,Object, pid,ExecutorPid}),
			eresye:assert(object_store, {object,Object, class,Class}),
			eresye:assert(object_store, {object,Object, state,?INIT_STATUS})
 	end,	
	
%%  must be here, after the facts are asserted 	
%% 	server_call(PropertyServerPid,
%%                 {self(), set, name, Name}),
	
    c_tor_call(Object, Class, P),

	%%the name is set in constructor
	if Class == 'sync' -> nil;
	   true -> eresye:assert(object_store, {object,Object,name,get(Object,name)})
	end,

    Object.
	
new(Class) ->
    new(Class,[]).

create(Class,P) -> 
	Object = new(Class,P),
	start(Object),
	get(Object,name).

delete(Name) when is_atom(Name) ->
	Object = get_by_name(Name),
	if
        length(Object)  == 0 -> name_not_exist;
        true -> delete(Object)
    end;
delete(Object) when is_record(Object,object) ->
    Class = Object#object.class,
%% 	io:format("Before d_tor ~w\n", [Object]),
    d_tor_call(Object, Class, []),
    S = get(Object, ?JOINING_SYNC),
%% 	io:format("Before d_tor ~w\n", [S]),
    if
        S =/= nil -> delete(S);
        true -> nil
    end,
    M = get(Object, ?MULTISYNC),
    if
        S =/= nil -> catch(multisync:abort(M));
        true -> nil
    end,
    %% Destroy the property server
%% 	io:format("After d_tor II ~w\n", [Object]),
    catch(exit(Object#object.property_server, kill)),
    %% Destroy the executor
%% 	io:format("After d_tor ~w\n", [Object]),
    catch(exit(Object#object.executor, kill)),
%% 	io:format("After d_tor executor ~w\n", [Object]),
	%%delete all related KB facts 	
	eresye:retract_match(object_store, {object,Object,'_','_'}),
%% 	io:format("After retract_match~w\n", [Object]),
	ok;
delete([Object|T]) -> delete(Object), delete(T);
delete([]) -> ok.

set(Name,AttributeName, AttributeValue) when is_atom(Name) -> set(get_by_name(Name),AttributeName, AttributeValue);
set(Object, AttributeName, AttributeValue) ->
    server_call(Object#object.property_server,
                {self(), set, AttributeName, AttributeValue}),
    AttributeValue.

get(Name,AttributeName) when is_atom(Name) -> get(get_by_name(Name),AttributeName);
get(Object, AttributeName) when is_record(Object,object)->
%% io:format("[~w]GET: ~w\n", [?LINE,[Object, AttributeName]]),
    V = server_call(Object#object.property_server,
                    {self(), get, AttributeName}),
    case V of
        {value, Value} -> Value;
        _ -> exit({undef, [attribute, Object, AttributeName]})
    end.

call(Object, Method) when is_atom(Object) -> call(get_by_name(Object), Method);  
call(Object, Method) when is_record(Object,object) ->
    call(Object, Method, []).

call(Object, Method,Params) when is_atom(Object) -> call(get_by_name(Object), Method,Params);  
call(Object, Method, Params) when is_record(Object,object) ->
    %% only the virtual inheritance is implemented
%%     io:format("----- 1. Call/3 ~w,~w,~w\n", [Object,Method,Params]),
    Obj1 = Object#object {context = Object#object.class},
    X = call(Obj1, Method, Object#object.class, Params),
    %%io:format("Result is ~w,~w\n", [Object,X]),
    X.

call(Object, Method, nil, _) -> exit({undef, [method, Object, Method]});
call(Object, Method, Class, Params) ->
%%     io:format("----- 2. Call/4 ~w,~w,~w,~w\n", [Object,Method,Class,Params]),
    case public_call(Object, Method, Class, Params, fun(M,C) -> M end) of
        {error, Error} ->
            %%exit({undef, [method, Object, Method]});
            exit(Error);
        {ok, Value} -> Value
    end.


super_class(Object, Class) ->
    super_class(Object, Class, Class, []).

super_class(Object, Class, Method) ->
    super_class(Object, Class, Method, []).

super_class(Object, Class, Method, Params) ->
    Obj1 = Object#object {context = Class},
    call(Obj1, Method, Obj1#object.context, Params).


%% calls the constructor of the ancestor class
super(Object) ->
    super_class(Object, getParent(Object#object.context)).

%% calls the constructor of the ancestor class with the given arguments
super(Object, Arguments) when not is_list(Arguments) -> super(Object,[Arguments]);
super(Object, Arguments) when is_list(Arguments) ->
    Parent = getParent(Object#object.context),
%% 	io:format("super:~w,~w,~w~n", [Object,Parent,Arguments]),
    super_class(Object, Parent, Parent, Arguments);

%% calls the method of the ancestor class
super(Object, Method)  ->
    Parent = getParent(Object#object.context),
    super_class(Object, Parent, Method).

%% calls the method of the ancestor class with the given arguments
super(Object, Method, Arguments)  ->
    Parent = getParent(Object#object.context),
    super_class(Object, Parent, Method, Arguments).

%% calls the destructor of the ancestor class
super_ (Object) ->
    Parent = getParent(Object#object.context),
    Method = Parent ++ "_",
    super_class(Object, Parent, Method, []).


%%
%% behavioural procedures
%%
start(Object) when is_atom(Object)-> start(get_by_name(Object));
start(Object) when is_record(Object,object)->
    V = get(Object, ?PROPERTY_STATUS),
    if
        V == ?INIT_STATUS ->
            do(Object, start);
        true -> nil
    end,
    ok.

do(Object, State) when is_atom(Object) -> do(get_by_name(Object), State);
do(Object, State) when is_record(Object,object) ->
    set(Object, ?TERMINATING, false), %% FIXME: ?????
    V = get(Object, ?PROPERTY_STATUS),
    set(Object, ?PROPERTY_STATUS, State),

	case Object#object.class of
		sync -> nil;
 		_ -> 
			eresye:retract_match(object_store, {object,Object, state,'_'}),
			eresye:assert(object_store, {object,Object, state,State})
 	end,	

    if
        V == ?INIT_STATUS ->
            case catch(object:call(Object, on_starting)) of
                {'EXIT', {undef, _}} -> ok;
                {'EXIT', Error} ->
                    io:format("Error in 'on_starting' for object ~w: ~w\n",
                              [Object, Error]);
                _ -> ok
            end,
            set(Object, ?JOINING_SYNC, object:new(sync)),
            trigger_executor(Object);
        true -> nil
    end,
    ok.

stop(Object) ->
    set(Object, ?TERMINATING, true).

join(Object) ->
    S = get(Object, ?JOINING_SYNC),
    object:call(S, wait).

bind(Object, Agent) ->
    set(Object, ?BOUND_AGENT, Agent),
    set(Object, ?ACL_QUEUE, list_to_atom(atom_to_list(Agent) ++ "__queue")).

agentof(Object) ->
    get(Object, ?BOUND_AGENT).

aclqueueof(Object) ->
    get(Object, ?ACL_QUEUE).

%%
%%  get the executor of the object or a list of objects
%%  return the executor which is pid  
executorof(Object) when is_atom(Object) -> executorof(get_by_name(Object));
executorof(Object) when is_record(Object,object) ->
    Object#object.executor;
executorof(ObjectList) when is_list(ObjectList) ->
    	[X#object.executor||X <- ObjectList].

%%
%%  get the name of the object or a list of objects
%%  return the name or name list  
nameof(Object) when is_record(Object,object) ->
    get(Object,name);
nameof(ObjectList) when is_list(ObjectList) ->
	[get(X,name)||X <- ObjectList].

%%
%%  get the executor of the object or a list of objects
%%  return the executor which is pid  
property_server_of(Object) when is_atom(Object) -> property_server_of(get_by_name(Object));
property_server_of(Object) when is_record(Object,object) ->
    Object#object.property_server;
property_server_of(ObjectList) when is_list(ObjectList) ->
	[X#object.property_server||X <- ObjectList].

%%
%% reflection API
%%
getClass(Object) when is_atom(Object)->
	getClass(get_by_name(Object));
getClass(Object) when is_record(Object,object) ->
    Object#object.class.

getParent(Class) ->
    %% calls "extends/0" function to obtain the parent class
    %%io:format("Calling getParent ~w\n", [Class]),
    X = call_ (Class, extends),
    %%io:format("Called getParent ~w,~w\n", [Class, X]),
    X.


%%
%% get the attribute names of the object
%%
getAttributeNames(Object) when is_atom(Object) -> getAttributeNames(get_by_name(Object));
getAttributeNames(Object) when is_record(Object,object) ->
    lists:sort(server_call(Object#object.property_server, {self(), list})).

%%
%% get the attributes of the object
%%
isAttribute(Object,AttrName) when is_atom(Object) -> isAttribute(get_by_name(Object),AttrName);
isAttribute(Object,AttrName) when is_record(Object,object) ->
    lists:member(AttrName,server_call(Object#object.property_server, {self(), list})).

%%
%% get the attributes of the object
%%
getAttributes(Object) when is_atom(Object) -> getAttributes(get_by_name(Object));
getAttributes(Object) when is_record(Object,object) ->
    lists:sort(server_call(Object#object.property_server, {self(), list_values})).

%% ----------------------------------------------
%%
%%              INTERNAL ROUTINES
%%
%% ----------------------------------------------

%%
%% Calling a function of a module
%%
call_ (Module, Func) ->
    call_ (Module, Func, []).

call_ (Module, Func, P) ->
%% 	io:format("----- 8. call_/3 M:~w,F:~w,A:~w\n", [Module, Func, P]),
    apply({Module, Func}, P).

%%
%% Performing a virtual call to a method of an object
%%
public_call(Object, Method, Class, Params, NameFun) ->
%%     io:format("----- 3. public_call/5 ~w,~w,~w,~w,~w\n", [Object, Method, Class, Params, NameFun]),
	public_call(Object, Method, Class, Params, NameFun,
                {undef, [method, Object, Method]}).

public_call(Object, Method, nil, Params, NameFun, LastException) ->
    {error, LastException};

public_call(Object, Method, Class, Params, NameFun, LastException) ->
    Parents = tolist(getParent(Class)),
%% 	io:format("----- 4. public_call/6 Method:[~w:~w], Parents:[~w]~n" , [Class,Method,Parents]),
%%     io:format("----- 4. public_call/6 ~w,~w,~w,~w,~w,~w\n", [Object, Method, Class, Params, NameFun,LastException]),
    list_call(Object, Method, Class, Parents, Params, NameFun, LastException).

list_call(Object, Method, Class, [], Params, NameFun, LastException) ->
    {error, LastException}; %%{undef, [method, Object, Method]}};

list_call(Object, Method, Class, [ParentH | ParentT],
          Params, NameFun, LastException) ->
%% 	io:format("----- 5. list_call/7 Self:~w,Parent:~w,Method:~w~n",[Class,ParentH,Method]),
%% 	io:format("----- 5. list_call/7 ~w,~w,~w,~w,~w,~w,~w\n", [Object, Method, Class, [ParentH | ParentT],Params, NameFun, LastException]),
    {Result, Value} = deep_call(Object, Method, Class,
                                ParentH, Params, NameFun),
    if
        Result == ok -> {Result, Value};
        true -> list_call(Object, Method, Class, ParentT,
                          Params, NameFun, LastException)
    end.

deep_call(Object, Method, Class, Parent, Params, NameFun) ->
%% 	io:format("----- 6. deep_call/6 Object:~w, Method:~w, Class:~w, Parent:~w, Params:~w, NameFun:~w\n", [Object, Method, Class, Parent, Params, NameFun]),
    case call_a_method(Object, Method, Class, Params, NameFun) of
        {ok, Value} ->
            {ok, Value};
        {to_parent, Exception} ->
%% 			io:format("**************** Deep call to parent:~s, parent:~w:~w~n", [Exception,Parent,Method]),
%%             io:format("----- 6. deep_call/6 Object:~w, Method:~w, Class:~w, Parent:~w, Params:~w, NameFun:~w\n", [Object, Method, Class, Parent, Params, NameFun]),
    		Obj1 = Object#object {context = Parent},
            public_call(Obj1, Method, Parent, Params, NameFun, Exception);
        {error, Exception} ->
            exit(Exception)
    end.

call_a_method(Object, Method, Class, Params, NameFun) ->
%% 	io:format("----- 7. call_a_method/5 ~w,~w,~w,~w,~w\n", [Object, Method, Class, Params, NameFun]),
    MethodName = NameFun(Method, Class),  % switch between a regular method or constructor 

	%%test if MethodName is a function of the Class module
	IsMethod = lists:member({MethodName,erlang:length(Params)+1},get_methods(Class)) orelse %for user defined method
				lists:member(MethodName,[extends,Class,list_to_atom(atom_to_list(Class) ++ "_")]) ,  % for system methonds

	case IsMethod of
		true -> 
			case catch(call_(Class, MethodName, [Object | Params])) of
			    {'EXIT', Error = {undef, [ {Module, Function, _,_} | _]}} ->
			        MethodNotPresend = (Module == Class) and(Function == MethodName), 
			        check_for_returning_error(Error, MethodNotPresend);
			    {'EXIT', Error = {function_clause, [ {Module, Function, _} | _]}} ->
			        ClauseNotPresend = (Module == Class) and(Function == MethodName),
			        check_for_returning_error(Error, ClauseNotPresend);
			    {'EXIT', Other} ->
			        {error, Other};
			    Val ->
			        {ok, Val} 
			end;
		false -> 
%% 			io:format("**************** Method:~w, NOT in class:~s~n", [{MethodName,erlang:length(Params)+1},Class]),
			{to_parent, "No Method"}
	end.


check_for_returning_error(Error, true) ->%% the method is not present
	io:format("check_for_returning_error: true, ~w~n", [Error]),
    {to_parent, Error};
check_for_returning_error(Error, _) ->%% an internal exception occurred
	io:format("check_for_returning_error: false, ~w~n", [Error]),
    exit(Error).


%%
%% Performing a virtual call to the Constructor of an object
%%
c_tor_call(Object, Class, Params) ->
    case catch(public_call(Object, Class, Class, Params,
                           fun(M,C) -> C end)) of
        {'EXIT', {undef, _}} -> nil;
        {'EXIT', Other} -> exit(Other);
        _ -> nil
    end.

%% c_tor_call(Object, nil, Params) ->
%%   nil;
%% c_tor_call(Object, Class, Params) ->
%%   Parents = tolist(getParent(Class)),
%%   ctor_list_call(Object, Class, Parents, Params).

%% ctor_list_call(Object, nil, _, Params) ->
%%   nil;

%% ctor_list_call(Object, Class, [], Params) ->
%%   nil;

%% ctor_list_call(Object, Class, [ParentH | ParentT], Params) ->
%%   ctor_deep_call(Object, Class, ParentH, Params),
%%   ctor_list_call(Object, Class, ParentT, Params).

%% ctor_deep_call(Object, Class, Parent, Params) ->
%%   {Result, Value} = call_a_method(Object, method, Class, Params,
%%                                    fun(M,C) -> C end),
%%   if
%%     Result == error -> throw({bad_constructor, {Object, Class}});
%%     true -> nil
%%   end,
%%   Obj1 = Object#object {context = Parent},
%%   c_tor_call(Obj1, Parent, Params).


%%
%% Performing a virtual call to the Destructor of an object
%%
d_tor_call(Object, nil, Params) ->
    nil;
d_tor_call(Object, Class, Params) ->
    Parents = tolist(getParent(Class)),
    dtor_list_call(Object, Class, Parents, Params).

dtor_list_call(Object, nil, _, Params) ->
    nil;

dtor_list_call(Object, Class, [], Params) ->
    nil;

dtor_list_call(Object, Class, [ParentH | ParentT], Params) ->
    dtor_deep_call(Object, Class, ParentH, Params),
    dtor_list_call(Object, Class, ParentT, Params).

dtor_deep_call(Object, Class, Parent, Params) ->
    call_a_method(Object, method, Class, Params,
                  fun(M,C) ->
                          list_to_atom(atom_to_list(C) ++ "_")
                  end),
    Obj1 = Object#object {context = Parent},
    d_tor_call(Obj1, Parent, Params).


tolist(X) when is_atom(X) -> [X];
tolist(X) -> X.

%%
%% Execution Management
%%
trigger_executor(Object) ->
    Object#object.executor ! {go, Object}.

%%
%% Executor Server
%%
executor() ->
    receive
        {go, Object} -> executor_loop(Object);
        Other -> io:format("Executor[~w]: Invalid message ~w\n",
                           [self(), Other])
    end.

executor_loop(Object) ->
    %%io:format("Executor: ~w\n", [Object]),
    V = get(Object, ?PROPERTY_STATUS),
    T = not(get(Object, ?TERMINATING)),
%%     io:format("Value: ~w,~w\n", [Object, V]),
    if
        T  ->
            case catch(executor_do(Object, V)) of
                {'EXIT', Reason} ->
                    io:format("\n=ERROR REPORT====\nError in object behaviour ~w, state ~w, error value: ~w\n", [Object, V, Reason]),
                    set(Object, ?TERMINATING, true);
                Other -> nil
            end,
            executor_loop(Object);
        true ->
            case catch(object:call(Object, on_stopping)) of
                {'EXIT', {undef, _}} -> ok;
                {'EXIT', Error} ->
                    io:format("Error in 'on_stopping' for object ~w: ~w\n",
                              [Object, Error]);
                _ -> ok
            end,
			eresye:retract_match(object_store, {object,Object,'_','_'}),
            S = get(Object, ?JOINING_SYNC),
            if
                S =/= nil -> object:call(S, signal_all);
                true -> nil
            end
            %%io:format("Behaviour[~w]: Stopped.\n", [Object])
    end.

executor_do(Object, State) ->
%% 	io:format("[~w] Executor_Do: ~w,~w\n", [?LINE,Object, State]),
    Class = Object#object.class,
    %% get the event & proc list
    EventProc = call(Object, action, [State]),
    %% convert event-proc to a list(if not)
    if
        is_list(EventProc) -> EventProcList = EventProc;
        true -> EventProcList = [EventProc]
    end,
    CompleteEventList = make_event_list(Object, EventProcList),
    %%
    %% CompleteEventList is made of:
    %%   [ {EventType, Pattern, Proc}, .... ]
    %%
    %% now get the bound agent
    %%
    case catch(agentof(Object)) of
        {'EXIT', _ } -> Agent = ?NO_AGENT;
        Other -> Agent = Other
    end,
%% 	io:format("[~w] Event list = ~w\n", [?LINE,CompleteEventList]),
    M = multisync:new(),
    set(Object, ?MULTISYNC, M),
    make_multi_sync_tasks(M, Agent, CompleteEventList),
    {FiredEvent, PatternValue, Proc} = multisync:wait_one(M),
    set(Object, ?MULTISYNC, nil),
%% 	io:format("[~w] ~w,~w,~w\n", [?LINE,FiredEvent, PatternValue,Proc]),
    call(Object, Proc, [FiredEvent, PatternValue, State]).


make_event_list(Object, []) -> [];
make_event_list(Object, [{Event, Proc}|T]) ->
    {EventType, PatternName} = call(Object, event, [Event]),
    Pattern = getpattern(Object, PatternName),
    [ {EventType, Pattern, Proc} | make_event_list(Object, T)].

make_multi_sync_tasks(M, Agent, []) -> ok;
make_multi_sync_tasks(M, Agent, [{acl, Pattern, Proc}|T]) ->
    if
        Agent =/= ?NO_AGENT ->
            multisync:add_task(M, eventmanager,
                               eventmanager:eventof(acl),
                               [Agent, Pattern, Proc]);
        true -> nil
    end,
    make_multi_sync_tasks(M, Agent, T);
make_multi_sync_tasks(M, Agent, [{EventType, Pattern, Proc}|T]) ->
    multisync:add_task(M, eventmanager,
                       eventmanager:eventof(EventType),
                       [Agent, Pattern, Proc]),
    make_multi_sync_tasks(M, Agent, T).

getpattern(Object, $_) -> nil;
getpattern(Object, nil) -> nil;
getpattern(Object, PatternName) -> call(Object, pattern, [PatternName]).

%% get_matching_acl_message(AclQueue, Pattern) ->
%%   %Message = lq:dequeue(AclQueue),
%%   Message = agent:get_acl(AclQueue),
%%   %io:format("~w\n", [Message]),
%%   Match = match_lib:match(Pattern, Message),
%%   case Match of
%%     false -> get_matching_acl_message(AclQueue, Pattern);
%%     true -> Message
%%   end.

%%
%% Property Management Server
%%
server_call(Server, Data) ->
%% 	io:format("~w\n", [[Server, Data]]),
    Server ! Data,
    receive
        {ack, X} -> X;
        Other -> io:format("Invalid reply = ~p on call ~p: ~p\n",
                           [Server, Data, Other]),
                 exit({badtransaction, Other})
    end.

property_server(Dict) ->
    receive
        {From, get, AttributeName} ->
            case catch(dict:fetch(AttributeName, Dict)) of
                {'EXIT', _} -> From ! {ack, undef};
                Other -> From ! {ack, {value, Other}}
            end,
            property_server(Dict);
		{From, set, pid, ExecutorPid} ->
			From ! {ack, ok},
			property_server(dict:store(pid, ExecutorPid, Dict));
		{From, set, AttributeName, AttributeValue} ->
            From ! {ack, ok},
%% 			case dict:is_key(AttributeName, Dict)  of
%% 				true -> nil;
%% 				false -> % store the attribute definition into object store when set for the forst time 
%% 					case dict:is_key(pid, Dict) andalso dict:fetch(class, Dict) =/= 'sync' of						
%% 						true -> 
%% 								eresye:assert(object_store, {object,get_by_executor(dict:fetch(pid, Dict)),AttributeName,AttributeValue});
%% 					   	false -> nil 
%% 					end
%% 			end,
            property_server(dict:store(AttributeName, AttributeValue, Dict));
        {From, list} ->
            X = dict:fetch_keys(Dict),
            From ! {ack, X},
            property_server(Dict);
        {From, list_values} ->
            X = dict:to_list(Dict),
            From ! {ack, X},
            property_server(Dict);
        {From, exit } ->
            From ! {ack, ok};
        Other ->
            property_server(Dict)
    end.

%%
%% Object Store Manager:query, delete
%% TODO: seperate the following APIs into object_store module, has no reference to self

%%
%% get all objects in this store, return a list of objects
%%
get_all() ->
	[X||{object,X,_,_} <- 
			eresye:query_kb(object_store, {object,'_',pid,'_'})].

%%
%% get all objects in this store, return a sorted list of names
%%
get_all_name() ->
	lists:sort(
	  	[X||{object,_,name,X} <- eresye:query_kb(object_store, {object,'_',name,'_'})]
				).

get_size() ->
	length(get_all_name()).

%%
%% get objects by attribute name and value, e.g. Name = state, Value = [start,running,waiting]
%% return a list of objects
%%
get_by_attr(Name,Value) when is_atom(Value) orelse is_pid(Value) ->
	Obj = [X||{object,X,Name,Value} <- eresye:query_kb(object_store, {object,'_',Name,Value})];
get_by_attr(Name,Value) when is_list(Value) ->
	get_by_attr(Name,Value,[]).

get_by_attr(Name,[Value|T],Acc) -> 
	get_by_attr(Name,T,[(get_by_attr(Name,Value,Acc))|Acc]);
get_by_attr(Name,[],Acc) -> Acc.

jget_by_attr(Name,Value) ->
	[get(X,name)||X<-get_by_attr(Name,Value)].
%%
%% get objects by Class type, e.g. ClassName = ping_agent
%% return a list of objects
%% TODO: also show the child class objects
get_by_class(ClassName) ->
	get_by_attr(class,ClassName).

%%
%% get objects by executor, e.g. Pid 
%% return an objects or a list if the Pid is list
%%
get_by_executor(Pid) ->
	Obj = get_by_attr(pid,Pid),
	if length(Obj) == 1 -> [H|_] = Obj, H;
	   true -> Obj
	end.

%%
%% get objects by state, e.g. StateName = waiting or a list of states [waiting,running,start]
%% return a list of objects
%%
get_by_state(Value) ->
	get_by_attr(state,Value).

jget_by_state(Value) ->
	[get(X,name)||X<-get_by_attr(state,Value)].
  
%%
%% get objects by name, e.g. Name = ping1, the name is unique for each object
%% return a list of objects if Name is a list, or an object if Name is atom
%%  
get_by_name(Value) when is_atom(Value)->
	Obj = [X||{object,X,name,Value} <- eresye:query_kb(object_store, {object,'_',name,Value})],
	if length(Obj) == 1 -> [H|_] = Obj, H;
	   true -> Obj
	end;
get_by_name(Value) when is_list(Value) ->
	get_by_nameValue.

get_by_name([Value|T],Acc) -> 
	get_by_name(T,[get_by_name(Value,Acc)|Acc]);
get_by_name([],Acc) -> Acc.

%%
%% check if the name is available
%% return true or false
%%
check_name(Value) ->
	case get_by_name(Value) of
		[] -> available;
		_ -> not_available
	end.

%%
%% get the defined attributes, not shown the system attributes
%% return a proplists.
%%
get_defined_attrs(Object) when is_atom(Object) -> 
	get_defined_attrs(get_by_name(Object));
get_defined_attrs(Object) when is_record(Object,object) ->	
	getAttributes(Object) -- get_system_attrs(Object).


%%
%% get the system attributes, not shown the system attributes
%% return a proplists.
%%
get_system_attrs(Object) when is_atom(Object) -> get_system_attrs(get_by_name(Object));
get_system_attrs(Object) when is_record(Object,object) ->	
	lists:sort([{?TERMINATING,get(Object,?TERMINATING)},
	   {?JOINING_SYNC,get(Object,?JOINING_SYNC)},
	   {?MULTISYNC,get(Object,?MULTISYNC)},
	   {pid,get(Object,pid)},
	   {name,get(Object,name)},
	   {class,get(Object,class)},
	   {?PROPERTY_STATUS,get(Object,?PROPERTY_STATUS)}
	]).

%%
%% get the methods
%% return a list of methods.
%%
get_methods(Class) -> erlang:apply(Class, module_info, [exports]).


delete_all() -> 
	object:delete(object:get_all()),
	eresye:retract_match(object_store, {object,'_','_','_'}), 
	ok.