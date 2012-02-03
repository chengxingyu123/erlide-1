-module (base_monitor).
-compile(export_all).
-include("../../include/object.hrl").
-include("../../include/monitor.hrl").
-include("../../include/monitor_template.hrl").

extends () -> nil .

?ACTION(start) -> [{wakeup_event,init}];
?ACTION(disabled) -> [{timed_enable_event,enable},{enable_event, enable}];
%% ?ACTION(running) -> {triggering_event, logging};
?ACTION(waiting) -> [
%% 					 {disable_event,disable},
						{frequency_event,update}
					].

?EVENT(wakeup_event)-> {eresye,wakeup};
?EVENT(disable_event)-> {eresye,disable};
?EVENT(enable_event)-> {eresye,enable};
?EVENT(frequency_event) -> {timeout,frequency};
?EVENT(timed_enable_event) -> {timeout,disable_time}.

?PATTERN(wakeup)-> {?VALUE(name), get, {wakeup}};
?PATTERN(enable)-> [{?VALUE(name), get, {enable}},{?VALUE(name), get, {enable,fun(Time)-> Time >= 0 end}}];
?PATTERN(disable)-> [{?VALUE(name), get, {disable}},{?VALUE(name), get, {disable,fun(Time)-> Time >= 0 end}}];
?PATTERN(?FREQUENCY ) -> ?VALUE(?FREQUENCY)*1000;
?PATTERN(disable_time) -> ?VALUE(disable_time)*10000.

init(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=init,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,waiting).

update(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=update,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,start).

base_monitor (Self,Name) ->
	?SETVALUE(?NAME,monitor),
	?SETVALUE(?FREQUENCY,5),
	?SETVALUE(?LASTUPDATE,0),
	?SETVALUE(disable_time,0),
	?SETVALUE(?MEASUREMENTTIME,0),
	?SETVALUE(?DISABLED,false),
	?SETVALUE(?VERFIY_ERROR,true),
	?SETVALUE(?ERROR_FREQUENCY,60),
	?SETVALUE(?DEPENDS_ON,none),
	?SETVALUE(?DEPENDS_CONDITION,error),	
	?SETVALUE(name,Name),
	eresye:start(Name).

base_monitor_(Self)-> 
	eresye:stop(?VALUE(name)).

disable(Self,EventType,Pattern,State) ->
	io:format ( "Action: disable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,disabled).

enable(Self,EventType,Pattern,State) ->
	io:format ( "Action: enable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,waiting).

logging(Self) -> 
	%TODO: logging data to database
	object:do(Self,waiting).

on_starting(Self) ->
	io:format("This [~w] ~w object is starting \n",[?VALUE(name),?MODULE]).

on_stopping(Self) ->
	io:format("This [~w] ~w object is stopping \n",[?VALUE(name),?MODULE]).


test(Name) ->
	case object:get_by_name(Name) of
		[] -> 
				X = object:new(?MODULE,[Name]),
				object:start(X),
				eresye:assert(Name,{wakeup}),
				X;
		_ -> atom_to_list(Name) ++ " not available, choose a new name"
	end.