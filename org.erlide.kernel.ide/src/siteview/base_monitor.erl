-module (base_monitor).
-compile(export_all).
-include("../../include/object.hrl").
-include("../../include/monitor.hrl").
-include("../../include/monitor_template.hrl").

%
% monitor life cycle management:
%% start, waiting, waiting-for-resource, updating, logging,waiting
%% waiting, disable, waiting
%% waiting, disable
%
extends () -> nil .

?ACTION(start) -> [{wakeup_event,init_action}];
?ACTION(disabled) -> [{timed_enable_event,enable},{enable_event, enable_action}];
?ACTION(logging) -> {logging_event, logging_action};
?ACTION(waiting) -> [
					{disable_event,disable_action},
					{refresh_event,request_refresh_resource_action},
					{frequency_event,request_resource_action}
					];
?ACTION(waiting_for_resource) -> [{resource_allocated_event,update_action}].

?EVENT(wakeup_event)-> {eresye,wakeup};
?EVENT(disable_event)-> {eresye,disable_pattern};
?EVENT(enable_event)-> {eresye,enable};
?EVENT(frequency_event) -> {timeout,?FREQUENCY};
?EVENT(resource_allocated_event)-> {eresye,resource_allocated_pattern};
?EVENT(logging_event)-> {eresye,logging_pattern};
?EVENT(refresh_event)-> {eresye,logging_pattern};
?EVENT(timed_enable_event) -> {timeout,disable_time}.

?PATTERN(resource_allocated_pattern)-> {resource_pool, get, {?VALUE(name),resource_allocated}}; 
?PATTERN(wakeup)-> {?VALUE(name), get, {wakeup}};
?PATTERN(logging_pattern)-> {?VALUE(name), get, {logging}};
?PATTERN(refresh_pattern)-> {?VALUE(name), get, {refresh}};
?PATTERN(enable)-> [{?VALUE(name), get, {enable}},{?VALUE(name), get, {enable,fun(Time)-> Time >= 0 end}}];
?PATTERN(disable_pattern)-> [{?VALUE(name), get, {disable}},{?VALUE(name), get, {disable,fun(Time)-> Time >= 0 end}}];
?PATTERN(?FREQUENCY ) -> ?VALUE(?FREQUENCY)*1000;
?PATTERN(disable_time) -> ?VALUE(disable_time)*10000.

init_action(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=init,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,waiting).

update_action(Self,EventType,Pattern,State) ->
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

disable_action(Self,EventType,Pattern,State) ->
	io:format ( "Action: disable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,disabled).

request_resource_action(Self,EventType,Pattern,State) ->
	io:format ( "Action: request_resource, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	eresye:assert(resource_pool,{?VALUE(name),request_resource}), %%trigging the request_resource_pattern in resource_pool module
	object:do(Self,waiting_for_resource).

request_refresh_resource_action(Self,EventType,Pattern,State) ->
	io:format ( "Action: request_refresh_resource, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	eresye:assert(resource_pool,{?VALUE(name),request_refresh_resource}), %%trigging the request_refresh_resource_pattern in resource_pool module
	object:do(Self,waiting_for_resource).

enable_action(Self,EventType,Pattern,State) ->
	io:format ( "Action: enable, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	object:do(Self,waiting).

post_updating(Self) -> eresye:assert(?VALUE(name), {logging}).

logging_action(Self,EventType,Pattern,State) -> 
	%TODO: logging data to database
	io:format ( "[~w:~w] Action: logging, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[?MODULE,?LINE,State,EventType,Pattern]),
	object:do(Self,waiting).

refresh(Self) ->
	eresye:assert(?VALUE(name), {refresh}).

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