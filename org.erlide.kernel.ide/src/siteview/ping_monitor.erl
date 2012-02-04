-module (ping_monitor).
-compile(export_all).
-include("../../include/object.hrl").
-include("../../include/monitor.hrl").

extends () -> atomic_monitor .

?SUPERCLAUSE(action).
?SUPERCLAUSE(event).
?SUPERCLAUSE(pattern).

ping_monitor(Self, Name)->
	?SETVALUE(timeout,1000),
	?SETVALUE(size,4),	
	?SETVALUE(name,Name),	
	object:super(Self, [Name]).

ping_monitor_(Self)->eresye:stop(?VALUE(name)).


init(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=update,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,waiting).

get_max() -> 10.  %max number of this type of monitor can be run in parallel

do_pong(Self,EventType,Pattern,State) ->
	io:format ( "[~w]:Type=~w,Action=update,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
	object:do(Self,start).

update(Self,EventType,Pattern,State) ->
%% 	io:format ( "[~w]:Type=~w,Action=update,State=~w,Event=~w,Pattern=~w\n",	[?VALUE(name),?MODULE,State,EventType,Pattern]),
  	Start = erlang:now(),
	object:do(Self,running),
	io:format("[~w:~w] Running: Hostname:~w, Timeout:~w, Size:~w\n", [?MODULE,?LINE,?VALUE(name),?VALUE(timeout),?VALUE(size)]),
%% 	Cmd = "ping -n 4 -l " ++ object:get(Size,'value') ++ " -w " ++ object:get(Timeout,'value') ++ "  " ++ object:get(Hostname,'value'),

%% 	simulated random data

	?SETVALUE(round_trip_time,100 * random:uniform(10)),
	?SETVALUE(packetsgood,random:uniform(4)),
	
	Diff = timer:now_diff(erlang:now(), Start)/1000000,
	?SETVALUE(?MEASUREMENTTIME,Diff),
	?SETVALUE(?LASTUPDATE,erlang:now()),	
 	io:format("[~w:~w] finish in ~w ms, return: RoundTripTime:~w, PacketsGood:~w\n", [?MODULE,?LINE,Diff,?VALUE(round_trip_time),?VALUE(packetsgood)]),
	eresye:assert(resource_pool,{?VALUE(name),release_resource}), %%trigging the release_resource_pattern in resource_pool module
	object:super(Self, post_updating,[]),	
	object:do(Self,logging).

start(Name) ->
	X = object:new(ping_monitor,[Name]),
	object:start(X),
	eresye:assert(Name,{wakeup}),
	X.