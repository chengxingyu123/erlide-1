-module(relativepoint).
-compile(export_all).
-include("../../include/object.hrl").

extends () -> point .

%% action(Self,start) -> {wakeup_event,init}.

relativepoint ( Self , Name, X , Y , OffX , OffY ) ->
	object : super ( Self , [Name, X , Y]),
	object : set ( Self , 'offsetX ' , OffX ),
	object : set ( Self , 'offsetY ' , OffY ).

relativepoint ( Self , Name,X , Y) ->
 relativepoint ( Self , Name,X , Y , 0 , 0).

relativepoint ( Self , Name) ->
	object:super(Self, [Name]),
	object:set (Self, 'offsetX ', 5 ),
	object:set(Self, 'offsetY ', 5).

relativepoint_(Self)->eresye:stop(?VALUE(name)).

getx(Self) ->
 object:super(Self,getx) + ?VALUE(offsetX).

gety(Self) ->
	object:super(Self,gety) + ?VALUE(offsetY).

print1(Self)  ->
	io:format("printing in [~w:~w]~n",[?MODULE,?VALUE(name)]).

on_starting(Self) ->
	io:format("This [~w] ~w object is starting \n",[?VALUE(name),?MODULE]).

on_stopping(Self) ->
	io:format("This [~w] ~w object is stopping \n",[?VALUE(name),?MODULE]).

init1(Self,EventType,Pattern,State) ->
	io:format ( "[~w:~w]:Action=init,State=~w,Event=~w,Pattern=~w,State=~w\n",	[?MODULE,?VALUE(name),State,EventType,Pattern,State]),
	object:do(Self,start).


test(Name) -> 
	X = object:new(?MODULE,[Name,10,10]),
	object:start(X),
	eresye:assert(Name,{wakeup}),
	X.


