%%
%% @author SiteView
%% @copyright Dragonflow Networks, Inc.
%% @version 1.0.0
%% @title The resource pool manager
%% @doc The resource pool needs started during system initializing phase.  The resource pool module implement managing the limited system resource for different monitor type.  
%% Limiting the vary large parallel process by queue the resource request when exeeding the resourse limits.

%% when monitor request resource, the resource counter increased by one, if less than limits, assert the resource allocated
%% if exeed the limit, put it into a queue.  Once finishing monitoring, the resource counter decrease by one or taking a queue
%% item  
%% 
%% 
%% 
%% @end
%%
-module(resource_pool).
-compile(export_all).
-include("object.hrl").
-include_lib("eunit/include/eunit.hrl").

extends () -> nil .

?ACTION(start) -> [
				   {request_resource_event,request_resource_action},
				   {request_refresh_resource_event,request_refresh_resource_action},
				   {release_resource_event,release_resource_action}
				  ].

?EVENT(request_resource_event)-> {eresye,request_resource_pattern};
?EVENT(request_refresh_resource_event)-> {eresye,request_refresh_resource_pattern};
?EVENT(release_resource_event)-> {eresye,release_resource_pattern}.

?PATTERN(request_resource_pattern)-> {resource_pool, get, {'_',request_resource}};
?PATTERN(request_refresh_resource_pattern)-> {resource_pool, get, {'_',request_refresh_resource}};
?PATTERN(release_resource_pattern)-> {resource_pool, get, {'_',release_resource}}.

start()->
	Pool = object:new(resource_pool),
	object:start(Pool),
	Pool.

resource_pool(Self)->
	?SETVALUE(name,pool),
	eresye:start(resource_pool).

resource_pool_(Self)->eresye:stop(?VALUE(name)).

request_resource_action(Self,EventType,Pattern,State) ->
	io:format ( "[~w:~w]Action: request_resource_action, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[?MODULE,?LINE,State,EventType,Pattern]),
	{Name,_} = Pattern,
	ResourceType = object:getClass(Name),
	%% get resource capacity and current resource consumption
	%% TODO: this max can be set based on sysinfo and usage model, can be sophisticated resource allocation, need create a model
	%% based on OS info, erlang system and process info, number of each ResourceType and monitor frequency, empirical limits, queue history, to come up an optimal max for each resource type 
	Max = erlang:apply(ResourceType, get_max,[]),

	%% initialize if not an attribute
	IsAttribute = object:isAttribute(Self,ResourceType),
	if IsAttribute -> ok;
	   true -> ?SETVALUE(ResourceType,{0,queue:new()})
	end,
	
	{Counter,Queue} = ?VALUE(ResourceType),
	
	if Counter < Max -> 
		   ?SETVALUE(ResourceType,{Counter+1,Queue}),
		   eresye:assert(resource_pool, {Name,resource_allocated}); %trigginng the resource_allocated_pattern in each monitor (base_monitor)
	   true -> %reach the max, add into queue, will be allocated in release() once resourse released by others
		   ?SETVALUE(ResourceType,{Counter,queue:in(Name,Queue)})
	end,
	object:do(Self,start).

request_refresh_resource_action(Self,EventType,Pattern,State) ->
	io:format ( "Action: request_refresh_resource_action, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	{Name,_} = Pattern,
	ResourceType = object:getClass(Name),
	%% get resource capacity and current resource consumption
	%% TODO: this max can be set based on sysinfo and usage model, can be sophisticated resource allocation, need create a model
	%% based on OS info, number of each ResourceType and monitor frequency, empirical limits, queue history, to come up an optimal max for each resource type 
	Max = erlang:apply(ResourceType, get_max,[]),

	%% initialize if not an attribute
	IsAttribute = object:isAttribute(Self,ResourceType),
	if IsAttribute -> ok;
	   true -> ?SETVALUE(ResourceType,{0,queue:new()})
	end,
	
	{Counter,Queue} = ?VALUE(ResourceType),
	
	if Counter < Max -> 
		   ?SETVALUE(ResourceType,{Counter+1,Queue}),
		   eresye:assert(resource_pool, {Name,resource_allocated}); %trigginng the resource_allocated_pattern in each monitor (base_monitor)
	   true -> %reach the max, add into queue, will be allocated in release() once resourse released by others
		   ?SETVALUE(ResourceType,{Counter,queue:in_r(Name,Queue)})  %add the refresh request to the head for high priority, not the tail
	end,
	object:do(Self,start).


release_resource_action(Self,EventType,Pattern,State) ->
	io:format ( "Action: release_resource_action, [State]:~w, [Event type]:~w, [Pattern]: ~w '\n",	[State,EventType,Pattern]),
	{Name,_} = Pattern,
	ResourceType = object:getClass(Name),
	{Counter,Queue} = ?VALUE(ResourceType),
	Len = queue:len(Queue) ,
	if Len == 0 ->
		   ?SETVALUE(ResourceType,{Counter-1,Queue});
	   true -> %get the next item and drop it from the queue
		  eresye:assert(resource_pool, {queue:get(Queue),resource_allocated}),
		  ?SETVALUE(ResourceType,{Counter,queue:drop(Name,Queue)})
	end,
	io:format("[~w:~w] Resource pool ~w", [?MODULE,?LINE,?VALUE(ResourceType)]),
	object:do(Self,start).

%%TODO: check for error	
%% 	if Len == 0 andalso Counter > Max -> % this should never happen, serious error.
%% 		  io:format("[~w:~w] Resource pool SYSTEM ERROR. Queue empty and reach the max limit ~w", [?MODULE,?LINE,{Max,Counter}]);
%% 	   true -> ok
%% 	end,

%%TODO:stat info of the resource pool: number of pool, max, queue max/size/avg, create a monitor for it
%%TODO: create a monitors to monitor the health condition of pool

test() ->
	start().