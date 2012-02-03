%% 
%% @doc property object module
%% @version{1.0}
%% @copyright 2012 dragonflow.com

-module(property).
-compile(export_all).
-include("monitor_template.hrl").

extends() -> nil .

property(Self,Properties) ->
	object:set(Self, 'name', Properties#property_table.name),
 	object:set(Self, 'title',Properties#property_table.title),
	object:set(Self, 'type', Properties#property_table.type), %%Property type (scalar,frequency,server,text,bool,numeric,schedule,check_group,password,hidden)
 	object:set(Self, 'description', Properties#property_table.description),
	object:set(Self, 'order', Properties#property_table.order),
 	object:set(Self, 'editable', Properties#property_table.editable),
	object:set(Self, 'configurable', Properties#property_table.configurable),
 	object:set(Self, 'advance', Properties#property_table.advance),
	object:set(Self, 'state', Properties#property_table.state),
 	object:set(Self, 'primarystate', Properties#property_table.primarystate),
	object:set(Self, 'optional', Properties#property_table.optional), %%Property type (scalar,frequency,server,text,bool,numeric,schedule,check_group,password,hidden)
 	object:set(Self, 'default', Properties#property_table.default),
	object:set(Self, 'allowother', Properties#property_table.allowother),%%Whether to allow scalar attribute with the other input box
 	object:set(Self, 'listSize', Properties#property_table.listSize),
	object:set(Self, 'multiple', Properties#property_table.multiple),
 	object:set(Self, 'value', Properties#property_table.value).

test() ->
	P = #property_table{name=hostname,title="Host Name",type=text,order=1,description="Monitoring of the host name"},
	X = object:new(?MODULE,[P]),
	io:format("~w~n_", [object:getAttributes(X)]),
 	X.	 