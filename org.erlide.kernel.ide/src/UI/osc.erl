%% 
%% @doc object store console (mcc) module
%% @version{1.0}
%% @copyright 2012 dragonflow.com

-module(osc).
-compile(export_all).
-include("monitor_template.hrl").
-include_lib("wx/include/wx.hrl").

-export([start/0]).


start() ->
    object_console:start().