-module(errchat_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2]).
-export([stop/1]). 

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, _Args) ->  
    errchat_server_sup:start_link().
  
stop(_State) ->  
    ok.