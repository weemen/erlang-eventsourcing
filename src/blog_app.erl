-module(blog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    event_store:init(),
%%    read_store:init(),
    keypid:init(),

    case blog_sup:start_link() of
        {ok, Pid} ->
            io:fwrite("supervisor started\n"),
            command_handler:add_handler(),
%%            event_handler:add_handler(),
            io:fwrite("handler added to command handler\n"),
            {ok, Pid};
        Other ->
            io:fwrite("Couldn't start supervisor\n"),
            {error, Other}
    end.

stop(_State) ->
    ok.
