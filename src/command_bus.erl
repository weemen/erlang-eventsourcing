-module(command_bus).

%% The EventManager for our app

-export([start_link/0, add_handler/2, delete_handler/2, send_command/1,
	publish_event/2]).

-define(SERVER, ?MODULE).

%% API functions
start_link() ->
    gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).

send_command(Command) ->
	gen_event:notify(?SERVER, Command).

publish_event(EventName, Event) ->
  io:fwrite('Publishing event: ~p with content: ~p~n',[EventName, Event]),
  gen_event:notify(?SERVER, {EventName, Event}).
