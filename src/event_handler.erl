-module(event_handler).

-behavior(gen_event).

-include("blog_data_structures.hrl").

-export([add_handler/0, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2,
  handle_info/2, code_change/3, terminate/2]).

add_handler() ->
  command_bus:add_handler(?MODULE, []).

delete_handler() ->
  command_bus:delete_handler(?MODULE, []).

init([]) ->
  {ok, []}.

handle_event(_ = Event, State) ->
  %rabbitmq shit here
  {ok, State}.

handle_call(_, State) ->
  {ok, State}.

handle_info(_, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.