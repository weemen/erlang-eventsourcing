-module(draft_command_handler).
-author("weemen").

%% API
-export([add_handler/0, delete_handler/0, init/1, handle_event/2, handle_call/2, handle_info/2]).

-include("commands.hrl").

add_handler() ->
  draft_command_bus:add_handler(?MODULE, []).

delete_handler() ->
  draft_command_bus:delete_handler(?MODULE, []).

init([]) ->
  {ok, []}.

%% handle_event(#write_new_draft{id=Id}, State) ->
%% handle_event(#delete_draft{id=Id}, State) ->
%% handle_event(#refine_draft{id=Id}, State) ->
%% handle_event(#publish_draft{id=Id}, State) ->
%% handle_event(#unpublish_draft{id=Id}, State) ->

handle_event(_, State) ->
    {ok, State}.

handle_call(_, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.