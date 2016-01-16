-module(event_handler).

-behavior(gen_event).

-include("blog_data_structures.hrl").

-export([add_handler/0, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2,
  handle_info/2, code_change/3, terminate/2]).


%%new_draft_made
%%title_of_draft_refined
%%content_of_draft_refined
%%draft_published
%%draft_renewed

add_handler() ->
  command_bus:add_handler(?MODULE, []).

delete_handler() ->
  command_bus:delete_handler(?MODULE, []).

init([]) ->
  {ok, []}.


handle_event({EventName, Event}, State) when EventName == "new_draft_made" ->
  io:fwrite('Event handler: ~p~n',[EventName]),
  projection_refinements_per_blogitem:process_event({EventName, Event}),
  {ok, State};

handle_event({EventName, Event}, State)
  when
    EventName == "title_of_draft_refined";
    EventName == "content_of_draft_refined" ->
  io:fwrite('Event handler: ~p~n',[EventName]),
  projection_refinements_per_blogitem:process_event({EventName, Event}),
  {ok, State};

handle_event({_,_}, State) ->
  io:fwrite('Event handler: oops~n',[]),
  {ok, State};

handle_event(_, State) ->
  io:fwrite('Event handler: ignore it, unknown format. Message probably not for me~n',[]),
  {ok, State}.

handle_call(_, State) ->
  {ok, State}.

handle_info(_, State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.