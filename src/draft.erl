-module(draft).

-export([new/0, make_new_draft/2]).
-export([process_unsaved_changes/2, load_from_history/2]).

-record(state, {id, date_created, balance=0, changes=[]}).
-define(PROCESS_TIME_OUT, 45000).

-include("blog_data_structures.hrl").

%% API
new() ->
  spawn(fun() -> init() end).

make_new_draft(Pid, Id) ->
  Pid ! {attempt_command, {make_new_draft, Id}}.

process_unsaved_changes(Pid, Saver) ->
  Pid ! {process_unsaved_changes, Saver}.

load_from_history(Pid, Events) ->
  Pid ! {load_from_history, Events}.

%% Internals
init() ->
  State = #state{},
  loop(State).

loop(#state{id=Id}=State) ->
  %% error_logger:info_msg("Process ~p state:[~p]~n", [self(), State]),
  receive
    {apply_event, Event} ->
      NewState = apply_event(Event, State),
      loop(NewState);
    {attempt_command, Command} ->
      NewState = attempt_command(Command, State),
      loop(NewState);
    {process_unsaved_changes, Saver} ->
      Id = State#state.id,
      Saver(Id, lists:reverse(State#state.changes)),
      NewState = State#state{changes=[]},
      loop(NewState);
    {load_from_history, Events} ->
      NewState = apply_many_events(Events, #state{}),
      loop(NewState);
    Unknown ->
      error_logger:warning_msg("Received unknown message (~p)~n", [Unknown]),
      loop(State)
  after ?PROCESS_TIME_OUT ->
    repository:remove_from_cache(Id),
    exit(normal)
  end.

attempt_command({make_new_draft, Id}, State) ->
  Event = #new_draft_made{id=Id, date_created=erlang:localtime()},
  apply_new_event( Event, State);

attempt_command(Command, State) ->
  error_logger:warn_msg("attempt_command for unexpected command (~p)~n", [Command]),
  State.

apply_new_event(Event, State) ->
  NewState = apply_event(Event, State),
  CombinedChanges = [Event] ++ NewState#state.changes,
  NewState#state{changes=CombinedChanges}.

apply_event(#new_draft_made{id=Id,date_created=DateCreated}, State) ->
  repository:add_to_cache(Id),
  State#state{id=Id, date_created=DateCreated};

apply_event(_Event, State)->
  State. % For some events, we don't have state to mutate

apply_many_events([], State) ->
  State;

apply_many_events([Event|Rest], State) ->
  NewState = apply_event(Event, State),
  apply_many_events(Rest, NewState).