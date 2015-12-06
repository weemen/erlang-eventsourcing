-module(draft).

-export([new/0, make_new_draft/2, refine_title_of_draft/2]).
-export([process_unsaved_changes/2, load_from_history/2]).

-record(state, {id, date_created, title, changes=[]}).
-define(PROCESS_TIME_OUT, 45000).

-include("blog_data_structures.hrl").

%% API
new() ->
  spawn(fun() -> init() end).

make_new_draft(Pid, Id) ->
  Pid ! {attempt_command, {make_new_draft, Id}}.

refine_title_of_draft(Pid, Title) ->
  io:fwrite("sending message to draft process!\n"),
  Pid ! {attempt_command, {refine_title_of_draft, Title}}.

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
  {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
  DateTime = lists:flatten(
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec])
  ),

  Event = #new_draft_made{id=Id, date_created=DateTime},
  apply_new_event(Event, State);

attempt_command({refine_title_of_draft, Title}, State) ->
  io:fwrite("attempting command: refine_title_of_draft !\n"),
  Id    = State#state.id,
  Event = #title_of_draft_refined{id=Id, title=Title},
  apply_new_event(Event, State);

attempt_command(Command, State) ->
  error_logger:warn_msg("attempt_command for unexpected command (~p)~n", [Command]),
  State.

apply_new_event(Event, State) ->
  EventState      = apply_event(Event, State),
  NewState        = element(1,EventState),
  EventRecord     = element(2,EventState),

  CombinedChanges = [EventRecord] ++ NewState#state.changes,
  NewState#state{changes=CombinedChanges}.

apply_event(#new_draft_made{id=Id,date_created=DateCreated}, State) ->
  repository:add_to_cache(Id),
  {
    State#state{id=Id, date_created=DateCreated},
    #{"id"=>Id, "event_name"=>"new_draft_made", "event"=>#{<<"id">>=>list_to_binary(Id),<<"date_created">>=>list_to_binary(DateCreated)}}
  };

apply_event(#title_of_draft_refined{id=Id, title=Title}, State) ->
  io:fwrite("applying event: title_of_draft_refined !\n"),
  {
    State#state{id=Id, title=Title},
    #{"id"=>Id, "event_name"=>"title_of_draft_refined", "event"=>#{<<"id">>=>list_to_binary(Id),<<"title">>=>list_to_binary(Title)}}
  };

apply_event(_Event, State)->
  State. % For some events, we don't have state to mutate

apply_many_events([], State) ->
  State;

%%TODO this code should be cleanend up to reduce redundency
apply_many_events([Event|Rest], State) ->
  MappedEventRecord = maps:from_list(Event),

  case maps:find(<<"event_name">>,MappedEventRecord) of
    {ok, <<"new_draft_made">>} ->
      {_,MappedEvent} = maps:find(<<"event">>, MappedEventRecord),
      {EventAsList}   = jiffy:decode(MappedEvent),
      EventAsMap      = maps:from_list(EventAsList),

      {_,Id}          = maps:find(<<"id">>,EventAsMap),
      {_,DateCreated} = maps:find(<<"date_created">>,EventAsMap),

      NewState = State#state{id=binary_to_list(Id), date_created=binary_to_list(DateCreated)};

    {ok, <<"title_of_draft_refined">>} ->
      {_,MappedEvent} = maps:find(<<"event">>, MappedEventRecord),
      {EventAsList}   = jiffy:decode(MappedEvent),
      EventAsMap      = maps:from_list(EventAsList) ,

      {_,Title}          = maps:find(<<"title">>,EventAsMap),
      NewState = State#state{title=binary_to_list(Title)}
  end,
  apply_many_events(Rest, NewState).