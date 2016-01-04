-module(draft).

-export([
  new/0,
  make_new_draft/2,
  refine_title_of_draft/2,
  refine_content_of_draft/2,
  publish_draft/1,
  unpublish_draft/1,
  renew_draft/2,
  hide_draft/1]).


-export([process_unsaved_changes/2, load_from_history/2]).

-record(state, {id, date_created, title, content, published, followUpId, hidden, changes=[]}).
-define(PROCESS_TIME_OUT, 45000).

-include("blog_data_structures.hrl").

%% API
new() ->
  spawn(
    fun() ->
      init(),
      process_flag(trap_exit, true)
    end
  ).

make_new_draft(Pid, Id) ->
  Pid ! {attempt_command, {make_new_draft, Id}}.

refine_title_of_draft(Pid, Title) ->
  Pid ! {attempt_command, {refine_title_of_draft, Title}}.

refine_content_of_draft(Pid, Content) ->
  Pid ! {attempt_command, {refine_content_of_draft, Content}}.

publish_draft(Pid) ->
  Pid ! {attempt_command, {publish_draft}}.

unpublish_draft(Pid) ->
  Pid ! {attempt_command, {unpublish_draft}}.

renew_draft(Pid, FollowUpId) ->
  Pid ! {attempt_command, {renew_draft, FollowUpId}}.

hide_draft(Pid) ->
  Pid ! {attempt_command, {hide_draft}}.

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

attempt_command({refine_content_of_draft, Content}, State) ->
  io:fwrite("attempting command: refine_content_of_draft !\n"),
  Id    = State#state.id,
  Event = #content_of_draft_refined{id=Id, content=Content},
  apply_new_event(Event, State);

attempt_command({publish_draft}, State) when State#state.published == false ->
  io:fwrite("attempting command: publish_draft !\n"),
  Id    = State#state.id,
  Event = #draft_published{id=Id},
  RenewEvent = #draft_renewed{id=Id, followUpId=uuid:to_string(uuid:uuid4())},
  apply_new_event(Event, State),
  apply_new_event(RenewEvent, State);

attempt_command({unpublish_draft}, State) when State#state.published == true ->
  io:fwrite("attempting command: unpublish_draft !\n"),
  Id    = State#state.id,
  Event = #draft_unpublished{id=Id},
  apply_new_event(Event, State);

attempt_command({renew_draft, FollowUpId}, State) ->
  io:fwrite("attempting command: renew_draft !\n"),
  Id    = State#state.id,
  Event = #draft_renewed{id=Id, followUpId=FollowUpId},
  apply_new_event(Event, State);

attempt_command({hide_draft}, State) when State#state.hidden == false ->
  io:fwrite("attempting command: hide_draft !\n"),
  Id    = State#state.id,
  Event = #draft_hidden{id=Id},
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
    #{"id"=>Id, "event_name"=>"new_draft_made", "event"=>#{<<"id">>=>list_to_binary(Id),<<"date_created">>=>list_to_binary(DateCreated), <<"published">>=>atom_to_binary(false, utf8), <<"hidden">>=>atom_to_binary(false, utf8)}}
  };

apply_event(#title_of_draft_refined{id=Id, title=Title}, State) ->
  io:fwrite("applying event: title_of_draft_refined !\n"),
  {
    State#state{id=Id, title=Title},
    #{"id"=>Id, "event_name"=>"title_of_draft_refined", "event"=>#{<<"id">>=>list_to_binary(Id),<<"title">>=>list_to_binary(Title)}}
  };

apply_event(#content_of_draft_refined{id=Id, content=Content}, State) ->
  io:fwrite("applying event: content_of_draft_refined !\n"),
  {
    State#state{id=Id, content=Content},
    #{"id"=>Id, "event_name"=>"content_of_draft_refined", "event"=>#{<<"id">>=>list_to_binary(Id),<<"content">>=>list_to_binary(Content)}}
  };

apply_event(#draft_published{id=Id}, State) ->
  io:fwrite("applying event: draft_published !\n"),
  {
    State#state{id=Id, published=Id},
    #{"id"=>Id, "event_name"=>"draft_published", "event"=>#{<<"id">>=>list_to_binary(Id), <<"published">>=>atom_to_binary(true, utf8)}}
  };

apply_event(#draft_unpublished{id=Id}, State) ->
  io:fwrite("applying event: draft_unpublished !\n"),
  {
    State#state{id=Id, published=Id},
    #{"id"=>Id, "event_name"=>"draft_unpublished", "event"=>#{<<"id">>=>list_to_binary(Id), <<"published">>=>atom_to_binary(false, utf8)}}
  };

apply_event(#draft_renewed{id=Id, followUpId=FollowUpId}, State) ->
  io:fwrite("applying event: draft_renewed!\n"),
  {
    State#state{id=Id, published=Id},
    #{"id"=>Id, "event_name"=>"draft_renewed", "event"=>#{<<"id">>=>list_to_binary(Id), <<"renewed">>=>atom_to_binary(true, utf8), <<"followUpId">>=>list_to_binary(FollowUpId)}}
  };

apply_event(#draft_hidden{id=Id}, State) ->
  io:fwrite("applying event: draft_hidden!\n"),
  {
    State#state{id=Id, published=Id},
    #{"id"=>Id, "event_name"=>"draft_hidden", "event"=>#{<<"id">>=>list_to_binary(Id), <<"hidden">>=>atom_to_binary(true, utf8)}}
  };

apply_event(_Event, State)->
  State. % For some events, we don't have state to mutate

apply_many_events([], State) ->
  State;

apply_many_events([EventAsMap|Rest], State) ->
  NewState = apply_event_properties([
    {#state.id, <<"id">>},
    {#state.title, <<"title">>},
    {#state.content, <<"content">>},
    {#state.date_created, <<"date_created">>},
    {#state.published, <<"published">>},
    {#state.followUpId, <<"followUpId">>},
    {#state.hidden, <<"hidden">>}
  ], State, EventAsMap),
  apply_many_events(Rest, NewState).

apply_event_properties([], State, _) ->
  State;

apply_event_properties([StateFieldMap|Rest], State, Map) ->
  {StateField,MapKey} = StateFieldMap,
  apply_event_properties(Rest, updating_state_from_applied_event(StateField, maps:find(MapKey, Map), State), Map).

updating_state_from_applied_event(StateField, {ok, PropertyOfEvent}, State) ->
  setelement(StateField, State, binary_to_list(PropertyOfEvent));

updating_state_from_applied_event(_, _, State) ->
  State.