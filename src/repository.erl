-module(repository).

-export([get_by_id/1,save/1, add_to_cache/1, remove_from_cache/1]).

add_to_cache(Id) ->
  keypid:save(Id, self()).

remove_from_cache(Id) ->
  keypid:delete(Id).

get_by_id(Id) ->
  case keypid:get(Id) of
    not_found ->
      io:fwrite("process not found loading events from event store!~n"),
      load_from_event_store(Id);
    Pid ->
      io:fwrite("process found, sending back PID~n"),
      {ok, Pid}
  end.

save(Pid) ->
  Saver = fun(Id, Events) ->
    event_store:append_events(Events) end,
    draft:process_unsaved_changes(Pid, Saver).

load_from_event_store(Id) ->
  case event_store:get_events(Id) of
    [] ->
      not_found;
    Events ->
      io:fwrite("Events from event store loaded!~n"),
      Pid = draft:new(),
      keypid:save(Id,Pid),
      draft:load_from_history(Pid, Events),
      {ok, Pid}
  end.