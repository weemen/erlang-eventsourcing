-module(event_store).

%% Using ets for an event store is fine for a demo, but the data
%% will be discarded when the creating process dies, and there is no
%% automatic garbage collection for ets tables.
-include("blog_data_structures.hrl").

-export([init/0,get_events/1,append_events/1, delete/1]).
-define(TABLE_ID, ?MODULE).

init() ->
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

append_events(Events) ->

  NewEvents = lists:reverse(Events),
  lists:foreach(fun (EventRecord) ->

    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    DateTime = lists:flatten(
      io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Year, Month, Day, Hour, Min, Sec])
    ),

    Query = erlang:iolist_to_binary(
      io_lib:format("INSERT INTO events SET uuid = ~p, event = '~s', event_name = ~p, processed_date = ~p",
        [
          maps:get("id", EventRecord),
          jiffy:encode(maps:get("event", EventRecord)),
          maps:get("event_name", EventRecord),
          DateTime
        ])
    ),

    emysql:execute(
      erlang_es_blog,
      Query
    ),
    command_bus:publish_event(maps:get("event", EventRecord)) end, NewEvents
  ).

get_events(Key) ->
  lists:reverse(get_raw_events(Key)).

delete(Key) ->
  ets:delete(?TABLE_ID, Key).

get_raw_events(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Events}] -> Events;
    [] -> []
  end.