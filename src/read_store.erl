-module(read_store).
-author("weemen").

%% API
-export([init/0, setup_refinement_for_blogitem/1, update_refinement_for_blogitem/1]).

init() ->
  ok.

setup_refinement_for_blogitem(Uuid) ->
  Query = erlang:iolist_to_binary(
    io_lib:format("INSERT INTO refinements_per_blogitem (uuid, count) VALUES (~p,0)",
      [
        Uuid
      ])
  ),

  emysql:execute(erlang_es_blog, Query),
  ok.

update_refinement_for_blogitem(Uuid) ->
  CurrentCountQuery = erlang:iolist_to_binary(
    io_lib:format("SELECT count FROM refinements_per_blogitem WHERE uuid = ~p", [
      Uuid
    ])
  ),

  Result       = emysql:execute(erlang_es_blog, CurrentCountQuery),
  ResultAsJson = emysql_util:as_json(Result),
  Record       = maps:from_list(lists:nth(1, ResultAsJson)),
  {_,Count}    = maps:find(<<"count">>, Record),

  io:fwrite("Mysql: UPDATE refinements_per_blogitem SET count = ~p WHERE uuid = ~p~n", [(Count+1), Uuid]),

  Query = erlang:iolist_to_binary(
    io_lib:format("UPDATE refinements_per_blogitem SET count = ~p WHERE uuid = ~p",
      [
        (Count+1),
        Uuid
      ])
  ),
  emysql:execute(erlang_es_blog, Query),
  ok.
