-module(keypid).

-export([init/0, save/2, get/1, delete/1]).
-define(TABLE_ID, ?MODULE).

init() ->
  io:fwrite("caching feature keypid initialized!~n"),
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

delete(Key) ->
  ets:delete(?TABLE_ID, Key).

save(Key, Pid) ->
  io:fwrite("Saving Key: ~p to PID: ~p~n~n", [Key, Pid]),
  save_helper(Key, Pid, is_pid(Pid)).

save_helper(Key, Pid, true) ->
  ets:insert(?TABLE_ID, {Key, Pid});
save_helper(_, _, _) ->
  false.

get(Key) ->
  io:fwrite("Looking up Key: ~p~n", [Key]),
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] ->
      case is_pid(Pid) andalso is_process_alive(Pid) of
        true ->
          io:fwrite("Pid found: ~p~n", [Pid]),
          Pid;
        false ->
          io:fwrite("Pid is DEAD??!~n"),
          not_found
      end;
    [] ->
      io:fwrite("Pid NOT found~n"),
      not_found
  end.