-module(book_command_handler).
-author("weemen").

%% API
-export([add_handler/0, delete_handler/0, init/1]).

add_handler() ->
  book_command_bus:add_handler(?MODULE, []).

delete_handler() ->
  book_command_bus:delete_handler(?MODULE, []).

init([]) ->
  {ok, []}.