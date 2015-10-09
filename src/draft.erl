-module(draft).

-export([open/0, close/0, write_page/1, delete_page/1, publish_page/1, unpublish_page/1])

-include("commands.hrl").

open() -> application:start(book).

close() -> application:stop(book).

write_page(Draft) ->

%%delete_page(Draft) ->

%%publish_page(Draft) ->

%%unpublish_page(Draft) ->