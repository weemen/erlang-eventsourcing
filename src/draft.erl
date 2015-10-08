-module(book)

-export([open/0, close/0])

open() -> application:start(book)

close() -> application:stop(book)

write_page(Blog) ->

