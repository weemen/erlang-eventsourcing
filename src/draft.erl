%%%-------------------------------------------------------------------
%%% @author weemen
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. okt 2015 11:55
%%%-------------------------------------------------------------------
-module(draft).
-author("weemen").

%% API
-export([open/0, close/0, write_page/1, delete_page/1, publish_page/1, unpublish_page/1])

-include("commands.hrl").

%open() -> application:start(draft).
%close() -> application:stop(draft).

%write_page(Draft) ->

%%delete_page(Draft) ->

%%publish_page(Draft) ->

%%unpublish_page(Draft) ->