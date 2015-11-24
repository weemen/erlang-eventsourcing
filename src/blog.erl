-module(blog).

-behaviour(gen_server).

%% API
-export([start/1, make_new_draft/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  terminate/2]).

%-include
-include("blog_data_structures.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

start([]) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  io:fwrite("creating a blog\n"),
  {ok, #state{}}.

make_new_draft() ->
  io:write("making a draft for our blog!\n"),
  gen_server:cast(?MODULE, {make_new_draft, uuid:to_string(uuid:uuid4())}),
  ok.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({make_new_draft, Id}, State) ->
  command_bus:send_command(#make_new_draft{id = Id}),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:fwrite("Terminating blog\n"),
  ok.

%rename_title() ->
%    %rename title of draft
%
%version_draft() ->
%    %creates a new version of a draft
%
%publish_draft() ->
%    %publish stuff and make copy current draft
%
%unpublish_draft() ->
%    %mark draft as unpublished
%
%remove_draft() ->
%    %hide a draft