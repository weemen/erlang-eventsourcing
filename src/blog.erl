-module(blog).

-behaviour(gen_server).

%% API
-export([
  start/1,
  make_new_draft/0,
  refine_title_of_draft/2,
  refine_content_of_draft/2,
  publish_draft/1,
  unpublish_draft/1,
  renew_draft/1,
  hide_draft/1
]).

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
  io:fwrite("making a draft for our blog!\n"),
  gen_server:cast(?MODULE, {make_new_draft, uuid:to_string(uuid:uuid4())}),
  ok.

refine_title_of_draft(Id, Title) ->
  io:fwrite("refining title of draft!\n"),
  gen_server:cast(?MODULE, {refine_title_of_draft, Id, Title}),
  ok.

refine_content_of_draft(Id, Content) ->
  io:fwrite("refining content of draft!\n"),
  gen_server:cast(?MODULE, {refine_content_of_draft, Id, Content}),
  ok.

publish_draft(Id) ->
  io:fwrite("publishing draft!\n"),
  gen_server:cast(?MODULE, {publish_draft, Id}),
  ok.

unpublish_draft(Id) ->
  io:fwrite("unpublishing draft!\n"),
  gen_server:cast(?MODULE, {unpublish_draft, Id}),
  ok.

renew_draft(Id) ->
  io:fwrite("renewing draft!\n"),
  gen_server:cast(?MODULE, {renew_draft, Id}),
  ok.

hide_draft(Id) ->
  io:fwrite("hiding draft!\n"),
  gen_server:cast(?MODULE, {hide_draft, Id}),
  ok.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({make_new_draft, Id}, State) ->
  command_bus:send_command(#make_new_draft{id = Id}),
  {noreply, State};

handle_cast({refine_title_of_draft, Id, Title}, State) ->
  command_bus:send_command(#refine_title_of_draft{id = Id, title=Title}),
  {noreply, State};

handle_cast({refine_content_of_draft, Id, Content}, State) ->
  command_bus:send_command(#refine_content_of_draft{id = Id, content=Content}),
  {noreply, State};

handle_cast({publish_draft, Id}, State) ->
  command_bus:send_command(#publish_draft{id = Id}),
  {noreply, State};

handle_cast({unpublish_draft, Id}, State) ->
  command_bus:send_command(#unpublish_draft{id = Id}),
  {noreply, State};

handle_cast({renew_draft, Id}, State) ->
  command_bus:send_command(#renew_draft{id = Id}),
  {noreply, State};

handle_cast({hide_draft, Id}, State) ->
  command_bus:send_command(#hide_draft{id = Id}),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:fwrite("Terminating blog\n"),
  ok.