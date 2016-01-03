-module(command_handler).

-behavior(gen_event).

-export([add_handler/0, delete_handler/0]).
-export([init/1, handle_event/2, handle_call/2,
	handle_info/2, code_change/3, terminate/2]).

-include("blog_data_structures.hrl").

add_handler() ->
    command_bus:add_handler(?MODULE, []).

delete_handler() ->
    command_bus:delete_handler(?MODULE, []).

init([]) ->
	{ok, []}.

handle_event(#make_new_draft{id=Id}, State) ->
	case repository:get_by_id(Id) of
		not_found ->
			Pid = draft:new(),
			draft:make_new_draft(Pid, Id),
			repository:save(Pid),
			{ok, State};
		_ ->
			{ok, State}
	end;

handle_event(#refine_title_of_draft{id=Id,title=Title}, State) ->
	case repository:get_by_id(Id) of
		not_found ->
			handle_not_found(Id, State);
		{ok,Pid} ->
			io:fwrite("Id: ~s found!!\n", [Id]),
			draft:refine_title_of_draft(Pid, Title),
			repository:save(Pid),
			{ok, State}
	end;

handle_event(#refine_content_of_draft{id=Id,content=Content}, State) ->
	case repository:get_by_id(Id) of
		not_found ->
			handle_not_found(Id, State);
		{ok,Pid} ->
			io:fwrite("Id: ~s found!!\n", [Id]),
			draft:refine_content_of_draft(Pid, Content),
			repository:save(Pid),
			{ok, State}
	end;

handle_event(#publish_draft{id=Id}, State) ->
  case repository:get_by_id(Id) of
    not_found ->
      handle_not_found(Id, State);
    {ok,Pid} ->
      io:fwrite("Id: ~s found!!\n", [Id]),
      draft:publish_draft(Pid),
      repository:save(Pid),
      handle_event(#renew_draft{id=Id}, State),
      {ok, State}
  end;

handle_event(#unpublish_draft{id=Id}, State) ->
  case repository:get_by_id(Id) of
    not_found ->
      handle_not_found(Id, State);
    {ok,Pid} ->
      io:fwrite("Id: ~s found!!\n", [Id]),
      draft:unpublish_draft(Pid),
      repository:save(Pid),
      {ok, State}
  end;

handle_event(#renew_draft{id=Id}, State) ->
  case repository:get_by_id(Id) of
    not_found ->
      handle_not_found(Id, State);
    {ok,Pid} ->
      io:fwrite("Id: ~s found!!\n", [Id]),
      NewId = uuid:to_string(uuid:uuid4()),
      draft:renew_draft(Pid, NewId),
      repository:save(Pid),
      handle_event(#make_new_draft{id=NewId}, State),
      {ok, State}
  end;

handle_event(#hide_draft{id=Id}, State) ->
  case repository:get_by_id(Id) of
    not_found ->
      handle_not_found(Id, State);
    {ok,Pid} ->
      io:fwrite("Id: ~s found!!\n", [Id]),
      draft:hide_draft(Pid),
      repository:save(Pid),
      {ok, State}
  end;

handle_event(_, State) ->
	{ok, State}.

handle_not_found(Id, State) ->
	io:fwrite("Id: ~s not found!!\n", [Id]),
	{ok, State}.

handle_call(_, State) ->
	{ok, State}.

handle_info(_, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.