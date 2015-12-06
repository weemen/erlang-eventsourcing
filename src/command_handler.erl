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
			io:fwrite("Id: ~s not found!!\n", [Id]),
			{ok, State};
		{ok,Pid} ->
			io:fwrite("Id: ~s found!!\n", [Id]),
			draft:refine_title_of_draft(Pid, Title),
			repository:save(Pid),
			{ok, State}
	end;

handle_event(_, State) ->
	{ok, State}.

handle_call(_, State) ->
	{ok, State}.

handle_info(_, State) ->
	{ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.