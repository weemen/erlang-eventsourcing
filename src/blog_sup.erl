-module(blog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupervisorFlags = {one_for_one, 5, 10},
    BlogWorker          = {blog, {blog, start, [[]]}, permanent, 2000, worker, []},
    CommandBusWorker    = ?CHILD(command_bus, worker),
    ProjectionRefinementsBlogitem = ?CHILD(projection_refinements_per_blogitem, worker),
    {ok, { SupervisorFlags, [CommandBusWorker, BlogWorker, ProjectionRefinementsBlogitem]} }.

