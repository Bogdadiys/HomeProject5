-module(cache_server_sup).
-author("Kalyta Bogdan").
-behaviour(supervisor).

%% API.
-export([start_link/1]).

%% supervisor.
-export([init/1]).

% API
start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).

%% supervisor callbacks
init(Args) ->
	Worker = #{
		id       => cache_server_worker,
		start    => {cache_server_worker, start_link, Args}, 
		restart  => transient, 
		shutdown => brutal_kill, 
		type     => worker, 
		modules  => [cache_server_worker]
	},
{ok, {{one_for_one, 1000, 1}, [Worker]}}.
