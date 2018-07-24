-module(cache_server_app).
-author("Kalyta Bogdan").
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/api/cache_server", cache_server, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	Delete_interval = application:get_env(home_project_5, delete_interval,120),
	cache_server_sup:start_link([Delete_interval]).
	
stop(_State) ->
ok.
