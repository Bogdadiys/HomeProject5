{application, 'home_project_5', [
	{description, "web cache server project"},
	{vsn, "0.1.0"},
	{modules, ['cache_server','cache_server_app','cache_server_ets','cache_server_sup','cache_server_worker']},
	{registered, []},
	{applications, [kernel,stdlib,cowboy,jsx]},
	{mod, {cache_server_app, []}},
	{env, []}
]}.
