-module(cache_server).
-author("Kalyta Bogdan").

%%CowBoy Callbacks
-export([init/2]).

%%CowBoy Callbacks
init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	Length = cowboy_req:body_length(Req0),
	{ok,Body,_Req2} = cowboy_req:read_body(Req0,#{length => Length+20}),		
	Req = echo(Method, jsx:decode(Body), Req0),
	{ok, Req, Opts}.

%%API
echo(<<"POST">>, undefined, Req) ->
	cowboy_req:reply(400, #{}, <<"Missing echo parameter.">>, Req);
echo(<<"POST">>, [{<<"action">>,<<"insert">>},{<<"key">>,Key},{<<"value">>,Value}], Req) ->
	LiveTime = application:get_env(home_project_5, live_time,300),
	ok = cache_server_worker:insert(Key,Value,LiveTime),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, jsx:encode([{<<"result">>,<<"ok">>}]), Req);
echo(<<"POST">>, [{<<"action">>,<<"lookup">>},{<<"key">>,Key}], Req) ->
	{ok, Value} = cache_server_worker:lookup(Key),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, jsx:encode([{<<"result">>,Value}]), Req);
echo(<<"POST">>, [{<<"action">>,<<"lookup_by_date">>},{<<"date_from">>,DateF},{<<"date_to">>,DateT}], Req) ->
	DateFrom = date_convert(binary_to_list(DateF)),
	DateTo = date_convert(binary_to_list(DateT)),
	{ok, Value} = cache_server_worker:lookup_by_date(DateFrom,DateTo),
	cowboy_req:reply(200, #{
		<<"content-type">> => <<"text/plain; charset=utf-8">>
	}, jsx:encode([{<<"result">>,Value}]), Req);
echo(_, _, Req) ->
	%% Method not allowed.
cowboy_req:reply(405, Req).
%%API

date_convert(DateTime) ->
	[Date,Time] = string:split(DateTime," "),
	[YYYY,MM,DD] = [
		begin 
		{Int,_}=string:to_integer(Token), 
		Int 
		end || 
		Token<-string:tokens(Date,"/")
	],
	[Hh,Mm,Ss] = [
		begin 
		{Int,_}=string:to_integer(Token), 
		Int 
		end || 
		Token<-string:tokens(Time,":")
	],
	{{YYYY,MM,DD},{Hh,Mm,Ss}}.

