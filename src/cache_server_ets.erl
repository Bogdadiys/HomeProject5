-module(cache_server_ets).

%% API
-export([create/0]).
-export([insert/3, lookup/1, lookup_by_date/2]).
-export([delete_obsolete/0]).

%% API
create() ->
	ets:new(cache, [public, named_table, {write_concurrency, true}]).

insert(Key, Value, LiveTime) ->
	case is_integer(LiveTime) of
        	true -> ets:insert(cache, [{Key, Value, os:system_time(seconds) + LiveTime, erlang:localtime()}]),
			ok;
		false -> {error,bad_argument}
	end.

lookup(Key) ->
	case ets:lookup(cache,Key) of
		[{Key,Value,LiveTime,_Date}] ->
			case LiveTime < os:system_time(seconds) of
				false ->
					{ok,Value};
				true ->
					ets:delete(cache,Key),
					{error,"live_time is over"}
			end;
		[] ->
			{error, undefined}
	end.

lookup_by_date(DateFrom, DateTo) ->
lookup_by_date(DateFrom, DateTo, [], ets:first(cache)).

lookup_by_date(_,_, Acc, '$end_of_table') ->
	{ok, Acc};
lookup_by_date(DateFrom, DateTo, Acc, Key) ->
	NextKey = ets:next(cache,Key),
        [{Key, Value, LiveTime, Date}] = ets:lookup(cache, Key),
	NewAcc = case LiveTime < os:system_time(seconds) of
		false ->
			case (DateFrom < Date andalso DateTo > Date) of
				true  -> [Value|Acc];
				false -> Acc
			end;
		true  ->
			ets:delete(cache,Key),
			[del|Acc]
	end,
       	lookup_by_date(DateFrom, DateTo, NewAcc, NextKey).

delete_obsolete() ->
delete_obsolete(ets:first(cache)).

delete_obsolete('$end_of_table') ->
	ok;
delete_obsolete(Key) ->
	[{Key, _Value, LiveTime, _Date}] = ets:lookup(cache, Key),
	NextKey = ets:next(cache,Key),
	case LiveTime < os:system_time(seconds) of
		false ->
			void;
		true  ->
			ets:delete(cache,Key)
	end,
	delete_obsolete(NextKey).
