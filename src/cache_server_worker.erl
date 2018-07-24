-module(cache_server_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([insert/3, lookup/1, lookup_by_date/2]).
%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-define(T, ?MODULE).
-record(state, {
	interval,
	interval_timer_Ref
}).

%% API
start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
	gen_server:cast(Pid, stop).

insert(Key, Value, LiveTime) ->
	gen_server:call(whereis(worker),{insert,{Key,Value,LiveTime}}).

lookup(Key) ->
	gen_server:call(whereis(worker),{lookup,{Key}}).

lookup_by_date(DateFrom, DateTo) ->
	gen_server:call(whereis(worker),{lookup_by_date,{DateFrom, DateTo}}).

%% gen_server callbacks
init(Interval) ->
	{ok, IntervalTimerRef} = timer:send_after(Interval, delete_obsolete), 
	State = #state{
		interval = Interval,
		interval_timer_Ref = IntervalTimerRef
	},
	cache_server_ets:create(),
	register(worker,self()),
	{ok, State}.

terminate(_Reason, _State) ->
ok.

handle_call({insert,{Key,Value,LiveTime}},_From,State) ->
	Reply = cache_server_ets:insert(Key,Value,LiveTime),
	{reply,Reply,State};
handle_call({lookup,{Key}},_From,State) ->
	Reply = cache_server_ets:lookup(Key),
	{reply,Reply,State};
handle_call({lookup_by_date,{DateFrom, DateTo}},_From,State) ->
	Reply = cache_server_ets:lookup_by_date(DateFrom, DateTo),
	{reply,Reply,State};
handle_call(_Request, _From, State) ->
	{reply, ignore, State}.

handle_cast(stop, State) ->
	ok = cache_server_ets:destroy(cache),
	{stop, normal, State}.

handle_info(stop, State) ->
	ok = cache_server_ets:destroy(cache),
	{stop, normal, State};
handle_info(delete_obsolete, State) ->
	#state{interval = Interval, interval_timer_Ref  = IntervalTimerRef} = State,
	{ok, cancel} = timer:cancel(IntervalTimerRef),
	cache_server_ets:delete_obsolete(),
	{ ok, NewIntervalTimerRef} = timer:send_after(Interval, delete_obsolete),
	{noreply, State#state{interval_timer_Ref = NewIntervalTimerRef}};
handle_info(_Info, State) -> 
	{noreply,State}.		


	
	

