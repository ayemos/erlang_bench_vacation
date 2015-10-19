-module(client).
-export([
         start/1,

         spawn_clients/3,

         init_client/2,
         client_loop/4,

         gen_trans/2,
         gen_t1/2
        ]).

-include("bench.hrl").

start(C) ->
    spawn_clients(C, C#config.client_nodes, C#config.n_tasks).

spawn_clients(C, Nodes, PerNode) ->
    [spawn_link(Node, ?MODULE, init_client, [self(), C]) ||
	Node <- Nodes,
	_    <- lists:seq(1, PerNode)].

init_client(Monitor, C) ->
    %%% TODO: fix
%    process_flag(trap_exit, true),
    Tables = mnesia:system_info(tables),
    ok = mnesia:wait_for_tables(Tables, infinity),
    rand:seed(exsplus),
%    Counters = reset_counters(C, C#config.statistics_detail),
%    SessionTab = ets:new(bench_sessions, [public, {keypos, 1}]),
    client_loop(Monitor, C, sessiontab, counters).

client_loop(Monitor, C, SessionTab, _Counters) ->
    receive

%       {ReplyTo, Ref, get_statistics} ->
%           Stats = get_counters(C, Counters),
%           ReplyTo ! {self(), Ref, Stats},
%           client_loop(Monitor, C, SessionTab, Counters);
%       {ReplyTo, Ref, reset_statistics} ->
%           Stats = get_counters(C, Counters),
%           Counters2 = reset_counters(C, Counters),
%           ReplyTo ! {self(), Ref, Stats},
%           client_loop(Monitor, C, SessionTab, Counters2);
%       {_ReplyTo, _Ref, stop} ->
%           exit(shutdown);
        {'EXIT', Pid, Reason} when Pid == Monitor ->
            exit(Reason);
        {'EXIT', Pid, Reason} ->
            exit(Reason)
%           Node = node(Pid),
%           ?d("Worker on node ~p(~p) died: ~p~n", [Node, node(), Reason]),
%           Key = {worker,Node},
%           case get(Key) of
%               undefined -> ignore;
%               Pid       -> erase(Key);
%               _         -> ignore
%           end,
%           generator_loop(Monitor, C, SessionTab, Counters)
    after 0 ->
        Before = erlang:monotonic_time(),
        [mnesia:activity(transaction, gen_trans(C, SessionTab)) ||
         _ <- lists:seq(1, 100)],
        After = erlang:monotonic_time(),
        Elapsed = elapsed(Before, After),
        ?d("elapsed: ~p~n", [Elapsed])
            %       post_eval(Monitor, C, Elapsed, Res, Name, CommitSessions, SessionTab, Counters)
    end.

gen_trans(C, SessionTab) ->
%   UserTasks   = C#config.n_user_tasks,
%   OtherTasks1 = UserTasks + (100 - UserTasks) / 2,
%   OtherTasks2 = UserTasks + OtherTasks1 + (100 - UserTasks) / 4,

%   case rand:uniform(100) of
%       Rand when Rand > 0,             Rand =<     UserTasks   -> gen_t1(C, SessionTab);
%       Rand when Rand > UserTasks,     Rand =<     OtherTasks1 -> gen_t2(C, SessionTab);
%       Rand when Rand > OtherTasks1,   Rand =<     OtherTasks2 -> gen_t3(C, SessionTab);
%       Rand when Rand > OtherTasks2                            -> gen_t4(C, SessionTab)
%   end.
    gen_t1(C, SessionTab).

gen_t1(C, _SessionTab) ->
    trans_make_reservation(C).

gen_t2(C, _SessionTab) ->
    trans_delete_customer(C).

gen_t3(C, _SessionTab) ->
    trans_add_to_item_tables(C).

gen_t4(C, _SessionTab) ->
    trans_remove_from_item_tables(C).

trans_delete_customer(_C) ->
    ok.
trans_remove_from_item_tables(_C) ->
    ok.
trans_add_to_item_tables(_C) ->
    ok.
trans_make_reservation(C) ->
    Ids     = [rand:uniform(C#config.n_range) || _ <- lists:seq(1, C#config.n_queries)],
    Types   = [rand:uniform(C#config.n_query_types) || _ <- lists:seq(1, C#config.n_queries)],
%    ?d("ids: ~p~n", [Ids]),
%    ?d("types: ~p~n", [Types]),
    fun() ->
            do_make_reservation(Ids, Types, C)
    end.

do_make_reservation(Ids, Types, C) ->
    Cars = [lists:nth(1, mnesia:read(car, Id)) || Id <- Ids],
%    ?d("cars:~p~n", [Cars]),
    Prices = [Car#car.price || Car <- Cars],
%    ?d("prices:~p~n", [Prices]),
    [H | T] = Cars,
%    ?d("h: ~p~n", [H]),
%    ?d("t: ~p~n", [T]),
    Highest = lists:foldl(
                fun(X, Max) 
                      when  X#car.price > Max#car.price   -> X;
                            (_, Max)                      -> Max
                end, H, T),
%    ?d("highest:~p~n", [Highest#car.n_free]),
    case Highest#car.n_free of
        Free when Free > 0 ->
            Used = Highest#car.n_used,
            NewCar = Highest#car{n_free=Free - 1,
                                 n_used=Used + 1},
            ?APPLY(mnesia, write, [NewCar]),
            ?d("Reserved car:~p~n", [NewCar]);
        Free when Free =< 0 ->
            ?d("No free car:~p~n", [Highest]),
            ok
    end.

elapsed(Before, After) ->
    erlang:convert_time_unit(After-Before, native, micro_seconds).
