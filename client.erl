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
            exit(Reason)
%       {'EXIT', Pid, Reason} ->
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
        gen_trans(C, SessionTab),
        % mnesia:activity(transaction, Fun, [Wlock], Mod),
        After = erlang:monotonic_time(),
        Elapsed = elapsed(Before, After)
            %       post_eval(Monitor, C, Elapsed, Res, Name, CommitSessions, SessionTab, Counters)
    end.

gen_trans(C, SessionTab) ->
    UserTasks   = C#config.n_user_tasks,
    OtherTasks1 = UserTasks + (100 - UserTasks) / 2,
    OtherTasks2 = UserTasks + OtherTasks1 + (100 - UserTasks) / 4,

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
    ?d("ids: ~p~n", [Ids]),
    ?d("types: ~p~n", [Types]).
    
%   long maxPrices[NUM_RESERVATION_TYPE] = { -1, -1, -1 };
%   long maxIds[NUM_RESERVATION_TYPE] = { -1, -1, -1 };
%   long n;
%   N_QUERY = rand:uniform(C#config.n_tasks),
%   long customerId = random_generate(randomPtr) % queryRange + 1;
%   for (n = 0; n < numQuery; n++) {
%       types[n] = random_generate(randomPtr) % NUM_RESERVATION_TYPE;
%       ids[n] = (random_generate(randomPtr) % queryRange) + 1;
%   }
%   bool_t isFound = FALSE;
%   for (n = 0; n < numQuery; n++) {
%       long t = types[n];
%       long id = ids[n];
%       long price = -1;
%       switch (t) {
%           case RESERVATION_CAR:
%               if (MANAGER_QUERY_CAR(managerPtr, id) >= 0) {
%                   price = MANAGER_QUERY_CAR_PRICE(managerPtr, id);
%               }
%               break;
%           case RESERVATION_FLIGHT:
%               if (MANAGER_QUERY_FLIGHT(managerPtr, id) >= 0) {
%                   price = MANAGER_QUERY_FLIGHT_PRICE(managerPtr, id);
%               }
%               break;
%           case RESERVATION_ROOM:
%               if (MANAGER_QUERY_ROOM(managerPtr, id) >= 0) {
%                   price = MANAGER_QUERY_ROOM_PRICE(managerPtr, id);
%               }
%               break;
%           default:
%               assert(0);
%       }
%       if (price > maxPrices[t]) {
%           maxPrices[t] = price;
%           maxIds[t] = id;
%           isFound = TRUE;
%       }
%   } /* for n */
%   if (isFound) {
%       MANAGER_ADD_CUSTOMER(managerPtr, customerId);
%   }
%   if (maxIds[RESERVATION_CAR] > 0) {
%       MANAGER_RESERVE_CAR(managerPtr,
%                           customerId, maxIds[RESERVATION_CAR]);
%   }
%   if (maxIds[RESERVATION_FLIGHT] > 0) {
%       MANAGER_RESERVE_FLIGHT(managerPtr,
%                               customerId, maxIds[RESERVATION_FLIGHT]);
%   }
%   if (maxIds[RESERVATION_ROOM] > 0) {
%       MANAGER_RESERVE_ROOM(managerPtr,
%                               customerId, maxIds[RESERVATION_ROOM]);
%   }
%   TM_END();
%
elapsed(Before, After) ->
    erlang:convert_time_unit(After-Before, native, micro_seconds).
