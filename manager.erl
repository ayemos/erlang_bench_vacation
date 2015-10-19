-module(manager).
-export([
         start/1,

         send_activity/3,

         count_contention/1,

         add_reservation/4
        ]).

-include("bench.hrl").

start(C) ->
    ets:new(contention_count, [set]),
    ets:new(activity_state, [set]),
    create_tables(C),
    Populate =
	fun() ->
		populate_car(write, C),
        populate_customer(write, C)
	end,
    mnesia:activity(sync_dirty, Populate, [], mnesia_frag),
    ok.

create_tables(C) ->
    ?d("Delete old tables...~n", []),
    mnesia:delete_table(car),
    mnesia:delete_table(room),
    mnesia:delete_table(flight),
    mnesia:delete_table(reservation),

    ?d("Create new tables...~n", []),

    CarDef = [{attributes, record_info(fields, car)},
              {C#config.storage_type, C#config.table_nodes}
             ],

    ?APPLY(mnesia, create_table, [car, CarDef]),

    %%% ToDo: flight, room...

    CustomerDef = [{attributes, record_info(fields, customer)},
                   {C#config.storage_type, C#config.table_nodes}
                  ],
    ?APPLY(mnesia, create_table, [customer, CustomerDef]).

populate_car(Wlock, C) ->
    random:seed(),
    N = C#config.n_relations,
    ?d("    Populate ~p cars...", [N]),
    do_populate_car(Wlock, N - 1, C).

do_populate_car(Wlock, Id, C) when Id >= 0 ->
    add_car(Wlock, Id, rand:uniform(5) * 100, rand:uniform(5) * 10 + 50),
    do_populate_car(Wlock, Id - 1, C);
do_populate_car(_Wlock, _, _) ->
    ?d(" totally ~p bytes~n", 
	      [mnesia:table_info(car, memory) * 4]),
    ok.

add_car(Wlock, Id, Num, Price) ->
    add_reservation(Wlock, Id, Num, Price).

add_reservation(Wlock, Id, Num, Price) ->
    case mnesia:read({car, Id}) =:= [] of
        true ->
            % create new record
            Reservation = #reservation{id = Id,
                                       n_total = Num,
                                       n_used = 0,
                                       n_free = Num,
                                       price = Price},
            Car = #car{id = Id, 
                       reservation = Reservation},
            ?APPLY(mnesia, write, [car, Car, Wlock]);
        false ->
            % update reservation
            Car = mnesia:read({car, Id}),
            Reservation = Car#car.reservation,
            Free = Reservation#reservation.n_free,
            NewCar = #car{id = Id, 
                       reservation = Reservation#reservation{n_free = Free + Num}},
            ?APPLY(mnesia, write, [car, NewCar, Wlock])
    end.

populate_customer(Wlock, C) ->
    random:seed(),
    N = C#config.n_relations,
    ?d("    Populate ~p customers...", [N]),
    do_populate_customer(Wlock, N - 1, C).
do_populate_customer(Wlock, Id, C) when Id >= 0 ->
    add_customer(Wlock, Id),
    do_populate_customer(Wlock, Id - 1, C);
do_populate_customer(_Wlock, _, _) ->
    ?d(" totally ~p bytes~n", 
	      [mnesia:table_info(customer, memory) * 4]),
    ok.

add_customer(Wlock, Id) ->
    Customer = #customer{id = Id, 
                         reservation_info_list = []},
    ?APPLY(mnesia, write, [customer, Customer, Wlock]).

%%% Private
send_activity(Fun, Args, _Candidates) ->
    %%% check contention count and activity state
    mnesia:activity(transaction, Fun, Args).

count_contention([Word | Rest]) ->
    ets:update_counter(contention_count, Word, {0, 1}),
    count_contention([Rest]);
count_contention([]) ->
    ok.
    

