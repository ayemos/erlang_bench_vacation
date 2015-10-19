-module(test).
-export([run/0]).
-record(car,
        {
         id,
         reservation
        }).

run() ->
    mnesia:start(),
    mnesia:create_table(car,
                        [{attributes, record_info(fields, car)},
                         {index, id},
                         {ram_copies, []}]).
