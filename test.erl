-module(test).
-export([run/0]).
-record(car,
        {
         id,
         n_total = 0,
         n_used = 0,
         n_free = 0,
         price
        }).

run() -> 
    Cars = [#car{id=1,price=100},
            #car{id=2,price=500},
            #car{id=3,price=300},
            #car{id=4,price=200}],
    io:format("cars:~p~n", [Cars]),
    Prices = [Car#car.price || Car <- Cars],
    io:format("price:~p~n", [Prices]),

    [H | T] = Cars,
    Highest = lists:foldl(
                fun(X, Max)
                      when X#car.price > Max#car.price -> X;
                    (_, Max)        ->   Max end, 
                H, T),
    io:format("high:~p~n", [Highest]).



