-module(util).
-export([
         num_seq/1,

         shuffle_array/1
        ]).

num_seq(N) ->
        num_seq_rec(N, []).

num_seq_rec(N, Arr) when N >= 1 ->
        num_seq_rec(N - 1, [N | Arr]);
num_seq_rec(_, Arr) ->
        Arr.

shuffle_array([]) ->
    [];
shuffle_array([X]) ->
    [X];
shuffle_array([X | Rem]) ->
    T = random:uniform(length(Rem) + 1) - 1,
    {L, R} = lists:split(T, shuffle_array(Rem)),
    lists:append([L, [X], R]).

