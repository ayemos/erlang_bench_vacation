%%% An implementation of vacation benchmark from STAMP benchmark 
%%% suite(http://stamp.stanford.edu) written in Erlang.
%%%
-module(bench).
-export([
         run/0,
         run/1]).

-include("bench.hrl").

run() ->
    FileName = "bench.config",
    run([FileName]).

run(Args) ->
    C = args_to_config(Args),
    erlang:set_cookie(node(), C#config.cookie),
    io:format("Table nodes: ~p~n",[C#config.table_nodes]),
    application:start(mnesia),
    mnesia:start(C#config.table_nodes),
    manager:start(C),
    client:run(C).

%%% Private

%%% Config
args_to_config(C) when is_record(C, config) ->
    C;
args_to_config(Args) when is_list(Args) ->
    do_args_to_config(Args, []).

do_args_to_config([{Key, Val} | Rest], Acc) when is_list(Acc) ->
    do_args_to_config(Rest, Acc ++ [{Key, Val}]);
do_args_to_config([FileName | Rest], Acc) when is_list(Acc) ->
    io:format("Reading configuration file ~p...", [FileName]),
    case file:consult(FileName) of
        {ok, Config} ->
            io:format(" ok~n", []),
            do_args_to_config(Rest, Acc ++ Config);
        {error, Reason} ->
            io:format(" FAILED: ~s~n",
                      [[lists:flatten(file:format_error(Reason))]]),
            {error, {args_to_config, FileName, Reason}}
    end;
do_args_to_config([], Acc) when is_list(Acc) ->
    verify_config(Acc, #config{}).

verify_config([{Tag, Val} | T], C) ->
    case Tag of
        cookie when is_atom(Val) ->
            verify_config(T, C#config{cookie = Val});
        table_nodes when is_list(Val) >= 0 ->
            verify_config(T, C#config{table_nodes = Val});
        client_nodes when is_list(Val) >= 0 ->
            verify_config(T, C#config{client_nodes = Val});
        n_queries when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_queries = Val});
        n_query_types when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_query_types = Val});
        n_range when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_range = Val});
        n_relations when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_relations = Val});
        n_tasks when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_tasks = Val});
        n_user_tasks when is_integer(Val), Val >= 0 ->
            verify_config(T, C#config{n_user_tasks = Val});
        storage_type when is_atom(Val) ->
            verify_config(T, C#config{storage_type = Val});
        transaction_type when is_atom(Val) ->
            verify_config(T, C#config{transaction_type = Val});
        _ ->
            io:format("Bad config value: ~p~n", [{Tag, Val}]),
            exit({bad_config_value, {Tag, Val}})
    end;
verify_config([], C) ->
    display_config(C),
    C;
verify_config(Config, _) ->
    io:format("Bad config: ~p~n", [Config]).

display_config(C) when is_record(C, config) ->
    io:format("Actual configuration...~n", []),
    Fields = record_info(fields, config),
    [config | Values] = tuple_to_list(C),
    display_config(Fields, Values).

display_config([F | Fields], [V | Values]) ->
    io:format("    ~s ~p~n", [left(F), V]),
    display_config(Fields, Values);
display_config([], []) ->
    ok.

left(Term) ->
    string:left(lists:flatten(io_lib:format("~p", [Term])), 27, $.).

