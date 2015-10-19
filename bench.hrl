-record(config,
        {
          cookie                    = abc,
          storage_type              = ram_copies,
          transaction_type          = mnesia_only,
          table_nodes               = [],
          client_nodes              = [],
          n_queries                 = 2,
          n_query_types             = 1,
          n_range                   = 90,
          n_relations               = 16384,
          n_tasks                   = 4096,
          n_user_tasks              = 90
         }).

-record(customer,
        {
         id,
         reservation_info_list      = []
        }).

-record(reservation_info,
        {
         id,
         price                      = 0,
         reservation_type           = undefined
        }).

-record(car,
        {
         id,
         n_total                  = 0,
         n_used                   = 0,
         n_free                   = 0,
         price                      = 0
        }).

-define(d(Format, Args),
        io:format("~s" ++ Format, [string:left(lists:flatten(io_lib:format("~p(~p):", [?MODULE, ?LINE])), 30, $ ) | Args])).

-define(e(Format, Args),
        begin 
            ok = error_logger:format("~p(~p): " ++ Format, [?MODULE, ?LINE | Args]),
            timer:sleep(1000)
        end).

-define(ERROR(M, F, A, R),
        ?e("~w:~w~p\n\t ->~p\n", [M, F, A, R])).

-define(APPLY(M, F, A),
        fun() ->
                case catch apply(M, F, A) of
                    ok -> {ok, ok};
                    {atomic, R} -> {ok, R};
                    {ok, R} -> {ok, R};
                    {aborted, R} -> ?ERROR(M, F, A, R);
                    {error, R} ->  ?ERROR(M, F, A, R);
                    R -> ?ERROR(M, F, A, R)
                end
        end()).

