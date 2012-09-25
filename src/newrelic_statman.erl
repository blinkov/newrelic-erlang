-module(newrelic_statman).
-compile([export_all]).

poll() ->
    {ok, Metrics} = statman_aggregator:get_window(60),
    Histograms = lists:filter(
                   fun (Metric) ->
                           proplists:get_value(type, Metric) =:= histogram andalso
                               (not is_list(proplists:get_value(node, Metric))) andalso
                               proplists:get_value(value, Metric) =/= []
                   end,
                   Metrics),

    Ms = lists:filter(
           fun (M) -> M =/= [] end,
           lists:map(fun transform/1, Histograms)),

    [add_total(Ms) | Ms].




transform(Metric) ->
    Summary = statman_histogram:summary(proplists:get_value(value, Metric)),
    Data = [proplists:get_value(observations, Summary),
            proplists:get_value(sum, Summary) / 1000000,
            proplists:get_value(sum, Summary) / 1000000,
            proplists:get_value(min, Summary) / 1000000,
            proplists:get_value(max, Summary) / 1000000,
            proplists:get_value(sum2, Summary) / 1000000000
           ],

    case proplists:get_value(key, Metric) of
        {Scope, {Class, Segment}} when is_binary(Scope) ->
            [{[{name, <<(class2bin(Class))/binary, "/", (segment2bin(Segment))/binary>>},
               {scope, scope2bin(Scope)}]},
             Data];
        {Scope, total} when is_binary(Scope) ->
            [{[{name, <<"WebTransaction/Uri", Scope/binary>>},
               {scope, <<"">>}]},
             Data];
        _ ->
            []
    end.


add_total(Ms) ->
    N    = lists:sum(pluck(1, Ms)),
    Sum  = lists:sum(pluck(2, Ms)),
    Min  = lists:min(pluck(4, Ms)),
    Max  = lists:max(pluck(5, Ms)),
    Sum2 = lists:sum(pluck(6, Ms)),

    [{[{name, <<"HttpDispatcher">>},
       {scope, <<"">>}]},
     [N, Sum, Sum, Min, Max, Sum2]].

pluck(N, L) ->
    lists:map(fun ([_, []]) -> 0;
                  ([_, D]) -> lists:nth(N, D)
              end, L).



class2bin(db) -> <<"Database">>;
class2bin(Atom) when is_atom(Atom) ->
    [F | R] = atom_to_list(Atom),
    list_to_binary([string:to_upper(F) | R]).

segment2bin(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom)).

scope2bin(Url) when is_binary(Url) ->
    <<"WebTransaction/Uri", Url/binary>>.
