-module(newrelic_statman).
-compile([export_all]).

poll() ->
    {ok, Keys} = statman_aggregator:get_keys(),
    {Histograms, _} =
        lists:unzip(
          lists:filter(fun ({_, Type}) -> Type =:= histogram end, Keys)),

    lists:map(fun transform/1, lists:map(fun get_histogram/1, Histograms)).

get_histogram(Key) ->
    {ok, Aggregated} = statman_aggregator:get_window(Key, 60),
    {Key, statman_histogram:summary(Aggregated)}.


transform({{_Node, {Scope, {Class, Segment}}}, Summary}) ->

    [{[{name, <<(class2bin(Class))/binary, "/", (segment2bin(Segment))/binary>>},
       {scope, scope2bin(Scope)}]},
     [proplists:get_value(observations, Summary),
      proplists:get_value(sum, Summary) / 1000000,
      proplists:get_value(sum, Summary) / 1000000,
      proplists:get_value(min, Summary) / 1000000,
      proplists:get_value(max, Summary) / 1000000,
      proplists:get_value(sum2, Summary) / 1000000000000
     ]
    ];

transform({{_Node, {Scope, total}}, Summary}) ->
    [{[{name, <<"WebTransaction/Uri", Scope/binary>>},
       {scope, <<"">>}]},
     [proplists:get_value(observations, Summary),
      proplists:get_value(sum, Summary) / 1000000,
      proplists:get_value(sum, Summary) / 1000000,
      proplists:get_value(min, Summary) / 1000000,
      proplists:get_value(max, Summary) / 1000000,
      proplists:get_value(sum2, Summary) / 1000000000000
     ]
    ] ++
   [{[{name, <<"WebTransaction">>},
       {scope, <<"">>}]},
     [proplists:get_value(observations, Summary),
      proplists:get_value(sum, Summary) / 1000000,
      proplists:get_value(sum, Summary) / 1000000,
      proplists:get_value(min, Summary) / 1000000,
      proplists:get_value(max, Summary) / 1000000,
      proplists:get_value(sum2, Summary) / 1000000000000
     ]
    ] ++
   [{[{name, <<"HttpDispatcher">>},
       {scope, <<"">>}]},
     [proplists:get_value(observations, Summary),
      proplists:get_value(sum, Summary) / 1000000,
      proplists:get_value(sum, Summary) / 1000000,
      proplists:get_value(min, Summary) / 1000000,
      proplists:get_value(max, Summary) / 1000000,
      proplists:get_value(sum2, Summary) / 1000000000000
     ]
    ].



class2bin(db) -> <<"Database">>;
class2bin(Atom) when is_atom(Atom) ->
    [F | R] = atom_to_list(Atom),
    list_to_binary([string:to_upper(F) | R]).

segment2bin(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom)).

scope2bin(Url) when is_binary(Url) ->
    <<"WebTransaction/Uri", Url/binary>>.
