-module(newrelic).
-compile([export_all]).

-define(BASE_URL, "http://~s/agent_listener/invoke_raw_method?").

-define(l2b(L), list_to_binary(L)).
-define(l2i(L), list_to_integer(L)).

%%
%% API
%%

%% @doc: Connects to New Relic and sends the hopefully correctly
%% formatted data and registers it under the given hostname.
push(Hostname, Data) ->
    Collector = get_redirect_host(),
    RunId = connect(Collector, Hostname),
    push_metric_data(Collector, RunId, Data).


%%
%% NewRelic protocol
%%


get_redirect_host() ->
    Url = url([{method, get_redirect_host}]),
    case request(Url) of
        {ok, {{200, "OK"}, _, Body}} ->
            {struct, Struct} = mochijson2_fork:decode(Body),
            binary_to_list(proplists:get_value(<<"return_value">>, Struct));
        {ok, {{503, _}, _, _}} ->
            throw(newrelic_down)
    end.


connect(Collector, Hostname) ->
    Url = url(Collector, [{method, connect}]),

% local_config['pid'] = os.getpid()
%         local_config['language'] = 'python'
%         local_config['host'] = socket.gethostname()
%         local_config['app_name'] = app_names
%         local_config['identifier'] = ','.join(app_names)
%         local_config['agent_version'] = version
%         local_config['environment'] = environment
%         local_config['settings'] = settings

    Data = [{struct, [
              {pid, ?l2i(os:getpid())},
              {language, <<"python">>},
              {host, ?l2b(Hostname)},
              {app_name, [app_name()]},
              {identifier, app_name()},
              {agent_version, <<"1.7.0.31">>},
              {environment, []},
              {settings, {struct, []}}
             ]}],
    case request(Url, mochijson2_fork:encode(Data)) of
        {ok, {{200, "OK"}, _, Body}} -> 
            {struct, Struct} = mochijson2_fork:decode(Body),
            {struct, Return} = proplists:get_value(<<"return_value">>, Struct),
            proplists:get_value(<<"agent_run_id">>, Return);
        {ok, {{503, _}, _, _}} ->
            throw(newrelic_down)
    end.


push_metric_data(Collector, RunId, MetricData) ->
    Url = url(Collector, [{method, metric_data},
                          {run_id, RunId}]),

    Data = [RunId,
            now_to_seconds() - 60,
            now_to_seconds(),
            MetricData],

    case request(Url, mochijson2_fork:encode(Data)) of
        {ok, {{200, "OK"}, _, Response}} ->
            {struct, Struct} = mochijson2_fork:decode(Response),
            case proplists:get_value(<<"exception">>, Struct) of
                undefined ->
                    ok;
                Exception ->
                    {error, Exception}
            end;
        {ok, {{503, _}, _, _}} ->
            throw(newrelic_down)
    end.



%%
%% HELPERS
%%


now_to_seconds() ->
    {MegaSeconds, Seconds, _} = os:timestamp(),
    MegaSeconds * 1000000 + Seconds.


app_name() ->
    {ok, Name} = application:get_env(newrelic, application_name),
    list_to_binary(Name).


license_key() ->
    {ok, Key} = application:get_env(newrelic, license_key),
    Key.



request(Url) ->
    request(Url, <<"[]">>).

request(Url, Body) ->
    lhttpc:request(Url, post, [{"Content-Encoding", "identity"}],
                   Body, 30000).


url(Args) ->
    url("collector.newrelic.com", Args).

url(Host, Args) ->
    BaseArgs = [{protocol_version, 9},
                {license_key,  license_key()},
                {marshal_format, json}],


    lists:flatten([io_lib:format(?BASE_URL, [Host]), urljoin(Args ++ BaseArgs)]).

urljoin([H | T]) ->
    [url_var(H) | [["&", url_var(X)] || X <- T]];
urljoin([]) ->
    [].

url_var({Key, Value}) -> [to_list(Key), "=", to_list(Value)].


to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(List) when is_list(List)-> List;
to_list(Int) when is_integer(Int) -> integer_to_list(Int).




sample_data() ->
    [
     [{struct, [{name, <<"MFA/gen_server:call/2">>},
        {scope, <<"WebTransaction/Uri/test">>}]},
      [20,
       2.0030434131622314,
       2.0030434131622314,
       0.10012507438659668,
       0.10023093223571777,
       0.2006091550569522]],

     [{struct, [{name, <<"Database/Redis-HSET">>},
        {scope, <<"WebTransaction/Uri/test">>}]},
      [20,
       2.0030434131622314,
       2.0030434131622314,
       0.10012507438659668,
       0.10023093223571777,
       0.2006091550569522]],

     [{struct, [{name, <<"S3/GET">>},
        {scope, <<"WebTransaction/Uri/test">>}]},
      [20,
       2.0030434131622314,
       2.0030434131622314,
       0.10012507438659668,
       0.10023093223571777,
       0.2006091550569522]],

     [{struct, [{name, <<"WebTransaction">>},
        {scope, <<"">>}]},
      [1,
       2.0055530071258545,
       0.00017213821411132812,
       2.0055530071258545,
       2.0055530071258545,
       4.022242864391558]],

     [{struct, [{name, <<"HttpDispatcher">>},
        {scope, <<"">>}]},
      [1,
       2.0055530071258545,
       0.00017213821411132812,
       2.0055530071258545,
       2.0055530071258545,
       4.022242864391558]],

     [{struct, [{name, <<"Database/allWeb">>},
        {scope, <<"">>}]},
      [1,
       2.0055530071258545,
       0.00017213821411132812,
       2.0055530071258545,
       2.0055530071258545,
       4.022242864391558]],

     [{struct, [{name, <<"Memcache/allWeb">>},
        {scope, <<"">>}]},
      [1,
       2.0055530071258545,
       0.00017213821411132812,
       2.0055530071258545,
       2.0055530071258545,
       4.022242864391558]],

     [{struct, [{name, <<"WebTransaction/Uri/test">>},
        {scope, <<"">>}]},
      [1,
       2.0055530071258545,
       0.00017213821411132812,
       2.0055530071258545,
       2.0055530071258545,
       4.022242864391558]],

     [{struct, [{name, <<"Python/WSGI/Application">>},
        {scope, <<"WebTransaction/Uri/test">>}]},
      [1,
       2.005380868911743,
       7.176399230957031e-05,
       2.005380868911743,
       2.005380868911743,
       4.021552429397218]]
    ].

