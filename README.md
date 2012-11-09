# NewRelic for Erlang

This library implements the New Relic API and allows sending arbitrary
metrics directly to their collectors from Erlang. [New
Relic](http://newrelic.com/) is a paid "application monitoring"
service.

You need to supply the metrics on the correct format. If you happen to
be using [statman](https://github.com/knutin/statman) you can use the
included `newrelic_statman` transformer. It is fairly easy to
transform the metrics, so if you're using Folsom, estatsd or your own
tools, have a look at `newrelic_statman` to see how it's done.

## Configuration

Two application environment variables must be set in the `newrelic` app:

 * `application_name`: human readable name of the app, will show up in the web interface
 * `license_key`: secret license key


## Statman integration

If you're using Statman and use the following conventions for naming
your keys, you can use New Relic "for free".


 * `{<<"/hello/world">>, {class, segment}}` - Web transaction, class
   and segment can be anything and will show up in the "Performance
   breakdown"
 * `{<<"/hello/world">>, {db, <<"something">>}}` - Web transaction
   with database access, will show up in the "Performance breakdown"
   as well as "Overview". Unfortunately not in "Database" yet
 * `{<<"/hello/world">>, total}` - Total time of the transaction,
   inclusive any children. Will show up in the "Overview"
 * `{<<"/hello/world">>, {ext, <<"some.host.name">>}}` - External call
   inside a web transaction, will show up in the "Performance
   breakdown" and "External services"
 * `{foo, bar}` - Background task

## JSON Encoding
This fork assumes that the [`mochijson2_fork`](https://github.com/blinkov/sockjs-erlang/blob/master/src/mochijson2_fork.erl) module is available in the path.