# NewRelic for Erlang

This library implements the New Relic API and allows sending arbitrary
metrics directly to their collectors from Erlang. (New
Relic)[http://newrelic.com/] is a paid "application monitoring"
service.

You need to supply the metrics on the correct format. If you happen to
be using (statman)[https://github.com/knutin/statman] you can use the
included `newrelic_statman` transformer. It is fairly easy to
transform the metrics, so if you're using Folsom, estatsd or your own
tools, have a look at `newrelic_statman` to see how it's done.



## Statman integration

If you're using Statman and use the following conventions for naming
your keys, you can use New Relic "for free".


 * `{<<"/hello/world">>, {class, segment}}` - Web transaction, class
   and segment can be anything and will show up in the "Performance
   breakdown"
 * `{<<"/hello/world">>, {db, <<"something">>}}` - Web transaction
   with database access, will show up in the "Performance breakdown"
   as well as "Overview" and "Database"
 * `{<<"/hello/world">>, {ext, <<"some.host.name">>}}` - External call
   inside a web transaction, will show up in the "Performance
   breakdown" and "External services"
 * `{foo, bar}` - Background task

