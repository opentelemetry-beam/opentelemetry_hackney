OpenTelemetry Hackney
=====

Use the same as you would [Hackney](https://github.com/benoitc/hackney) but with
module `otel_hackney` instead of `hackney`:

``` erlang
{ok, 200, _, _} = otel_hackney:request("http://localhost:3000")
```

The default Span name used in the above example will be `HTTP GET`. You can pass a
custom Span name or a function that is passed the HTTP method and the full url
of the request and returns a Span name:

```
{ok, 200, _, _} = otel_hackney:request(get, "http://localhost:3000/hello/otel", [],
                                       <<>>, [{span_name, fun(_Method, _Url) -> <<"/hello/{who}">> end}])
```
