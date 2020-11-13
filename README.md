OpenTelemetry Hackney
=====

Use the same as you would [Hackney](https://github.com/benoitc/hackney) but with module `otel_hackney` instead of
`hackney`:

``` erlang
{ok, 200, _, _} = otel_hackney:request("http://localhost:3000")
```
