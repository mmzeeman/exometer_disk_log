# exometer_disk_log

Simple `disk_log` based persistency reporter for exometer.

## Synopsis

[Exometer](https://github.com/Feuerlabs/exometer) is Erlang metrics aggregator from Feuerlabs. 
It's a perfect tool for a realtime metrics collection and simple statistical analysis. 

It has builtin reporters allowing one to export metrics to graphite or statsd. What I was missing
was persistence layer allowing me to follow the changes without relying on external tools. 



