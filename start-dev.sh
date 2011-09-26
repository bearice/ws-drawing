#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
make && exec erl -pa ebin edit deps/*/ebin \
    -sname drawing \
    -s drawing \
    +K true \
    -boot start_sasl 
