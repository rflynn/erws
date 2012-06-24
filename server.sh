#!/bin/bash

erl \
    +K true \
    -pa apps/*/ebin \
    -pa deps/*/ebin \
    -s lager \
    -s crypto \
    -s erws
