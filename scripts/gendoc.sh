#!/bin/sh

erl -noshell -s edoc files $* -s init stop
