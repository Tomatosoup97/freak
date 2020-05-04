#!/bin/sh
while true; do
    inotifywait -e move_self -e modify thesis.tex;
    sleep 0.1;
    make build_auto;
done
