#!/bin/bash
set -eu
trap 'rm -f -- "$tmp"' EXIT

random=$(mktemp)
tmp=$(mktemp)

start_time=$(date +%s)
${CASK} emacs --module-assertions --batch -l tests/wait_10_seconds.el "$random" 2>&1 | tee "$tmp" &
emacs_pid=$(pgrep -f "$random") # $! dont work beacause cask
rm "$random"

sleep 4 # give emacs time to start.
kill -n 42 "$emacs_pid" || exit 2
wait "$emacs_pid"
end_time=$(date +%s)

grep -q KeyboardInterrupt "$tmp" || exit 3 # dont work in CI for some reason
exit  $(( ( start_time + 7 ) < end_time ))
