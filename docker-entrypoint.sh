#!/bin/sh
# docker-entrypoint.sh
# Wraps the lightblue Express server launch with fixed options.
# Extra arguments (e.g. --jsemid 207) are appended from CMD / docker run.

set -e

exec lightblue jp jsem \
    "$@" \
    --nsample 1 \
    -s express \
    -f "${JSEM_DATA}" \
    --nparse 4 \
    --ntypecheck 4 \
    --nproof 1 \
    --maxdepth 5 \
    --depth 0
