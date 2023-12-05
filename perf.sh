#!/usr/bin/env bash
bin_path=$1

perf record --call-graph=dwarf ./_build/default/bin/${bin_path}.exe
perf script | stackcollapse-perf.pl | flamegraph.pl > flamegraph.svg
rm perf.data
xdg-open flamegraph.svg