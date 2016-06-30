#!/bin/sh

export PATH=.:$PATH
IFACE=${1:-lo}
IPADDR=$( ip addr show "$IFACE" |sed -n -e 's/[[:space:]]\+inet[[:space:]]\+\(.*\)\/.*/\1/p')

oconperf server -a "$IPADDR"

