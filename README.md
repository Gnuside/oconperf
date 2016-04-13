# oconperf

OCaml connection performance test application.

## Build

~~~
make build_byte
~~~

## Run

~~~
./oconperf.byte client [-I iface] [-a4 IPv4Address] [-a6 IPv6Address] [-p port] [-w deadline]
./oconperf.byte server [-a4 IPv4Address] [-a6 IPv6Address] [-p port]
./oconperf.byte [--version][--help]
~~~



## Native code

~~~
make build_native
./oconperf
~~~
