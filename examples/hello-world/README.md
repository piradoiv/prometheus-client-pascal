[Home](..) | [Examples](..)

# Hello, Prometheus!

This is a very basic usage example of the library, and probably useless, but includes everything you need to know for getting started.

`TPrometheusRegistry` maintains a store of values in memory, and coordinates everything, from exposing the storage to freeing memory when needed.

You can create instances of `TPrometheusCounter`, `TPrometheusGauge` and `TPrometheusHistogram` directly from the Registry.

## How to build this example

A working installation of [Free Pascal](https://www.freepascal.org) compiler, 3.0.0 or newer, is the only requirement.

For convenience, there is a GNU Make's [Makefile](Makefile) that automates the building:

```
# Compile the example
$ make

# Run it
$ ./helloworld
```

Please take a look to the [Makefile](Makefile) to see how you can link your program to this library.
