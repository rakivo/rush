# [rush](https://github.com/rakivo/rush/tree/master)

# Currently Supported Functionality
```ninja
cflags = -std=gnu99 -Wall -Wextra -O3
builddir = build

phony all
build all: $builddir/main run

rule cc
  depfile = $out.d
  command = gcc -MD -MF $out.d $cflags -o $out -c $in

rule link
  command = cc $cflags -o $out $in

build $builddir/foo.o: cc foo.c
build $builddir/bar.o: cc bar.c
build $builddir/main.o: cc main.c

build $builddir/main: link $builddir/foo.o $
                           $builddir/bar.o $
                           $builddir/main.o
  cflags = -std=gnu99 -O2

phony run
build run:
  command = ./$builddir/main

phony clean
build clean:
  command = rm -f $builddir/*
```
