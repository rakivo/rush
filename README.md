# [rush](https://github.com/rakivo/rush/tree/master)

# Currently Supported Functionality
```ninja
# declare cflags to then shadow it in different jobs
cflags =

rule cc
  command = cc $cflags -o $out -c $in
  description = compile $in to $out

rule link
  command = cc $cflags -o $out $in
  description = link $in into $out

build foo.o: cc foo.c | foo.h
build bar.o: cc bar.c | bar.h
build main.o: cc main.c
  cflags = -Wall -Wextra -O3

build main: link foo.o $
  bar.o $
  main.o
  cflags = -O3
```
