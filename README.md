# [rush](https://github.com/rakivo/rush/tree/master)

# Currently supported functionality
```ninja
cflags = -Wall -Wextra -O2

rule cc
  command = cc $cflags -o $out -c $in

rule link
  command = cc -o $out $in

build foo.o: cc foo.c | foo.h
build bar.o: cc bar.c | bar.h
build main.o: cc main.c

build main: link foo.o bar.o main.o
```
