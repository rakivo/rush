# [rush](https://github.com/rakivo/rush/tree/master)

`rush` is a simple, peak-performance build system heavily inspired by Ninja.
It’s designed for speed and clarity — making it easy to build projects from small experiments to large codebases.

# Quick start
```console
$ cargo install rush-build
```

## Minimal showcase
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
```

## Real-World Example: Building Lua
```ninja
cflags = -std=gnu99 -O2 -Wall -Wextra -DLUA_COMPAT_5_3 -lm

rule cc
  description = compile $in to $out
  command = gcc -MD -MF $out.d -o $out -c $in $cflags
  depfile = $out.d

rule link
  description = link $in into $out
  command = gcc -o $out $in $cflags

build src/lapi.o: cc src/lapi.c
...
build src/lzio.o: cc src/lzio.c
build lua: link src/lapi.o src/lauxlib.o ... src/lzio.o

build clean:
  command = rm -f src/*.o lua
```

> [!Warning]
Currently, Rush only supports Unix-like systems (Linux, macOS).
Windows is not supported yet, and we do not have any active developers testing on Windows.
Contributions to improve Windows support are welcome!
---

## Documentation
- For the complete example, and an example that builds Ninja itself using rush, go to [`examples/`](https://github.com/rakivo/rush/tree/master/examples) directory.
- Ninja syntax compatibility: mostly supported, currently without generators and some other minor differences.

## Contributing

Issues and PRs are welcome! See [`CONTRIBUTING.md`](https://github.com/rakivo/rush/blob/master/CONTRIBUTING.md) for details.

## License

Dual-licensed under Apache-2.0 or MIT.
