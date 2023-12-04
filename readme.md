# Advent of Code 2023 - scala


Need [scala-cli][] and (_optional_) [just][].

## Using _just_

If you has installed the rust toolchain, install _just_ with:

```bash
cargo install just
```

Available recipes:

```plain
run day part             # running one quiz
fast day part            # running a packed executable (fast execution)
native day part          # running a full optimized quiz (scala-native)
solution day part        # running again and timing the optimized quiz
clean                    # clean scala-build files of a quiz
clean-all                # clean all scala-build files and executables (day01.part1,...)
```

Examples:

```bash
just run day01 part1

just fast day01 part2

just native day01 part2

just solution day01 part2

just clean

just clean-all

```

## Execution without _just_

You can run the quiz without _just__:

```bash
scala-cli . -M day01.part2
```

You can package the solution (faster) with:

```bash
scala-cli --power package . -M day01.part2 -o day01.part2

./day01.part2
```

Fastest using _scala native_:

```bash
scala-cli --power package . -M day01.part2 -o day01.part2 --native
```

Look into `justfile` for more variants.

[scala-cli]: https://scala-cli.virtuslab.org "Scala CLI"
[just]: https://just.systems "Just"
