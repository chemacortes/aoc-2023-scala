datafile := "data.txt"
datatraining := "data-training.txt"

format := "real %E, user %U, sys %S"
native_opts := "--native --native-mode release-full --native-gc none"


[private]
default:
  @just --list --unsorted

[private]
@_run day part:
  time -f "\nTiming: {{format}}" \
    scala-cli run . -M {{day}}.{{part}}

[private]
@ensure day part:
  {{ if day =~ 'day[01]\d$' {""} else {error("only admits day01 to day25")} }}
  {{ if part =~ 'part[12][a-z]?' {""} else {error("only runs part1 or part2")} }}

# running one quiz
@run day part: (ensure day part) && (_run day part)
  echo "running {{day}}.{{part}}\n"


[private]
@package day part opts="":
  echo "building {{day}}.{{part}}\n"
  scala-cli --power package -f . -M {{day}}.{{part}} -o {{day}}.{{part}} -f {{opts}}

# running a packed executable (fast execution)
@fast day part: (ensure day part) && (package day part) (solution day part)
  echo "running fast {{day}}.{{part}}\n"

# running a full optimized quiz (scala-native)
@native day part: (ensure day part) && (package day part native_opts) (solution day part)
  echo "native version {{day}}.{{part}}\n"

# running again and timing the optimized quiz
@solution day part: (ensure day part)
  echo "\nrunning {{day}}.{{part}}\n"
  (export SCALANATIVE_MIN_SIZE=1g && time -f "\nTiming: {{format}}" ./{{day}}.{{part}})

# clean scala-build files of a quiz
@clean:
  echo "cleaning...\n"
  scala-cli clean .

# clean all scala-build files and executables (day01.part1,...)
@clean-all: (clean)
  find . -maxdepth 2 -name "day??.part[12]" -delete
