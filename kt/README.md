# [Advent of Code 2023](https://adventofcode.com/2023)
### my answers in [Kotlin](https://www.kotlinlang.org/) ![Kotlin CI](https://github.com/ephemient/aoc2023/workflows/Kotlin%20CI/badge.svg)

This project builds with [Gradle](https://gradle.org/).

Run the test suite:

```sh
./gradlew :aoc2023-lib:allTests
```

Run [kotlinx.benchmark](https://github.com/Kotlin/kotlinx-benchmark) ([JMH](https://openjdk.java.net/projects/code-tools/jmh/)) benchmarks:

```sh
./gradlew :aoc2023-exe:benchmark
```

Print solutions for the inputs provided in local data files:

```sh
./gradlew :aoc2023-exe:jvmRun :aoc2023-exe:runReleaseExecutable{LinuxX64,Macos{X64,Arm64}} :aoc2023-exe:jsNodeProductionRun
```

Run all checks, including [Detekt](https://detekt.github.io/) static code analysis and [ktlint](https://ktlint.github.io/) formatter:

```sh
./gradlew check
```

Check for newer versions of dependencies:

```sh
./gradlew :dependencyUpdates
```
