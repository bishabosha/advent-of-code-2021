import day01.{part1, part2}

import advent.io.IO
import advent.console.printLine
import advent.challenge.{runChallenge, readLinesAsInt}

val day_01 = runChallenge("day01", readLinesAsInt)

val all =
  day_01("part1", part1) *>
  day_01("part2", part2)

@main def runAll = IO.unsafeRunSync(all)