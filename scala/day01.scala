package day01

import advent.*
import advent.io.IO

def challenge(n: Int)(depths: List[Int]): Int =
  depths.zip(depths.drop(n)).count(_ < _)

val part1 = challenge(1)
val part2 = challenge(3)
