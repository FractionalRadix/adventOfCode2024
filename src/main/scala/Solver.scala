package com.cormontia.adventOfCode2024

import java.time.temporal.ChronoUnit
import java.time.{LocalDateTime, LocalTime, Period}
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

abstract class Solver {
  def readInput(filename: String): Try[List[String]] =
    Using(Source.fromFile(filename)) { source =>
      val source = Source.fromFile(filename)
      source.getLines.toList
    }

  def solvePart1(lines: List[String]): Long

  def solvePart2(lines: List[String]): Long

  def solve(filename: String): Unit =
    val input = readInput(filename)
    input match
      case Success(lines) =>
        val startTimePart1 = LocalTime.now()
        val solutionPart1 = solvePart1(lines)
        val endTimePart1 = LocalTime.now()
        val seconds1 = ChronoUnit.SECONDS.between(startTimePart1, endTimePart1)
        println(s"The solution to part 1 is $solutionPart1 ($seconds1 seconds).")

        val startTimePart2 = LocalTime.now()
        val solutionPart2 = solvePart2(lines)
        val endTimePart2 = LocalTime.now()
        val seconds2 = ChronoUnit.SECONDS.between(startTimePart2, endTimePart2)
        println(s"The solution to part 2 is is $solutionPart2 ($seconds2 seconds).")
      case Failure(_) => println("Failed to read input file!")

}
