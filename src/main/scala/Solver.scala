package com.cormontia.adventOfCode2024

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
        val solutionPart1 = solvePart1(lines)
        println(s"The solution to part 1 is $solutionPart1.")
        val solutionPart2 = solvePart2(lines)
        println(s"The solution to part 2 is is $solutionPart2.")
      case Failure(_) => println("Failed to read input file!")

}
