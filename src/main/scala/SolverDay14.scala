package com.cormontia.adventOfCode2024

import scala.io.Source
import scala.util.{Try, Using}

class SolverDay14 extends Solver {
  override def parseInput(filename: String): Try[List[String]] =
    Using (Source.fromFile(filename)) { source =>
      source.getLines.toList
    }

  override def solvePart1(lines: List[String]): Long =
    0 //TODO!~

  override def solvePart2(lines: List[String]): Long =
    0 //TODO!~
}
