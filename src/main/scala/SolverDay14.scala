package com.cormontia.adventOfCode2024

import scala.io.Source
import scala.util.{Try, Using}

class SolverDay14 {
  def parseDay14Input(filename: String): Try[List[String]] =
    Using (Source.fromFile(filename)) { source =>
      source.getLines.toList
    }

  def solvePart1(lines: List[String]): Long =
    0 //TODO!~

  def solvePart2(lines: List[String]): Long =
    0 //TODO!~

}
