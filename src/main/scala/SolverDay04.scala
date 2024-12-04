package com.cormontia.adventOfCode2024

import scala.io.Source
import scala.util.matching.Regex

class SolverDay04 {
  def parseDay04Input(filename: String): List[String] = {
    val source = Source.fromFile(filename)
    source.getLines.toList
  }

  def solvePart1(lines: List[String]): Int = {
    val block = CharacterBlock(lines)
    block.print()

    val horizontalMatches = countMatches(lines)
    val verticalMatches = countMatches(block.getColumns)
    val forwardDiagonals = block.getForwardDiagonals
    val forwardDiagonalMatches = countMatches(forwardDiagonals)
    val backwardDiagonals = block.getBackwardsDiagonals
    val backwardDiagonalMatches = countMatches(backwardDiagonals)

    //minitest() // This would not have been necessary if IntelliJ and build.sbt could agree on how to import a Test framework...

    println(s"$horizontalMatches $verticalMatches $forwardDiagonalMatches $backwardDiagonalMatches")

    horizontalMatches + verticalMatches + forwardDiagonalMatches + backwardDiagonalMatches
  }

  /**
   * Count all matches of the word "XMAS" in the given list of strings, including the ones where it is spelled backwards.
   * @param lines A list of String.
   * @return The number of times the word "XMAS" (or its reverse, "SAMX") appears in the list of strings.
   */
  private def countMatches(lines: List[String]): Int = {
    val forwardMatches = findMatches(lines)
    val backward = lines.map { str => str.reverse }
    val backwardMatches = findMatches(backward)
    forwardMatches + backwardMatches
  }

  private def findMatches(lines: List[String]): Int = {
    val pattern: Regex = "XMAS".r
    var sum = 0
    for line <- lines do
      val occurrences = pattern
        .findAllMatchIn(line)
        .toList
        .length
      sum = sum + occurrences
    sum
  }
  
  def solvePart2(lines: List[String]): Int = {
    val block = CharacterBlock(lines)

    // First, find every 'A' in the block.
    // Then, for every 'A', see if it is surrounded diagonally by opposing 'M' and 'S' characters.


    0 //TODO!+
  }
}
