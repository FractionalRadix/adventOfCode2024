package com.cormontia.adventOfCode2024

import scala.util.matching.Regex

class SolverDay04 extends Solver {

  override def solvePart1(lines: List[String]): String = {
    val block = CharacterBlock(lines)

    val horizontalMatches = countMatches(lines)
    val verticalMatches = countMatches(block.getColumns)
    val forwardDiagonals = block.getForwardDiagonals
    val forwardDiagonalMatches = countMatches(forwardDiagonals)
    val backwardDiagonals = block.getBackwardsDiagonals
    val backwardDiagonalMatches = countMatches(backwardDiagonals)

    val allMatches = horizontalMatches + verticalMatches + forwardDiagonalMatches + backwardDiagonalMatches
    allMatches.toString
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
  
  override def solvePart2(lines: List[String]): String = {
    val block = CharacterBlock(lines)

    // First, find every 'A' in the block.
    // Then, for every 'A', see if it is surrounded diagonally by opposing 'M' and 'S' characters.
    val nrOfRows = block.getNrOfRows
    val nrOfCols = block.getNrOfColumns

    var count = 0
    for rowIdx <- Range(0, nrOfRows) do
      for colIdx <- Range(0, nrOfCols) do
        val center = block.getCharAt(rowIdx, colIdx)
        if center.contains('A') then
          if surroundedByMS(block, rowIdx, colIdx) then
            count = count + 1
    count.toString
  }

  private def surroundedByMS(block: CharacterBlock, rowIdx: Int, colIdx: Int): Boolean = {
    val aboveLeft = block.getCharAt(rowIdx - 1, colIdx - 1)
    val aboveRight = block.getCharAt(rowIdx - 1, colIdx + 1)
    val belowLeft = block.getCharAt(rowIdx + 1, colIdx - 1)
    val belowRight = block.getCharAt(rowIdx + 1, colIdx + 1)

    // First, check if aboveLeft/belowRight form "SAM" or "MAS"
    val condition1 = aboveLeft.contains('S') && belowRight.contains('M')
    val condition2 = aboveLeft.contains('M') && belowRight.contains('S')
    val diagonal1 = condition1 || condition2

    // Next, check if belowLeft/aboveRight form "SAM" or "MAS"
    val condition3 = belowLeft.contains('S') && aboveRight.contains('M')
    val condition4 = belowLeft.contains('M') && aboveRight.contains('S')
    val diagonal2 = condition3 || condition4

    diagonal1 && diagonal2
  }
}
