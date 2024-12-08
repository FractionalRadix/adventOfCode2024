package com.cormontia.adventOfCode2024

import scala.io.Source
import scala.math.abs

class SolverDay08 {
  def parseDay08Input(filename: String): List[String] =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    lines

  def solvePart1(lines: List[String]): Long =
    val block = CharacterBlock2(lines)
    val testBlock = CharacterBlock2(block)
    // First, let's find all the different frequencies.
    val frequencies = block.findDistinct() - '.'
    //println(s"${frequencies.mkString(",")}")
    // Next, for every frequency, find all pairs.
    for frequency <- frequencies do
      val antennae = block.findCoordinatesOf(frequency)
      //println(s"Antennae with frequency $frequency at locations ${antennae.mkString(",")}.")
      val pairs = makePairs(antennae.toList)
      //println(s"Antennae pairs: ${pairs.mkString(", ")}")
      // For every pair, determine the distance between them (horizontal and vertical)
      for pair <- pairs do
        val antenna1 = pair._1
        val antenna2 = pair._2
        val rowDiff = antenna1._1 - antenna2._1
        val colDiff = antenna1._2 - antenna2._2
        // Use these distances to determine the two anti-nodes that they create.
        // Finally, record all anti-nodes that are within bounds.
        val antiNode1_row = antenna1._1 + rowDiff
        val antiNode1_col = antenna1._2 + colDiff
        val antiNode2_row = antenna2._1 - rowDiff
        val antiNode2_col = antenna2._2 - colDiff
        val antiNode1 = (antiNode1_row, antiNode1_col)
        val antiNode2 = (antiNode2_row, antiNode2_col)
        if block.withinBounds(antiNode1) then
          testBlock.setCharAt(antiNode1_row, antiNode1_col, '#')
        if block.withinBounds(antiNode2) then
          testBlock.setCharAt(antiNode2_row, antiNode2_col, '#')
    val antiNodes = testBlock.findCoordinatesOf('#').toList.length
    antiNodes

  //TODO?~ Find a more efficient algorithm. We're likely to need this repeatedly in part 2.
  // PRE: List should have at least 2 elements.
  def makePairs[A](l: List[A]): List[(A,A)] =
    if l.length == 2 then
      List((l.head, l.tail.head))
    else
      l.tail.map( elt => (l.head, elt) ) ++ makePairs(l.tail)

  def solvePart2(lines: List[String]): Long =
    0 //TODO!~
}
