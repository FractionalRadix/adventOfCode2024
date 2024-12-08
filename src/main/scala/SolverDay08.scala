package com.cormontia.adventOfCode2024

import scala.io.Source

case class Coor(row: Int, col: Int)

class SolverDay08 {
  def parseDay08Input(filename: String): List[String] =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    lines

  def solvePart1(lines: List[String]): Long =
    val block = CharacterBlock2(lines)
    val testBlock = CharacterBlock2(block)

    val pairs = findAntennaePairs(block)

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

  private def findAntennaePairs(block: CharacterBlock2): List[((Int, Int), (Int, Int))] =
    var allPairs: List[((Int, Int), (Int, Int))] = Nil
    // First, let's find all the different frequencies.
    val frequencies = block.findDistinct() - '.'
    // Next, for every frequency, find all pairs.
    for frequency <- frequencies do
      val antennae = block.findCoordinatesOf(frequency)
      val pairs = makePairs(antennae.toList)
      allPairs = allPairs ++ pairs
    allPairs

  private def findAntennaePairs2(block: CharacterBlock2): List[(Coor, Coor)] =
    var allPairs: List[(Coor, Coor)] = Nil
    // First, let's find all the different frequencies.
    val frequencies = block.findDistinct() - '.'
    // Next, for every frequency, find all pairs.
    for frequency <- frequencies do
      val antennae = block.findCoordinatesOf(frequency).toList.map( elt => pairToCoor(elt) )
      val pairs = makePairs(antennae)
      allPairs = allPairs ++ pairs
    allPairs

  private def pairToCoor(pair: (Int, Int)): Coor = Coor(pair._1, pair._2)

  def solvePart2(lines: List[String]): Long =
    val block = CharacterBlock2(lines)
    val testBlock = CharacterBlock2(block)
    val pairs = findAntennaePairs2(block)
    for pair <- pairs do
      val antenna1 = pair._1
      val antenna2 = pair._2
      val rowDiff = antenna1.row - antenna2.row
      val colDiff = antenna1.col - antenna2.col
      println(s"Pair: $antenna1, $antenna2")

    0 //TODO!~
}
