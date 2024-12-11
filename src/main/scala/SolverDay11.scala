package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay11 {
  def parseDay11Input(filename: String): List[Int] =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    val line = lines.head
    val numbers1 = line.split("\\s")
    val numbers2 = numbers1.map( str => str.toInt )
    numbers2.toList


  def solvePart1(stones: List[Int]): Long =
    // First let's do a quick check
    printList(stones)
    println()
    val newStones = blink1(stones)
    printList(newStones)
    println()
    0 //TODO!~


  def printList(stones: List[Int]): Unit =
    for stone <- stones do
      print(s" [$stone]")

  /**
   * Naive solution to part 1: blink once.
   * After we get this working properly we'll add some caching.
   * @param stones The list of numbers (engravings) on the stones.
   * @return The new series of stones after blinking once.
   */
  private def blink1(stones: List[Int]): List[Int] =
    var result: List[Int] = Nil
    for stone <- stones do
      if stone == 0 then
        result = 1 :: result
      else if stone.toString.length % 2 == 0 then
        val str = stone.toString
        val len = str.length
        val n = len / 2
        val newStone1 = str.drop(n).toInt
        val newStone2 = str.take(n).toInt
        result = newStone1 :: newStone2 :: result
      else
        result = (stone * 2024) :: result
    result.reverse

  def solvePart2(input: List[Int]): Long =
    0 //TODO!~

}
