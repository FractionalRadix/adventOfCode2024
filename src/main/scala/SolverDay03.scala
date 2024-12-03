package com.cormontia.adventOfCode2024

import scala.io.Source
import scala.util.matching.Regex

class SolverDay03 {
  def solve(): Unit = {
    val instructions = readInstructions("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay03.txt")
    val pairs = integerPairs(instructions)

    val sum = pairs.map( pair => pair._1 * pair._2 ).sum
    println(s"The sum of the multiplications is $sum.")

  }

  private def integerPairs(instructions: Iterator[String]) =
    instructions
      .map( str => str.drop(4).dropRight(1) )   // Drop the "mul(" and ")".   Iterator[String]
      .map( str => str.split(","))              // Split into 2 parts.        Iterator[Array[String]]
      .map( arr => parseIntPair(arr) )

  /**
   * Given an array of Strings, each of which represents an integer, return the first and second integer.
   * @param arr An array of integers represented as Strings. The array should have at least 2 elements.
   * @return The first two elements of the array as integers.
   */
  private def parseIntPair(arr: Array[String]): (Int,Int) =
    val fst = arr(0).toInt
    val snd = arr(1).toInt
    (fst,snd)

  private def readInstructions(filename: String): Iterator[String] =
    val input = readInput(filename)
    val pattern: Regex = "mul\\([0-9]+,[0-9]+\\)".r
    val instructions = pattern
      .findAllMatchIn(input)
      .map(m => m.toString())
    instructions

  private def readInput(filename: String): String =
    val source = Source.fromFile(filename)
    val input = source.getLines().mkString
    source.close()
    input
}
