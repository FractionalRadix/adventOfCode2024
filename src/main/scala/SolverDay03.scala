package com.cormontia.adventOfCode2024

import scala.io.Source
import scala.util.matching.Regex

class SolverDay03 {
  def solve(): Unit = {
    val input = readInput("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay03.txt")
    val sum = solvePart1(input)
    println(s"The sum of the multiplications is $sum.")        // 173529487
    val newSum = solvePart2(input)
    println(s"The sum of enabled multiplications is $newSum.") //  99532691
  }

  private def solvePart1(input: String): Int =
    val instructions = readInstructionsPart1(input)
    val pairs = integerPairs(instructions)
    pairs.map(pair => pair._1 * pair._2).sum

  private def solvePart2(input: String): Int =
    val instructions2 = readInstructionsPart2(input)
    var enabled = true
    var sum = 0
    for instruction <- instructions2 do
      if instruction.startsWith("don't") then
        enabled = false
      else if instruction.startsWith("do") then
        enabled = true
      else if enabled then // instruction is of the form mul(number,number)
        val arr = instruction.drop(4).dropRight(1).split(",")
        val pair = parseIntPair(arr)
        sum = sum + pair._1 * pair._2
    sum

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

  private def readInstructionsPart1(input: String): Iterator[String] =
    val pattern: Regex = "mul\\([0-9]+,[0-9]+\\)".r
    val instructions = pattern
      .findAllMatchIn(input)
      .map(m => m.toString())
    instructions

  private def readInstructionsPart2(input: String): Iterator[String] =
    val pattern: Regex = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)".r
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
