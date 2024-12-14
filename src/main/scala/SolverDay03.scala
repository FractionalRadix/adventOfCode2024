package com.cormontia.adventOfCode2024

import scala.io.Source
import scala.util.matching.Regex

class SolverDay03 extends Solver {

  override def solvePart1(lines: List[String]): Long = 
    val instructions = readInstructionsPart1(lines.mkString)
    val pairs = integerPairs(instructions)
    pairs.map(pair => pair._1 * pair._2).sum
  

  override def solvePart2(lines: List[String]): Long = {
    val instructions2 = readInstructionsPart2(lines.mkString)
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
}
