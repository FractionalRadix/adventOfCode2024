package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay07 {

  def parseDay07Input(filename: String): List[(Long, Array[Long])] = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    val equations = lines.map( line => parseLine(line) )
    equations
  }

  private def parseLine(line: String): (Long, Array[Long]) =
    val splitStr = line.split(" ")
    val testValue = splitStr(0).dropRight(1).toLong
    val numbers = splitStr.drop(1).map( str => str.toLong )
    (testValue, numbers)

  // Optimizations:
  // * If the product of all values is lower than the test value, it's an automatic fail.
  // * If the sum of all values is higher than the test value, it's an automatic fail.

  def solvePart1(equations: List[(Long, Array[Long])]): Long = {
    var totalCalibration: Long = 0
    for equation <- equations do
      val nrOfOperands = equation._2.length
      val nrOfOperators = nrOfOperands - 1
      val possibleCombinations = List.fill(nrOfOperators)(2).product
      var found = false
      for combination <- 0 until possibleCombinations if !found do
        val binary = binaryString(combination, nrOfOperands)
        val result = calculateUsingBinary(binary, equation._2)
        if equation._1 == result then
          totalCalibration += equation._1
          found = true
    totalCalibration
  }

  /**
   * Given a number, returns its binary representation, with zeroes padded to the front to obtain the required length.
   * @param n The number to convert.
   * @param length The total length of the resulting string, including leading zeroes.
   * @return The binary representation of n, with zeroes added to the front to reach the required length.
   */
  private def binaryString(n: Int, length: Int): String =
    val binary = n.toBinaryString
    val leader = "0".repeat(length - binary.length)
    leader ++ binary

  /**
   * Given a binary string (e.g. "00110") and a number of integers, calculate the value you'd get if every 0 was
   * addition and every 1 multiplication, without caring about predecessors.
   * For example, "010" (3,2,6,4) would result in 3 + 2 * 6 + 4 .
   * @param binStr A string consisting only of the characters 0 and 1.
   * @param operands A number of integers, one more number than binStr is long.
   * @return The value you'd get if every 0 represented "+" and every 1 represented "*", and you put these between
   *         the operands. Disregarding priority.
   */
  private def calculateUsingBinary(binStr: String, operands: Array[Long]): Long =
    var result: Long = 0
    for i <- 0 until binStr.length do
      val ch = binStr.charAt(i)
      ch match
        case '0' => result = result + operands(i)
        case '1' => result = result * operands(i)
    result

  def solvePart2(equations: List[(Long, Array[Long])]): Long = {
    var sum: Long = 0
    for equation <- equations do
      if eval(equation._1, 0, equation._2) then
        //println(s"Found: ${equation._1}")
        sum = sum + equation._1
    sum
  }

  private def concat(fst: Long, snd: Long): Long =
    (fst.toString ++ snd.toString).toLong

  private def eval(target: Long, total: Long, numbers: Array[Long]): Boolean =
    if numbers.length == 1 then
      // Last number, end of the recursion.
      if total + numbers(0) == target then
        true
      else if total * numbers(0) == target then
        true
      else if concat(total, numbers(0)) == target then
        true
      else
        false
    else
      // Optimize: note that the numbers never grow smaller,
      // so as soon as the running total exceeds the target you may skip that branch.
      if total > target then
        false
      else
        // Three-way recursion
        val candidate1 = total + numbers(0)
        val result = eval(target, candidate1, numbers.tail)
        if result then
          true
        else
          val candidate2 = total * numbers(0)
          val result = eval(target, candidate2, numbers.tail)
          if result then
            true
          else
            val candidate3 = concat(total, numbers(0))
            val result = eval(target, candidate3, numbers.tail)
            if result then
              true
            else
              false

}
