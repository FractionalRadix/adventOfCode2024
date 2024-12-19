package com.cormontia.adventOfCode2024

import scala.reflect.ClassTag
import scala.util.boundary
import scala.util.boundary.break

class SolverDay19 extends Solver {
  override def solvePart1(lines: List[String]): String = {
    val patterns = lines.head.split(",").map( str => str.trim )
    val designs = lines.drop(2)

    val possible = designs.map(design => isPossible(design, patterns))
    val count = possible.count(t => t)
    count.toString
  }

  override def solvePart2(lines: List[String]): String = {
    val patterns = lines.head.split(",").map(str => str.trim)
    val designs = lines.drop(2)

    var count = 0L
    for design <- designs do
      val countForDesign = countPossibilities2(design, patterns)
      count = count + countForDesign

    count.toString
  }

  private val cache = scala.collection.mutable.Map[String, Long]()
  private def countPossibilities2(design: String, patterns: Array[String]): Long = {
    val cur = cache.get(design)
    if cur.isDefined then
      return cur.head

    if design.isEmpty then
      1L
    else
      var sum = 0L
      for pattern <- patterns do
        if design.startsWith(pattern) then
          val possibilities = countPossibilities2(design.drop(pattern.length), patterns)
          cache(design.drop(pattern.length)) = possibilities
          sum = sum + possibilities
      sum
  }


  //TODO?~  If this is unit-tested to satisfaction, move to Util.
  /**
   * Splits the given string on a delimiter, but keeps the occurrences of the delimiter in the result.
   * For example, splitAndKeepDelimiter("abbaabba", "bb") yields ["a", "bb", "aa", "bb", "a"].
   * The delimiter will also be kept if the string ends with it.
   * For example, splitAndKeepDelimiter("abbaabbabb", "bb") yields ["a", "bb", "aa", "bb", "a", "bb"].
   * @param design The string to split.
   * @param splitter The splitter.
   * @return A list representation of the string, cut by the splitter.
   */
  private def splitAndKeepDelimiter(design: String, splitter: String): List[String] = {
    var i = 0
    var startOfSubstring = 0
    var res = List[String]()
    while i < design.length do
      //TODO?+ An optimization is possible when i + splitter.length > design.length.
      val tail = design.drop(i)
      if tail.startsWith(splitter) then
        val substring = design.slice(startOfSubstring, i)
        res = splitter :: substring :: res
        i = i + splitter.length
        startOfSubstring = i
      else
        i = i + 1

    val remainder = design.slice(startOfSubstring, design.length)
    res = remainder :: res

    res.filter(str => str.nonEmpty).reverse
  }

  //TODO!~ Make this a proper unit tst, once I've got that set up for Scala in IntelliJ.
  private def testSplitAndKeepDelimiter(): Unit = {
    val test0 = splitAndKeepDelimiter("HelloWorld", "oWo")
    println(s"$test0 should be List(Hell, oWo, rld).")
    val test1 = splitAndKeepDelimiter("gbbr", "bwu")
    println(s"$test1 should be List(gbbr).")
    val test2 = splitAndKeepDelimiter("abbaabba", "bb")
    println(s"$test2 should be List(a, bb, aa, bb, a)")
    val test3 = splitAndKeepDelimiter("abbaabbabb", "bb")
    println(s"$test3 should be List(a, bb, aa, bb, a, bb)")
  }

  private def isPossible(design: String, patterns: Array[String]): Boolean = {
    //println(s"Check: $design ${patterns.mkString(",")}")

    if design.isEmpty then
      return true

    var res = false
    boundary {
      for pattern <- patterns do
        if design.startsWith(pattern) then
          if isPossible(design.drop(pattern.length), patterns) then
            res = true
            break()
          end if
    }
    res
  }

  /**
   * Naive, brute-force solution to find all combinations.
   * Used as the basis for a memoized solution.
   * @param design The design to replicate.
   * @param patterns The patterns that can be used to replicate the design.
   * @return Then number of ways in which the given design can be replicated with the given patterns.
   */
  private def countPossibilities(design: String, patterns: Array[String]): Long = {
    if design.isEmpty then
      1L
    else
      var sum = 0L
      for pattern <- patterns do
        if design.startsWith(pattern) then
          sum = sum + countPossibilities(design.drop(pattern.length), patterns)
      sum
  }
}

