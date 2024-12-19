package com.cormontia.adventOfCode2024

class SolverDay19 extends Solver {
  override def solvePart1(lines: List[String]): String = {
    val patterns = lines.head.split(",").map( str => str.trim )
    val designs = lines.drop(2)

    println(s"Patterns: ${patterns.mkString("..")}")
    println(s"Designs: ${designs.mkString(" | ")}")

    val possible = designs.map(design => isPossible(design, patterns))
    val count = possible.count(t => t)
    count.toString
  }

  override def solvePart2(lines: List[String]): String = {
    val patterns = lines.head.split(",").map( str => str.trim )
    val designs = lines.drop(2)

    println(s"Patterns: ${patterns.mkString("..")}")
    println(s"Designs: ${designs.mkString(" | ")}")

    var count = 0L
    for design <- designs do
      println(s"Trying design: $design")
      val countForDesign = countPossibilities(design, patterns)
      println(s"...$countForDesign options.")
      count = count + countForDesign

    count.toString
  }

  private def isPossible(design: String, patterns: Array[String]): Boolean = {
    //println(s"Check: $design ${patterns.mkString(",")}")

    if design.isEmpty then
      return true

    for pattern <- patterns do
      if design.startsWith(pattern) then
        if isPossible(design.drop(pattern.length), patterns) then
          return true
        end if

    false
  }

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

