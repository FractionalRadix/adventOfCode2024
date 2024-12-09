package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay09 {
  def parseDay09input(filename: String): String =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    lines.head

  def quickConvert(ch: Character): Int =
    ch - '0'

  def solvePart1(input: String) =
    println(s"Input: $input")

    // NOTE: Since file numbers are going to be larger than 9, we'll need a Map, not a String.
    //val input1 = "12345"
    val input1 = input
    // First, let's process the thing...
    var file = true
    var fileNum = 0
    for i <- 0 until input1.length do
      val digit = quickConvert(input1.charAt(i))
      val str = if file then
        fileNum = fileNum + 1
        (fileNum-1).toString * digit
      else
        "." * digit
      print(str)
      file = !file
    println

    0 //TODO!~
}
