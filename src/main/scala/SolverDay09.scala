package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay09 {
  def parseDay09input(filename: String): String =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    lines.head

  private def quickConvert(ch: Character): Int =
    ch - '0'

  def solvePart1(input: String): Int =
    println(s"Input: $input")

    //TODO?~ It might be more elegant to use "None" for empty parts of the disk, rather than the sentinel value -1.

    // NOTE: Since file numbers are going to be larger than 9, we'll need a Map, not a String.
    val map1 = buildMap("12345")
    printMapAsString(map1)
    println

    val map2 = buildMap(input)
    printMapAsString(map2)
    moveBlocks(map2)
    calcChecksum(map2)


  private def calcChecksum(map: scala.collection.mutable.Map[Int, Int]): Int =
    var sum = 0
    var pos = 0
    for key <- map.keys do
      val value = map.get(key)
      value match
        case None => println("Oops! For key $key, pos $pos, sum $sum")
        case Some(fileNr) =>
          if fileNr != -1 then
            sum = sum + pos * fileNr
            //println(s"fileId * block = $fileNr * $pos")
      pos = pos + 1
    sum

  private def moveBlocks(map: scala.collection.mutable.Map[Int, Int]): Unit =
    // Keep taking the last element and move it to the first free position.
    // If the first free position is AT or BEFORE that last element, stop.
    var going = true
    while (going)
      val lastPos = map.filter { elt => elt._2 != -1 }.keys.max
      val firstFreePos = map.filter { elt => elt._2 == -1 }.keys.min
      if firstFreePos < lastPos then
        map(firstFreePos) = map(lastPos)
        map(lastPos) = -1
        //printMapAsString(map)
      else
        going = false

  private def buildMap(input: String): scala.collection.mutable.Map[Int, Int] =
    val map = scala.collection.mutable.Map[Int, Int]()
    // First, let's process the thing...
    var file = true
    var fileNum = 0
    var position = 0
    for i <- 0 until input.length do
      val digit = quickConvert(input.charAt(i))
      if file then
        for j <- 0 until digit do
          map(position + j) = fileNum
        fileNum = fileNum + 1
      else
        for j <- 0 until digit do
          map(position + j) = -1
      position = position + digit
      file = !file
    map

  //TODO?~ Pas an IMMUTABLE version of the map?
  private def printMapAsString(map: scala.collection.mutable.Map[Int, Int]): Unit =
    for key <- map.keys do
        val value = map.get(key)
        value match
          case Some(-1) => print(".")
          //case Some(n) => print(s"($n)")
          case Some(n) => print(n)
          case None => print("!")
    println


}

