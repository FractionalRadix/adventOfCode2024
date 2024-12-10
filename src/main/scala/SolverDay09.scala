package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay09 {
  def parseDay09Input(filename: String): String =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    lines.head

  private def quickConvert(ch: Character): Int =
    ch - '0'

  def solvePart1(input: String): Long =
    //TODO?~ It might be more elegant to use "None" for empty parts of the disk, rather than the sentinel value -1.
    // NOTE: Since file numbers are going to be larger than 9, we'll need a Map, not a String.
    val map2 = buildMap(input)
    moveBlocks(map2)
    calcChecksum(map2)

  def solvePart2(input: String): Long =
    val map = buildMap(input)
    printMapAsString(map)
    // First, let's convert this to a more efficient data structure:
    // a set of (fileNr, first position, length).
    val files = determineFiles(map).reverse

    println("Stats:")
    val smallestFileSize = files.filter { f => f.fileNr != - 1 }.map { f => f.length }.min
    val largestFileSize = files.filter { f => f.fileNr != -1 }.map { f => f.length }.max
    println(s"Smallest file size: $smallestFileSize")
    println(s"Largest file size: $largestFileSize")
    val smallestGapSize = files.filter { f => f.fileNr == - 1 }.map { f => f.length }.min
    val largestGapSize = files.filter { f => f.fileNr == -1 }.map { f => f.length }.max
    println(s"Smallest gap size: $smallestGapSize")
    println(s"Largest gap size: $largestGapSize")

    // Algorithm:
    // Find the file with the highest unused file ID.
    // Find the earliest gap in which it will fit.
    // Change its position to the start position of that gap.
    // Adjust the start position and length of the gap accordingly: it is now a (possibly much) smaller gap, might even have length 0.
    // Do this until ?? the file Nr you want to move, is one that you have moved already ??
    var set = scala.collection.mutable.Set[File]()
    set = set ++ files
    var movedFiles = scala.collection.mutable.Set[File]()
    val usedFileIDs = scala.collection.mutable.Set[Int]()
    val movedFileIDs = scala.collection.mutable.Set[Int]() //TODO?- For testing.
    var running = true
    while running do
      val candidateFiles = set.filter(f => f.fileNr != -1).filter( f => !usedFileIDs.contains(f.fileNr) )
      if candidateFiles.isEmpty then
        running = false
      else
        val currentFile = candidateFiles.maxBy( f => f.fileNr )
        //println(currentFile)
        val fittingGaps = set.filter( f => f.fileNr == -1 && f.length >= currentFile.length )
        if fittingGaps.nonEmpty then
          val earliestFittingGap = fittingGaps.minBy( f => f.startPos )
          val newFile = File(currentFile.fileNr, earliestFittingGap.startPos, currentFile.length)
          val newGap = File(-1, earliestFittingGap.startPos + currentFile.length, earliestFittingGap.length - currentFile.length)
          if (earliestFittingGap.startPos < currentFile.startPos)
            if movedFileIDs.contains(currentFile.fileNr) then
              println("WARNING! Moving already moved file!!")
            else
              print(s"${currentFile.fileNr} ")
            val gapLeftByMovedFile = File(-1, currentFile.startPos, currentFile.length)
            set.remove(earliestFittingGap)
            set.remove(currentFile)
            set.add(newFile)
            set.add(newGap)
            if (gapLeftByMovedFile.length > 0) then
              set.add(gapLeftByMovedFile)
            movedFileIDs.add(currentFile.fileNr)
          val intermediateMap = fileSetToMap(set)
          //printMapAsString(intermediateMap)
        usedFileIDs.add(currentFile.fileNr)

    val newMap = fileSetToMap(set)
    //printMapAsString(newMap)
    calcChecksum(newMap)


  private def fileSetToMap(set: scala.collection.mutable.Set[File]): scala.collection.mutable.Map[Int, Int] =
    val list = set.toList.sortWith(_.startPos < _.startPos)
    val result = scala.collection.mutable.Map[Int, Int]()
    for file <- list do
      // Note that a file could also be a gap here!
      for i <- 0 until file.length do
        result(file.startPos + i) = file.fileNr
    result

  // PRE: File list is sorted by start position, ascending.
  private def printFileList(files: List[File]): Unit =
    println
    for file <- files do
      for block <- 0 until file.length do
        if file.fileNr == -1 then
          print(".")
        else
          print(file.fileNr)
    println
    //TODO!+

  private case class File(fileNr: Int, startPos: Int, length: Int)

  private def determineFiles(map: scala.collection.mutable.Map[Int, Int]): List[File] =
    var files = List[File]()
    var startPos = 0
    var currentFileNr = map(startPos)
    var length = 0
    for i <- map.keys do
      val foundFileNr = map.get(i).head //TODO!~  Might be None, or -1 ...
      if foundFileNr == currentFileNr then
        length = length + 1
      else
        val file = File(currentFileNr, startPos, length)
        files = file :: files
        currentFileNr = foundFileNr
        startPos = i
        length = 1 // You've already found the first element, so the length is 1.
    // Add the last one...
    val lastFile = File(currentFileNr, startPos, length)
    files = lastFile :: files
    files


  private def calcChecksum(map: scala.collection.mutable.Map[Int, Int]): Long =
    var sum: Long = 0
    var pos = 0
    for key <- map.keys do
      val value = map.get(key)
      value match
        case None => println("Oops! For key $key, pos $pos, sum $sum")
        case Some(fileNr) =>
          if fileNr != -1 then
            sum = sum + pos.toLong * fileNr.toLong
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

  private def mapToArray(map: scala.collection.mutable.Map[Int, Int]): Array[Option[Int]] =
    val maxKey = map.keys.max()
    val array = Array.ofDim[Option[Int]](maxKey + 1)
    for i <- 0 to maxKey do
      val value = map.get(i)
      array(i) = value match
        case Some(-1) => None
        case Some(n) => Some(n)
        case None => None //TODO?+ Error message?
    array

  private def printArrayAsString(arr: Array[Option[Int]]): Unit =
    for i <- arr do
      i match
        case Some(n) => print(n)
        case None => print(".")

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

