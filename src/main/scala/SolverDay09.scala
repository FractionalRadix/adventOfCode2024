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
    val smallestFileSize = files.filter { f => f.fileNr.isDefined }.map { f => f.length }.min
    val largestFileSize = files.filter { f => f.fileNr.isDefined }.map { f => f.length }.max
    println(s"Smallest file size: $smallestFileSize")
    println(s"Largest file size: $largestFileSize")
    val smallestGapSize = files.filter { f => f.fileNr.isEmpty }.map { f => f.length }.min
    val largestGapSize = files.filter { f => f.fileNr.isEmpty }.map { f => f.length }.max
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
      val candidateFiles = set.filter(f => f.fileNr.isDefined).filter(f => !usedFileIDs.contains(f.fileNr.head) )
      if candidateFiles.isEmpty then
        running = false
      else
        val currentFile = candidateFiles.maxBy( f => f.fileNr )
        //println(currentFile)
        val fittingGaps = set.filter( f => f.fileNr.isEmpty && f.length >= currentFile.length )
        if fittingGaps.nonEmpty then
          val earliestFittingGap = fittingGaps.minBy( f => f.startPos )
          val newFile = File(currentFile.fileNr, earliestFittingGap.startPos, currentFile.length)
          val newGap = File(None, earliestFittingGap.startPos + currentFile.length, earliestFittingGap.length - currentFile.length)
          if (earliestFittingGap.startPos < currentFile.startPos)
            //if movedFileIDs.contains(currentFile.fileNr) then
            //  println("WARNING! Moving already moved file!!")
            //else
            //  print(s"${currentFile.fileNr} ")
            val gapLeftByMovedFile = File(None, currentFile.startPos, currentFile.length)
            set.remove(earliestFittingGap)
            set.remove(currentFile)
            set.add(newFile)
            set.add(newGap)
            if gapLeftByMovedFile.length > 0 then
              set.add(gapLeftByMovedFile)
            movedFileIDs.add(currentFile.fileNr.head)
          //val intermediateMap = fileSetToMap(set)
          //print(s"${calcChecksum(intermediateMap)} ")
          //printMapAsString(intermediateMap)
          //printFileList2(set.toSet)
          //print(s"${calcChecksum2(set.toSet)} ")
        usedFileIDs.add(currentFile.fileNr.head)

        if !allBlocksAccountedFor(set.toSet) then
          println("NOT ALL BLOCKS ACCOUNTED FOR!")

    val newMap = fileSetToMap(set)
    //printMapAsString(newMap)
    //val checksum1 = calcChecksum(newMap)
    //println(s"Checksum #1: $checksum1.")
    val checksum2 = calcChecksum2(set.toSet)
    println(s"Checksum #2: $checksum2.")
    val checksum3 = calcChecksum3(newMap.toMap) //TODO!~ Yields the wrong result even on the sample input! 2868 iso 2858 ...
    println(s"Checksum #3: $checksum3.")

    checksum2
    //TODO!~ Look at those checksum calculators.
    // Three different checksum calculators provide the same value for the sample input...
    // But they provide thee DIFFERENT answers for the actual output!


  private def calcChecksum2(set: Set[File]): Long =
    var sum: Long = 0
    for file <- set if file.fileNr.isDefined do
      val len = file.fileNr
      for i <- 0 until file.length do
        sum = sum + (file.startPos + i) * file.fileNr.head
    sum

  private def calcChecksum3(map: Map[Int, Option[Int]]): Long =
    var sum: Long = 0
    var pos = 0
    for key <- map.keys do
      val value = map.get(key)
      value match
        case None => println("Oops! For key $key, pos $pos, sum $sum")
        case Some(fileNr) =>
          if fileNr.isDefined then
            sum = sum + pos.toLong * fileNr.head.toLong
      pos = pos + 1
    sum

  /**
   * Given the list of files and gaps, verify that every block is either in a file or in a gap.
   */
  private def allBlocksAccountedFor(set: Set[File]): Boolean =
    // Verify that, for every entry (_, startPos, length), the next entry is (_, startPos + length, _)
    var list = List[File]()
    list = list ++ set
    list = list.sortWith(_.startPos > _.startPos)
    val pairs = list.zip(list.tail)
    for pair <- pairs do
      val expectedNextPos = pair._1.startPos + pair._1.length
      val actualNextPos = pair._2.startPos
      if actualNextPos != expectedNextPos then
        false
    true


  private def fileSetToMap(set: scala.collection.mutable.Set[File]): scala.collection.mutable.Map[Int, Option[Int]] =
    val list = set.toList.sortWith(_.startPos < _.startPos)
    val result = scala.collection.mutable.Map[Int, Option[Int]]()
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
        if file.fileNr.isEmpty then
          print(".")
        else
          print(file.fileNr)
    println
    //TODO!+

  private def printFileList2(files: Set[File]): Unit =
    println
    val fileWithHighestNr = files.maxBy(f => f.startPos)
    val highestBlock = fileWithHighestNr.startPos + fileWithHighestNr.length
    for i <- 0 to highestBlock do
      // Find all files on block i. There SHOULD be only one!
      // TESTCASE: i=8, file = File (_,5,4). This file occupies blocks 5,6,7,8 (5 + 4 - 1).
      //   file.startPos + file.length = 5 + 4 = 9.
      //   i < file.startPos + file.length because 8 < 9.
      //   If i==9, the condition would have failed as it should.
      val foundFiles = files.filter(f => f.startPos <= i && i < (f.startPos + f.length))
      if foundFiles.size > 1 then
        println(s"ERROR!! Block $i contains multiple files/gaps: ${foundFiles.mkString(",")}")
      else if foundFiles.size == 1 then // Note that we ignore the case that foundFiles.size == 0 ....
        val foundFile = foundFiles.head
        foundFile.fileNr match
          case None => print(".")
          case Some(fileNr) => print(fileNr)


  private case class File(fileNr: Option[Int], startPos: Int, length: Int)

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
        val n: Option[Int] = if currentFileNr == -1 then None else Some(currentFileNr)
        val file = File(n, startPos, length)
        files = file :: files
        currentFileNr = foundFileNr
        startPos = i
        length = 1 // You've already found the first element, so the length is 1.
    // Add the last one...
    val n: Option[Int] = if currentFileNr == -1 then None else Some(currentFileNr)
    val lastFile = File(n, startPos, length)
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

