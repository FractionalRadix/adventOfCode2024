package com.cormontia.adventOfCode2024

import scala.util.boundary
import scala.util.boundary.break

class SolverDay09 extends Solver {

  def parseInput(lines: List[String]): String = lines.head

  private def quickConvert(ch: Character): Int =
    ch - '0'

  override def solvePart1(lines: List[String]): String =
    val input = parseInput(lines)
    //TODO?~ It might be more elegant to use "None" for empty parts of the disk, rather than the sentinel value -1.
    // NOTE: Since file numbers are going to be larger than 9, we'll need a Map, not a String.
    val map2 = buildMap(input)
    moveBlocks(map2)
    calcChecksum(map2).toString

  override def solvePart2(lines: List[String]): String =
    val input = parseInput(lines)
    files = stringToFileSet(input)
    if !allBlocksAccountedFor(files.toSet) then
      println("ERROR in the input!")

    // Algorithm:
    // Find the file with the highest unused file ID.
    // Find the earliest gap in which it will fit.
    // Change its position to the start position of that gap.
    // Adjust the start position and length of the gap accordingly: it is now a (possibly much) smaller gap, might even have length 0.
    // Do this until ?? the file Nr you want to move, is one that you have moved already ??
    var movedFiles = scala.collection.mutable.Set[File]()
    val usedFileIDs = scala.collection.mutable.Set[Int]()
    var running = true
    while running do
      val candidateFiles = files.filter(f => f.fileNr.isDefined).filter(f => !usedFileIDs.contains(f.fileNr.head))
      if candidateFiles.isEmpty then
        running = false
      else
        val currentFile = candidateFiles.maxBy(f => f.fileNr)
        val fittingGaps = files.filter(f => f.fileNr.isEmpty && f.length >= currentFile.length)
        if fittingGaps.nonEmpty then
          val earliestFittingGap = fittingGaps.minBy(f => f.startPos)
          if (earliestFittingGap.startPos < currentFile.startPos)
            moveFileToGap(currentFile, earliestFittingGap)
            if !allBlocksAccountedFor(files.toSet) then
              println("NOT ALL BLOCKS ACCOUNTED FOR!")
              running = false // Abnormal program termination... should just throw an Exception...
        usedFileIDs.add(currentFile.fileNr.head)

    val newMap = fileSetToMap(files)
    calcChecksum2(files.toSet).toString

  /**
   * Structure to contain all the "file" elements.
   * Note that, for convenience, gaps are also represented by `File` elements, whose fileId is None!
   */
  private var files = scala.collection.mutable.Set[File]()

  /**
   * Update the "files" structure. Move the File named `file` to the gap named `gap`.
   * Note that moving this file:
   * - leaves a new gap where the file came from, and
   * - makes the original gap smaller, or removes it entirely.
   * This method updates the "files" set.
   *
   * @param file The file to move.
   * @param gap The gap to move it to.
   */
  private def moveFileToGap(file: File, gap: File): Unit =
    val newFile = File(file.fileNr, gap.startPos, file.length)
    val newGap = File(None, file.startPos, file.length)
    val smallerGap = File(None, gap.startPos + file.length, gap.length - file.length)
    files.remove(file)
    files.remove(gap)
    files.add(newFile)
    files.add(newGap)
    if smallerGap.length > 0 then
      files.add(smallerGap)

  private def printFileListStatistics(files: Set[File]): Unit = {
    println("Stats:")
    val smallestFileSize = files.filter { f => f.fileNr.isDefined }.map { f => f.length }.min
    val largestFileSize = files.filter { f => f.fileNr.isDefined }.map { f => f.length }.max
    println(s"Smallest file size: $smallestFileSize")
    println(s"Largest file size: $largestFileSize")
    val smallestGapSize = files.filter { f => f.fileNr.isEmpty }.map { f => f.length }.min
    val largestGapSize = files.filter { f => f.fileNr.isEmpty }.map { f => f.length }.max
    println(s"Smallest gap size: $smallestGapSize")
    println(s"Largest gap size: $largestGapSize")
  }

  private def calcChecksum2(set: Set[File]): Long =
    var sum: Long = 0
    for file <- set if file.fileNr.isDefined do
      val len = file.fileNr
      for i <- 0 until file.length do
        sum = sum + (file.startPos + i) * file.fileNr.head
    sum

  /**
   * Given the list of files and gaps, verify that every block is either in a file or in a gap.
   * In other words, given a set of `File` elements, consider the lowest and highest position covered by them.
   * Every position in this range should be represented in one of the ranges defined by the `File` elements.
   * (Note that `File` in this case may also be a gap).
   * @param set The files and gaps on the disk.
   * @return `true` if and only if every block on the disk contains either a file or a gap.
   */
  private def allBlocksAccountedFor(set: Set[File]): Boolean =
    // Verify that, for every entry (_, startPos, length), the next entry is (_, startPos + length, _)
    var list = List[File]()
    list = list ++ set
    list = list.sortWith(_.startPos < _.startPos)
    var result = true
    val pairs = list.zip(list.tail)
    boundary {
      for pair <- pairs do
        val expectedNextPos = pair._1.startPos + pair._1.length
        val actualNextPos = pair._2.startPos
        if actualNextPos != expectedNextPos then
          println(s"Error pair: (${pair._1.fileNr}, ${pair._1.startPos}, ${pair._1.length}) - (${pair._2.fileNr}, ${pair._2.startPos}, ${pair._2.length})")
          result = false
          break()
    }
    result

  private def fileSetToMap(set: scala.collection.mutable.Set[File]): scala.collection.mutable.Map[Int, Option[Int]] =
    val list = set.toList.sortWith(_.startPos < _.startPos)
    val result = scala.collection.mutable.Map[Int, Option[Int]]()
    for file <- list do
      // Note that a file could also be a gap here!
      for i <- 0 until file.length do
        result(file.startPos + i) = file.fileNr
    result
  
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

  private case class File(fileNr: Option[Int], startPos: Int, length: Int) {
    override def toString = s"($fileNr, $startPos, $length)"
  }

  private def determineFiles(map: scala.collection.mutable.Map[Int, Int]): List[File] =
    var files = List[File]()
    var startPos = 0
    var currentFileNr = map(startPos)
    var length = 0
    var logging = false
    for i <- map.keys do

      if currentFileNr >= 6847 then
        logging = true
      if currentFileNr >= 6850 then
        logging = false

      // First error:
      // Error pair: (None, 65534, 6) - (Some(6848), 65541, 4)
      val foundFileNr = map.get(i).head //TODO!~  Might be None, or -1 ...
      if foundFileNr == currentFileNr then
        length = length + 1
        if logging then println(s"startPos==$startPos length=$length")
      else
        val n: Option[Int] = if currentFileNr == -1 then None else Some(currentFileNr)
        val file = File(n, startPos, length)
        if logging then println(s"New file: $file")
        files = file :: files
        currentFileNr = foundFileNr
        startPos = i
        length = 1 // You've already found the first element, so the length is 1.
        if logging then println(s"New starting values: startPos==$startPos length=$length")
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

  private def calcChecksum3(map: scala.collection.mutable.Map[Int, Option[Int]]): Long =
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

  /**
   * Parse the input string to a Set of File instances.
   * For convenience, a File instance can also be a gap here.
   * @param input A list of digits (in String form) representing the number of blocks each file or gap takes.
   * @return The set of File instances corresponding to the input list.
   */
  private def stringToFileSet(input: String): scala.collection.mutable.Set[File] =
    val result = scala.collection.mutable.Set[File]()
    var fileNr = 0
    var position = 0
    var isFile = true
    for ch <- input do
      val length = quickConvert(ch)
      if length == 0 then
        isFile = true
      else
        if isFile then
          val file = File(Some(fileNr), position, length)
          result.add(file)
          fileNr = fileNr + 1
          isFile = false
          position = position + length
        else
          val file = File(None, position, length)
          result.add(file)
          isFile = true
          position = position + length
    result
}