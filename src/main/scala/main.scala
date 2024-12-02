package com.cormontia.adventOfCode2024

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.abs

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  solveDay02()

def solveDay02(): Unit =
  val reports = parseDay02Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay02.txt")
  val nrOfSafeReports = solveDay02Part1(reports) // 639
  println(s"Number of safe reports: $nrOfSafeReports")
  val nrOfSafeReports2 = solveDay02Part2(reports) // 674
  println(s"Nr of safe reports if a single level can be removed: $nrOfSafeReports2")

def solveDay02Part2(reports: List[Array[Int]]): Int =
  //TODO?~ It would be more elegant to use a count(...) function.
  var count = 0
  reports.foreach( report =>
    var asc = isSafeAscending2(report)
    var desc = isSafeDescending2(report)
    if (!asc && !desc)
      asc = trySafeAscending(report)
      desc = trySafeDescending(report)
    if asc || desc then count = count + 1
  )
  count

def trySafeAscending(report: Array[Int]): Boolean =
  // Naive solution that works: for each level, try removing it.
  for i <- report.indices do
    val candidate = report.patch(i, Nil, 1)
    if isSafeAscending2(candidate) then
      // If the entire report is safe after removing a single level, then it is safe.
      return true //TODO!+ Lookup "boundary" and "boundary.break"  in scala.util
  false

def trySafeDescending(report: Array[Int]): Boolean =
  for i <- report.indices do
    val candidate = report.patch(i, Nil, 1)
    if isSafeDescending2(candidate) then
      // If the entire report is safe after removing a single level, then it is safe.
      return true //TODO!+ Lookup "boundary" and "boundary.break"  in scala.util
  false

@tailrec
def isSafeAscending2(report: Array[Int]): Boolean =
  if report.length == 1 then
    true
  else
    val head = report(0)
    val next = report(1)
    if head >= next then
      false // Not ascending
    else if next - head > 3 then
      false // Difference is too large
    else
      isSafeAscending2(report.drop(1))

@tailrec
def isSafeDescending2(report: Array[Int]): Boolean =
  if report.length == 1 then
    true
  else
    val head = report(0)
    val next = report(1)
    if next >= head then
      false // Not descending
    else if head - next > 3 then
      false // Difference is too large
    else
      isSafeDescending2(report.drop(1))

def solveDay02Part1(reports: List[Array[Int]]): Int =
  reports.count( report => isSafe(report))

def parseDay02Input(filename: String) =
  val source = Source.fromFile(filename)
  val lines = source.getLines
  val reports = lines.map( line =>
    line.split("\\s+").map( str => str.toInt )
  ).toList
  reports

def isSafe(report: Array[Int]) =
  if isAscending(report) then
    isSafeAscending(report)
  else if isDescending(report) then
    isSafeDescending(report)
  else
    false

def isSafeAscending(report: Array[Int]): Boolean =
  val reportAsList = report.toList
  val tuples = reportAsList.zip(reportAsList.tail)
  tuples.forall( (a,b) => b - a <= 3 ) // Difference of at least 1 already established by isAscending method.

def isSafeDescending(report: Array[Int]): Boolean =
  val reportAsList = report.toList
  val tuples = reportAsList.zip(reportAsList.tail)
  tuples.forall((a, b) => a - b <= 3) // Difference of at least 1 already established by isDescending method.

def isAscending(report: Array[Int]) =
  val indices = Range(0, report.length - 1)
  indices.forall( idx => report(idx) < report(idx + 1) )

def isDescending(report: Array[Int]) =
  val indices = Range(0, report.length - 1)
  indices.forall(idx => report(idx) > report(idx + 1))

def solveDay01(): Unit =
  //TODO!~ Get it from the proper resources folder
  //val bufferedSource = Source.fromResource("inputFiles\\AoCDay01_sample.txt")
  //val firstPart = day01part1(bufferedSource)
  val (leftList, rightList) = parseDay01Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay01.txt")
  val firstPart = day01Part1(leftList, rightList)
  println(s"Solution to part 1: $firstPart") // 2031679
  val secondPart = day01Part2(leftList, rightList)
  println(s"Solution to part 2: $secondPart")

def day01Part1(leftList: ArrayBuffer[Int], rightList: ArrayBuffer[Int]): Int =
  val sortedLeftList = leftList.sorted
  val sortedRightList = rightList.sorted
  val summedDistances = sortedLeftList.zip(sortedRightList)
    .map(elem => abs(elem._1 - elem._2))
    .sum()
  summedDistances

def day01Part2(leftList: ArrayBuffer[Int], rightList: ArrayBuffer[Int]): Int =
  // Count the occurrences of each element in the right list.
  // Suggested by: https://stackoverflow.com/a/28495085/812149
  val occurrences = rightList.groupBy(identity).view.mapValues(_.size)
  // With the occurrences available, let's calculate the sum of products.
  var sum = 0
  leftList.foreach( elem =>
    val count = occurrences.getOrElse(elem, 0)
    val product = elem * count
    sum += product
  )
  sum

def parseDay01Input(filename: String): (ArrayBuffer[Int], ArrayBuffer[Int]) =
  //TODO!~ See if we can use the "using" syntax which is more clean.
  val source = Source.fromFile(filename)
  val lines = source.getLines
  val leftList: ArrayBuffer[Int] = ArrayBuffer()
  val rightList: ArrayBuffer[Int] = ArrayBuffer()
    lines.foreach(line => {
    val parts = line.split("\\s+")
    leftList += parts(0).toInt
    rightList += parts(1).toInt
  })
  source.close()
  (leftList, rightList)