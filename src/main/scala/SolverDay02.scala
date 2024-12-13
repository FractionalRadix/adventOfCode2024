package com.cormontia.adventOfCode2024

import scala.annotation.tailrec

class SolverDay02 extends Solver {

  override def solvePart1(lines: List[String]): Long =
    val reports = parseReports(lines)
    reports.count(report => isSafe(report))

  override def solvePart2(lines: List[String]): Long =
    val reports = parseReports(lines)
    //TODO?~ It would be more elegant to use a count(...) function.
    var count = 0
    reports.foreach(report =>
      var asc = isSafeAscending2(report)
      var desc = isSafeDescending2(report)
      if (!asc && !desc)
        asc = trySafeAscending(report)
        desc = trySafeDescending(report)
      if asc || desc then count = count + 1
    )
    count

  private def parseReports(lines: List[String]): List[Array[Int]] =
    val reports = lines.map(line =>
      line.split("\\s+").map(str => str.toInt)
    )
    reports

  private def trySafeAscending(report: Array[Int]): Boolean =
    // Naive solution that works: for each level, try removing it.
    for i <- report.indices do
      val candidate = report.patch(i, Nil, 1)
      if isSafeAscending2(candidate) then
        // If the entire report is safe after removing a single level, then it is safe.
        return true //TODO!+ Lookup "boundary" and "boundary.break"  in scala.util
    false

  private def trySafeDescending(report: Array[Int]): Boolean =
    for i <- report.indices do
      val candidate = report.patch(i, Nil, 1)
      if isSafeDescending2(candidate) then
        // If the entire report is safe after removing a single level, then it is safe.
        return true //TODO!+ Lookup "boundary" and "boundary.break"  in scala.util
    false

  @tailrec
  private final def isSafeAscending2(report: Array[Int]): Boolean =
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
  private final def isSafeDescending2(report: Array[Int]): Boolean =
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

  private def isSafe(report: Array[Int]): Boolean =
    if isAscending(report) then
      isSafeAscending(report)
    else if isDescending(report) then
      isSafeDescending(report)
    else
      false

  private def isSafeAscending(report: Array[Int]): Boolean =
    val reportAsList = report.toList
    val tuples = reportAsList.zip(reportAsList.tail)
    tuples.forall((a, b) => b - a <= 3) // Difference of at least 1 already established by isAscending method.

  private def isSafeDescending(report: Array[Int]): Boolean =
    val reportAsList = report.toList
    val tuples = reportAsList.zip(reportAsList.tail)
    tuples.forall((a, b) => a - b <= 3) // Difference of at least 1 already established by isDescending method.

  private def isAscending(report: Array[Int]): Boolean =
    val indices = Range(0, report.length - 1)
    indices.forall(idx => report(idx) < report(idx + 1))

  private def isDescending(report: Array[Int]): Boolean =
    val indices = Range(0, report.length - 1)
    indices.forall(idx => report(idx) > report(idx + 1))
}
