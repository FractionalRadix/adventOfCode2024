package com.cormontia.adventOfCode2024

import scala.collection.mutable.ArrayBuffer
import scala.math.abs

class SolverDay01 extends Solver {

  override def solvePart1(lines: List[String]): String =
    val (leftList, rightList) = determineLists(lines)
    val sortedLeftList = leftList.sorted
    val sortedRightList = rightList.sorted
    val summedDistances = sortedLeftList.zip(sortedRightList)
      .map(elem => abs(elem._1 - elem._2))
      .sum()
    summedDistances.toString

  override def solvePart2(lines: List[String]): String =
    val (leftList, rightList) = determineLists(lines)
    // Count the occurrences of each element in the right list.
    // Suggested by: https://stackoverflow.com/a/28495085/812149
    val occurrences = rightList.groupBy(identity).view.mapValues(_.size)
    // With the occurrences available, let's calculate the sum of products.
    var sum = 0
    leftList.foreach(elem =>
      val count = occurrences.getOrElse(elem, 0)
      val product = elem * count
      sum += product
    )
    sum.toString

  private def determineLists(lines: List[String]): (ArrayBuffer[Int], ArrayBuffer[Int]) =
    val leftList: ArrayBuffer[Int] = ArrayBuffer()
    val rightList: ArrayBuffer[Int] = ArrayBuffer()
    lines.foreach(line => {
      val parts = line.split("\\s+")
      leftList += parts(0).toInt
      rightList += parts(1).toInt
    })
    (leftList, rightList)

}