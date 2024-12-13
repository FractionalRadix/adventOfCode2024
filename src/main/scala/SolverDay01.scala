package com.cormontia.adventOfCode2024

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.abs
import scala.util.{Try, Using}

class SolverDay01 {
  
  def parseDay01Input(filename: String): Try[(ArrayBuffer[Int], ArrayBuffer[Int])] =
    //TODO!~ See if we can use the "using" syntax which is more clean.
    Using (Source.fromFile(filename)) { source =>
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
    }

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
    leftList.foreach(elem =>
      val count = occurrences.getOrElse(elem, 0)
      val product = elem * count
      sum += product
    )
    sum

}