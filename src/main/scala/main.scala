package com.cormontia.adventOfCode2024

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.math.abs

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
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
  var sum = 0
  leftList.foreach( elem =>
    val count = occurrences.getOrElse(elem, 0)
    val product = elem * count
    sum += product
  )
  sum

def parseDay01Input(filename: String): (ArrayBuffer[Int], ArrayBuffer[Int]) =
  //TODO!~ See if we an use the "using" syntax which is more clean.
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