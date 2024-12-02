package com.cormontia.adventOfCode2024

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  solveDay01()
  solveDay02()

def solveDay02(): Unit =
  val solver = SolverDay02()
  val reports = solver.parseDay02Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay02.txt")
  val nrOfSafeReports = solver.solveDay02Part1(reports)
  println(s"Number of safe reports: $nrOfSafeReports") // 639
  val nrOfSafeReports2 = solver.solveDay02Part2(reports)
  println(s"Nr of safe reports if a single level can be removed: $nrOfSafeReports2") // 674

def solveDay01(): Unit =
  //TODO!~ Get it from the proper resources folder
  //val bufferedSource = Source.fromResource("inputFiles\\AoCDay01_sample.txt")
  //val firstPart = day01part1(bufferedSource)
  val solver01 = SolverDay01()
  val (leftList, rightList) = solver01.parseDay01Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay01.txt")
  val firstPart = solver01.day01Part1(leftList, rightList)
  println(s"Solution to part 1: $firstPart") // 2031679
  val secondPart = solver01.day01Part2(leftList, rightList)
  println(s"Solution to part 2: $secondPart") // 19678534
