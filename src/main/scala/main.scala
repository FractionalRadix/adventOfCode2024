package com.cormontia.adventOfCode2024

import scala.collection.mutable.ArrayBuffer

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.
@main
def main(): Unit =
  //solveDay01()
  //solveDay02()
  //solveDay03()
  //solveDay04()
  //solveDay05()
  //solveDay06()
  //solveDay07()
  solveDay08()

def solveDay08(): Unit =
  val solver = SolverDay08()
  val input = solver.parseDay08Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay08.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The number of anti-nodes is $solutionPart1.") // 289
  val solutionPart2 = solver.solvePart2(input)
  println(s"The number of anti-nodes, including resonant harmonics, is $solutionPart2.")

def solveDay07(): Unit =
  val solver = SolverDay07()
  val input = solver.parseDay07Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay07.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The total calibration result is $solutionPart1") // 4122618559853
  val solutionPart2 = solver.solvePart2(input)
  println(s"The corrected total calibration result is $solutionPart2") // 227615740238334

def solveDay06(): Unit =
  val solver = SolverDay06()
  val input = solver.parseDay06Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay06.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The number of unique positions visited is $solutionPart1.") // 4776
  val solutionPart2 = solver.solvePart2(input)
  println(s"The number of places where an obstacle leads to an infinite loop is $solutionPart2.") // 1586

def solveDay05(): Unit =
  val solver = SolverDay05()
  val input = solver.parseDay05Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay05.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The sum of middle pages of correctly printed updates is $solutionPart1.") // 4774
  val solutionPart2 = solver.solvePart2(input)
  println(s"The sum of middle pages of corrected updates is $solutionPart2.") // 6004

def solveDay04(): Unit =
  val solver = SolverDay04()
  val input = solver.parseDay04Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay04.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The solution to part 1 is: $solutionPart1") // 2633
  val solutionPart2 = solver.solvePart2(input)
  println(s"The solution to part 2 is: $solutionPart2") // 1936

def solveDay03(): Unit =
  val solver = SolverDay03()
  solver.solve()

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
