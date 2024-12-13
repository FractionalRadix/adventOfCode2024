package com.cormontia.adventOfCode2024

import java.time.{Duration, LocalDateTime}
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.

//TODO!~ Put ALL the input functions in a "Using" block.
// --> This will be done automatically if we make every "SolverDayXX" a subclass of "Solver".

@main
def main(): Unit =
  //solveDay01()
  //solveDay02()
  //solveDay03()
  //solveDay04()
  //solveDay05()
  //solveDay06()
  //solveDay07()
  //solveDay08()
  //solveDay09()
  //solveDay10()
  //solveDay11()
  solveDay12()
  solveDay13()
  solveDay14()

abstract class Solver {
  def parseInput(filename: String): Try[List[String]]
  def solvePart1(lines: List[String]): Long
  def solvePart2(lines: List[String]): Long

  def solve(filename: String): Unit =
    val input = parseInput(filename)
    input match
      case Success(lines) =>
        val solutionPart1 = solvePart1(lines)
        println(s"The solution to Part 1 is $solutionPart1.")
        val solutionPart2 = solvePart2(lines)
        println(s"The solution to Part 2 is is $solutionPart2.")
      case Failure(_)  => println("Failed to read input file!")
}

def solveDay14(): Unit =
  SolverDay14().solve("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay14.txt")

def solveDay13(): Unit =
  // Part 1: 38839
  // Part 2: 75200131617108
  SolverDay13().solve("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay13.txt")

def solveDay12(): Unit =
  val solver = SolverDay12()
  val input = solver.parseDay12Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay12.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The total price is $solutionPart1.") // 1304764
  val solutionPart2 = solver.solvePart2(input)
  println(s"The solution to part 2 is $solutionPart2.") // 811148

def solveDay11(): Unit =
  val solver = SolverDay11()
  val input = solver.parseDay11Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay11.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The number of stones after blinking 25 times is $solutionPart1.") // 198075
  val solutionPart2 = solver.solvePart2(input)
  println(s"The number of stones after blinking 75 times is $solutionPart2.") // 235571309320764

def solveDay10(): Unit =
  val solver = SolverDay10()
  val input = solver.parseDay10Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay10.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The sum of scores for all trailheads is $solutionPart1") // 822
  val solutionPart2 = solver.solvePart2(input)
  println(s"The sum of ratings for all trailheads is $solutionPart2") // 1801

def solveDay09(): Unit =
  val solver = SolverDay09()
  val input = solver.parseDay09Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay09.txt")

  val startTime1 = LocalDateTime.now()
  val solutionPart1 = solver.solvePart1(input)
  val endTime1 = LocalDateTime.now()
  val duration1 = Duration.between(startTime1, endTime1)
  println(s"The checksum is $solutionPart1.") // 6344673854800
  println(s"It took: ${duration1.toSeconds} seconds.")

  val startTime2 = LocalDateTime.now()
  val solutionPart2 = solver.solvePart2(input)
  val endTime2 = LocalDateTime.now()
  val duration2 = Duration.between(startTime2, endTime2)
  println(s"The new checksum is $solutionPart2.") // 6360363199987
  println(s"It took: ${duration2.toSeconds} seconds.")
  
def solveDay08(): Unit =
  val solver = SolverDay08()
  val input = solver.parseDay08Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay08.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The number of anti-nodes is $solutionPart1.") // 289
  val solutionPart2 = solver.solvePart2(input)
  println(s"The number of anti-nodes, including resonant harmonics, is $solutionPart2.") // 1030

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
  val reportList = solver.parseDay02Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay02.txt")
  reportList match
    case Success(reports) =>
      val nrOfSafeReports = solver.solveDay02Part1(reports)
      println(s"Number of safe reports: $nrOfSafeReports") // 639
      val nrOfSafeReports2 = solver.solveDay02Part2(reports)
      println(s"Nr of safe reports if a single level can be removed: $nrOfSafeReports2") // 674
    case Failure(_) => println("Failed to open the file.")

def solveDay01(): Unit =
  val solver01 = SolverDay01()
  //TODO!~ Get the input file from the proper resources folder
  val tryLists = solver01.parseDay01Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay01.txt")
  tryLists match
    case Success(leftList, rightList) =>
      val firstPart = solver01.day01Part1(leftList, rightList)
      println(s"The total distance is: $firstPart") // 2031679
      val secondPart = solver01.day01Part2(leftList, rightList)
      println(s"The similarity score is: $secondPart") // 19678534
    case Failure(_) => println("Failed to parse input file!")
