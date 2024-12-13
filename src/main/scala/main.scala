package com.cormontia.adventOfCode2024

import java.time.{Duration, LocalDateTime}

//TIP To <b>Run</b> code, press <shortcut actionId="Run"/> or click the <icon src="AllIcons.Actions.Execute"/> icon in the gutter.

//TODO!~ Put ALL the input functions in a "Using" block.
// --> This will be done automatically if we make every "SolverDayXX" a subclass of "Solver".

@main
def main(): Unit =
  solveDay01()
  solveDay02()
  solveDay03()
  solveDay04()
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

def solveDay14(): Unit =
  println("Day 14:")
  SolverDay14().solve("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay14.txt")

def solveDay13(): Unit =
  println("Day 13:")
  // Part 1: 38839
  // Part 2: 75200131617108
  SolverDay13().solve("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay13.txt")

def solveDay12(): Unit =
  //TODO!~ Derive SolverDay12 from Solver and let that thing do the work.
  println("Day 12:")
  val solver = SolverDay12()
  val input = solver.parseDay12Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay12.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The total price is $solutionPart1.") // 1304764
  val solutionPart2 = solver.solvePart2(input)
  println(s"The solution to part 2 is $solutionPart2.") // 811148

def solveDay11(): Unit =
  //TODO!~ Derive SolverDay11 from Solver and let that thing do the work.
  println("Day 11:")
  val solver = SolverDay11()
  val input = solver.parseDay11Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay11.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The number of stones after blinking 25 times is $solutionPart1.") // 198075
  val solutionPart2 = solver.solvePart2(input)
  println(s"The number of stones after blinking 75 times is $solutionPart2.") // 235571309320764

def solveDay10(): Unit =
  //TODO!~ Derive SolverDay10 from Solver and let that thing do the work.
  println("Day 10:")
  val solver = SolverDay10()
  val input = solver.parseDay10Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay10.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The sum of scores for all trailheads is $solutionPart1") // 822
  val solutionPart2 = solver.solvePart2(input)
  println(s"The sum of ratings for all trailheads is $solutionPart2") // 1801

def solveDay09(): Unit =
  //TODO!~ Derive SolverDay09 from Solver and let that thing do the work.
  println("Day 09:")
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
  //TODO!~ Derive SolverDay08 from Solver and let that thing do the work.
  println("Day 08:")
  val solver = SolverDay08()
  val input = solver.parseDay08Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay08.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The number of anti-nodes is $solutionPart1.") // 289
  val solutionPart2 = solver.solvePart2(input)
  println(s"The number of anti-nodes, including resonant harmonics, is $solutionPart2.") // 1030

def solveDay07(): Unit =
  //TODO!~ Derive SolverDay07 from Solver and let that thing do the work.
  println("Day 07:")
  val solver = SolverDay07()
  val input = solver.parseDay07Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay07.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The total calibration result is $solutionPart1") // 4122618559853
  val solutionPart2 = solver.solvePart2(input)
  println(s"The corrected total calibration result is $solutionPart2") // 227615740238334

def solveDay06(): Unit =
  //TODO!~ Derive SolverDay06 from Solver and let that thing do the work.
  println("Day 06:")
  val solver = SolverDay06()
  val input = solver.parseDay06Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay06.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The number of unique positions visited is $solutionPart1.") // 4776
  val solutionPart2 = solver.solvePart2(input)
  println(s"The number of places where an obstacle leads to an infinite loop is $solutionPart2.") // 1586

def solveDay05(): Unit =
  //TODO!~ Derive SolverDay05 from Solver and let that thing do the work.
  println("Day 05:")
  val solver = SolverDay05()
  val input = solver.parseDay05Input("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay05.txt")
  val solutionPart1 = solver.solvePart1(input)
  println(s"The sum of middle pages of correctly printed updates is $solutionPart1.") // 4774
  val solutionPart2 = solver.solvePart2(input)
  println(s"The sum of middle pages of corrected updates is $solutionPart2.") // 6004

def solveDay04(): Unit =
  println("Day 04:")
  // Part 1: 2633
  // Part 2: 1936
  SolverDay04().solve("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay04.txt")

def solveDay03(): Unit =
  println("Day 03:")
  // Part 1: 173529487
  // Part 2: 99532691
  SolverDay03().solve()

def solveDay02(): Unit =
  println("Day 02:")
  // Part 1: 639
  // Part 2: 674
  SolverDay02().solve("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay02.txt")

def solveDay01(): Unit =
  println("Day 01:")
  // Part 1: 2031679
  // Part 2: 19678534
  //TODO!~ Get the input file from the proper resources folder
  SolverDay01().solve("/home/serge/IdeaProjects/adventOfCode2024/src/main/resources/inputFiles/AoCDay01.txt")
