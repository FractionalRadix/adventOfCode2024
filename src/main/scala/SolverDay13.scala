package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay13 {
  def parseDay13Input(filename: String): List[String] =
    val source = Source.fromFile(filename)
    source.getLines.toList

  def solvePart1(lines: List[String]): Long =
    val machines = parseInput(lines)
    var tokens: Long = 0
    for machine <- machines do
      //machine.print()
      //val r = // m = (ax * y - ay * x) / (ax * by - ay * bx)
      val nominator = machine.ax * machine.y - machine.ay * machine.x
      val denominator = machine.ax * machine.by - machine.ay * machine.bx
      if denominator == 0 then
        //println("Goal cannot be reached on this machine.")
        ;
      else
        val m = nominator / denominator
        val n = (machine.x - m* machine.bx) / machine.ax
        // 'n' and 'm' are Long, not Double or Float. They've been rounded off.
        // Now let's see if there's roundoff error:
        val reachedX = n * machine.ax + m * machine.bx
        val reachedY = n * machine.ay + m * machine.by
        if reachedX == machine.x && reachedY == machine.y then
          //println(s"$n times move A, $m times move B.")
          tokens = tokens + 3 * n + m
        //else
        //  println("Goal cannot be reached on this machine.")

    tokens

// The problem to solve is a set of equations:
//   n*ax + m*bx = x
//   n*ay + m*by = y
// The unknowns are n and m.
// n * ax = x - m * bx
// n * ay = y - m * by
// Therefore
// n = (x - m*bx) / ax
// n = (y - m*by) / ay
// Therefore
// (x - m*bx) / ax = (y - m*by) / ay
// Therefore
// ay * (x - m*bx) = ax * (y - m * by)
// Therefore
// ay * x - ay * m * bx = ax * y - ax * m * by
// Therefore
// ay * x - ay * m * bx + ax * m * by = ax * y
// Therefore
// - ay * m * bx + ax * m * by = ax * y - ay * x
// Therefore
// ax * m * by - ay * m * bx = ax * y - ay * x
// Therefore
// m * (ax * by - ay * bx) = ax * y - ay * x
// Therefore
// m = (ax * y - ay * x) / (ax * by - ay * bx)
// (And presumably, by analogy, m = (bx * y - by * x) / (bx * ay - by * ax) ....)


  def solvePart2(lines: List[String]): Long =
    println("Starting part 2.")
    val originalMachines = parseInput(lines)
    val machines = for machine <- originalMachines yield
      ClawMachine(machine.ax, machine.ay, machine.bx, machine.by, machine.x + 10000000000000L , machine.y + 10000000000000L)
      //  10000000000000

    var tokens: Long = 0
    for machine <- machines do
      //machine.print()
      val nominator = machine.ax * machine.y - machine.ay * machine.x
      val denominator = machine.ax * machine.by - machine.ay * machine.bx
      if denominator == 0 then
        //println("Goal cannot be reached on this machine.")
        ;
      else
        val m = nominator / denominator
        val n = (machine.x - m* machine.bx) / machine.ax
        // 'n' and 'm' are Long, not Double or Float. They've been rounded off.
        // Now let's see if there's roundoff error:
        val reachedX = n * machine.ax + m * machine.bx
        val reachedY = n * machine.ay + m * machine.by
        if reachedX == machine.x && reachedY == machine.y then
          //println(s"$n times move A, $m times move B.")
          tokens = tokens + 3 * n + m
        //else
        //  println("Goal cannot be reached on this machine.")

    tokens

  case class ClawMachine(ax: Long, ay: Long, bx: Long, by: Long, x: Long, y: Long) {
    def print(): Unit =
      println()
      println("Claw Machine:")
      println(s"  Button A: x+$ax y+$ay")
      println(s"  Button B: x+$bx y+$by")
      println(s"  Prize: $x $y")
  }

  private def parseInput(lines: List[String]): List[ClawMachine] =
    var result: List[ClawMachine] = Nil
    var ax: Long = 0
    var ay: Long = 0
    var bx: Long = 0
    var by: Long = 0
    var x: Long = 0
    var y: Long = 0
    for line <- lines do
      if line.startsWith("Button A") then
        val (ax1,ay1) = parseButton(line)
        ax = ax1
        ay = ay1
        //println(s"A: +$ax +$ay")
      else if line.startsWith("Button B") then
        val (bx1,by1) = parseButton(line)
        bx = bx1
        by = by1
        //println(s"B: +$bx +$by")
      else if line.startsWith("Prize") then
        val (x1,y1) = parsePrize(line)
        x = x1
        y = y1
        //println(s"prize: $x $y")
        val machine = ClawMachine(ax, ay, bx, by, x, y)
        result = machine :: result
    result.reverse

  private def parseButton(line: String): (Long, Long) =
    val secondHalf = line.drop(10) // Drop the "Button A: " or "Button B: " part.
    val increments = secondHalf.filter( ch => ch != ' ' ).split(",") // Filter whitespace, then split in "X+...", "Y+...".
    val x = increments(0).drop(2).toLong
    val y = increments(1).drop(2).toLong
    (x, y)

  private def parsePrize(line: String): (Long, Long) =
    val secondHalf = line.drop(7) // Drop the "Prize: " part.
    val coordinates = secondHalf.filter( ch => ch != ' ' ).split(",") // Filter whitespace, then split in "X=...", "Y=..."
    val x = coordinates(0).drop(2).toLong
    val y = coordinates(1).drop(2).toLong
    (x,y)

}

