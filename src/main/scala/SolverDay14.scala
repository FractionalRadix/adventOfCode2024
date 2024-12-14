package com.cormontia.adventOfCode2024

import scala.io.Source
import scala.io.StdIn.readLine
import scala.util.{Try, Using}

class SolverDay14 extends Solver {

  override def solvePart1(lines: List[String]): Long =
    val robots = parseInput(lines)

    val width = 101
    val height = 103
    val seconds = 100
    val newRobots = for robot <- robots yield
      nextRobot(robot, seconds, width, height)

    val xHalf = (width - 1) / 2
    val yHalf = (height - 1) / 2
    val robotsIn1stQuadrant = newRobots.count { robot => robot.px < xHalf && robot.py < yHalf}
    val robotsIn2ndQuadrant = newRobots.count { robot => robot.px < xHalf && robot.py > yHalf}
    val robotsIn3thQuadrant = newRobots.count { robot => robot.px > xHalf && robot.py < yHalf}
    val robotsIn4thQuadrant = newRobots.count { robot => robot.px > xHalf && robot.py > yHalf}
    val solution = robotsIn1stQuadrant * robotsIn2ndQuadrant * robotsIn3thQuadrant * robotsIn4thQuadrant

    solution

  override def solvePart2(lines: List[String]): Long =
    var robots = parseInput(lines)
    var count = 0
    val width = 101
    val height = 103

    var found = false
    while !found do
      robots = for robot <- robots
        yield nextRobot(robot, 1, width, height)
      count = count + 1

      // Hint from Reddit... the picture of a tree contains borders.
      // So look for (arbitrarily large) ranges of subsequent robots.
      found = robotsAlignedVertically(robots, 14) && robotsAlignedHorizontally(robots, 14)
      if found then
        val grid = buildGrid(robots, width, height)
        printGrid(grid)
        println(s"That was at count: $count")

    count

  private def buildGrid(robots: List[Robot], width: Int, height: Int): Grid[Char] =
    val grid = Grid[Char](height, width)
    for row <- 0 until height do
      for col <- 0 until width do
        val countBots = robots.count(r => r.px == col && r.py == row)
        val ch = if countBots == 0 then '.' else '*' // Should be a number? What if there's more than 10 robots in one place?
        grid.set(row, col, ch)
    grid

  // This didn't really work...
  private def hasBorders(robots: List[Robot], width: Int, height: Int): Boolean =
    val top = robots.filter( robot => robot.py == 0 )
    val left = robots.filter( robot => robot.px == 0 )
    top.length == width && left.length == height

  /**
   * Test if `n` robots are aligned vertically.
   * There should be `n` robots that have the same x coordinate, and whose y coordinate increases by 1.
   * @param robots The set of robots to inspect.
   * @param n The number of robots that should be aligned vertically.
   * @return `true` if and only if there are at least `n` robots aligned vertically.
   */
  private def robotsAlignedVertically(robots: List[Robot], n: Int): Boolean =
    val robotsPerColumn = robots.groupBy(robot => robot.px).filter((x, list) => list.size >= n)
    for column <- robotsPerColumn do
      val yPositions = column._2.map( robot => robot.py )
      if Util.containsContiguousChunk(yPositions, n) then
        return true
    false

  /**
   * Test if `n` robots are aligned horizontally.
   * There should be `n` robots that have the same y coordinate, and whose x coordinate increases by 1.
   *
   * @param robots The set of robots to inspect.
   * @param n      The number of robots that should be aligned horizontally.
   * @return `true` if and only if there are at least `n` robots aligned horizontally.
   */
  private def robotsAlignedHorizontally(robots: List[Robot], n: Int): Boolean =
    val robotsPerRow = robots.groupBy(robot => robot.py).filter((y, list) => list.size >= n)
    for row <- robotsPerRow do
      val xPositions = row._2.map(robot => robot.px)
      if Util.containsContiguousChunk(xPositions, n) then
        return true
    false

  private def printGrid(grid: Grid[Char]): Unit =
    for row <- 0 until grid.nrOfRows do
      for col <- 0 until grid.nrOfCols do
        print(grid.get(Coor(row, col)))
      println

  private def nextRobot(robot: Robot, seconds: Int, width: Int, height: Int): Robot =
    var px = (robot.px + seconds * robot.vx) % width
    var py = (robot.py + seconds * robot.vy) % height
    if px < 0 then px = px + width
    if py < 0 then py = py + height
    Robot(px, py, robot.vx, robot.vy)

  def parseInput(lines: List[String]): List[Robot] =
    for line <- lines yield
      val parts = line.split(" ")
      val positionStr = parts(0).drop(2) // Drop the "p="
      val velocityStr = parts(1).drop(2) // Drop the "v="
      val coordinates = positionStr.split(",")
      val (px, py) = (coordinates(0).toLong, coordinates(1).toLong)
      val velocityComponents = velocityStr.split(",")
      val (vx, vy) = (velocityComponents(0).toInt, velocityComponents(1).toInt)
      Robot(px, py, vx, vy)

  case class Robot(px: Long, py: Long, vx: Long, vy: Long)

}
