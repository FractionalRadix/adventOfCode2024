package com.cormontia.adventOfCode2024

import scala.io.Source
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
    var count = 0

    0 //TODO!~

  def nextRobot(robot: Robot, seconds: Int, width: Int, height: Int): Robot =
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
