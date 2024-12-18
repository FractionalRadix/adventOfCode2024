package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay10 extends Solver {
  def parseDay10Input(filename: String): CharacterBlock2 =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    CharacterBlock2(lines)

  //TODO?~  Use Grid[Char] instead of CharacterBlock2.
  def parseInput(lines: List[String]): CharacterBlock2 =
    CharacterBlock2(lines)

  override def solvePart1(lines: List[String]): String =
    val grid = parseInput(lines)
    val trailheads = grid.findCoordinatesOf2('0')
    //println(s"Trailheads: ${trailheads.mkString(",")}")
    var sum = 0
    for trailhead <- trailheads do
      //print(s"Trailhead $trailhead: ")
      val visited = scala.collection.mutable.Set[Coor]()
      val trails = countTrails(grid, trailhead, visited)
      //println(s" $trails")
      sum = sum + trails
    sum.toString

  override def solvePart2(lines: List[String]): String =
    val grid = parseInput(lines)
    val trailheads = grid.findCoordinatesOf2('0')
    //println(s"Trailheads: ${trailheads.mkString(",")}")
    var sum = 0
    for trailhead <- trailheads do
      val trails = countTrails2(grid, trailhead)
      sum = sum + trails
    sum.toString

  private def countTrails(grid: CharacterBlock2, startPos: Coor, visited: scala.collection.mutable.Set[Coor]): Int =
    var sum = 0
    val elevation = grid.getCharAt(startPos)

    if elevation == '9' then
      return 1

    var next = Coor(startPos.row - 1, startPos.col)
    if grid.withinBounds(next) && !visited.contains(next) then
      val neighbour = grid.getCharAt(next)
      if neighbour - elevation == 1 then
        visited.add(next)
        sum = sum + countTrails(grid, next, visited)

    next = Coor(startPos.row + 1, startPos.col)
    if grid.withinBounds(next) && !visited.contains(next) then
      val neighbour = grid.getCharAt(next)
      if neighbour - elevation == 1 then
        visited.add(next)
        sum = sum + countTrails(grid, next, visited)

    next = Coor(startPos.row, startPos.col - 1)
    if grid.withinBounds(next) && !visited.contains(next) then
      val neighbour = grid.getCharAt(next)
      if neighbour - elevation == 1 then
        visited.add(next)
        sum = sum + countTrails(grid, next, visited)

    next = Coor(startPos.row, startPos.col + 1)
    if grid.withinBounds(next) && !visited.contains(next) then
      val neighbour = grid.getCharAt(next)
      if neighbour - elevation == 1 then
        visited.add(next)
        sum = sum + countTrails(grid, next, visited)

    sum


  private def countTrails2(grid: CharacterBlock2, startPos: Coor): Int =
    var sum = 0
    val elevation = grid.getCharAt(startPos)

    if elevation == '9' then
      //print(" done!")
      return 1

    var next = Coor(startPos.row - 1, startPos.col)
    if grid.withinBounds(next) then
      val neighbour = grid.getCharAt(next)
      if neighbour - elevation == 1 then
        sum = sum + countTrails2(grid, next)

    next = Coor(startPos.row + 1, startPos.col)
    if grid.withinBounds(next) then
      val neighbour = grid.getCharAt(next)
      if neighbour - elevation == 1 then
        sum = sum + countTrails2(grid, next)

    next = Coor(startPos.row, startPos.col - 1)
    if grid.withinBounds(next) then
      val neighbour = grid.getCharAt(next)
      if neighbour - elevation == 1 then
        sum = sum + countTrails2(grid, next)

    next = Coor(startPos.row, startPos.col + 1)
    if grid.withinBounds(next) then
      val neighbour = grid.getCharAt(next)
      if neighbour - elevation == 1 then
        sum = sum + countTrails2(grid, next)

    sum
}

