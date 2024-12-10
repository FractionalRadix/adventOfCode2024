package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay10 {
  def parseDay10Input(filename: String): CharacterBlock2 =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    CharacterBlock2(lines)

  def solvePart1(grid: CharacterBlock2): Long =
    val trailheads = grid.findCoordinatesOf2('0')
    println(s"Trailheads: ${trailheads.mkString(",")}")
    var sum = 0
    for trailhead <- trailheads do
      print(s"Trailhead $trailhead: ")
      val visited = scala.collection.mutable.Set[Coor]()
      val trails = countTrails(grid, trailhead, visited)
      println(s" $trails")
      sum = sum + trails
    sum //TODO!~

  //TODO!~ This counts the total number of trails.
  // But the question is the number of ENDING positions, not how many ways to get there.
  // So, let's keep track of visited positions as well.
  // If a position has already been visited, do NOT add its trails.
  def countTrails(grid: CharacterBlock2, startPos: Coor, visited: scala.collection.mutable.Set[Coor]): Int =
    var sum = 0
    val elevation = grid.getCharAt(startPos)

    if elevation == '9' then
      print(" done!")
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

    sum //TODO!~

  def solvePart2(grid: CharacterBlock2): Long =
    val trailheads = grid.findCoordinatesOf2('0')
    println(s"Trailheads: ${trailheads.mkString(",")}")
    var sum = 0
    for trailhead <- trailheads do
      print(s"Trailhead $trailhead: ")
      //val visited = scala.collection.mutable.Set[Coor]()
      val trails = countTrails2(grid, trailhead)
      println(s" $trails")
      sum = sum + trails
    sum


  private def countTrails2(grid: CharacterBlock2, startPos: Coor): Int =
    var sum = 0
    val elevation = grid.getCharAt(startPos)

    if elevation == '9' then
      print(" done!")
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

    sum //TODO!~

}

