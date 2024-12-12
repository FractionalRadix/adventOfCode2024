package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay12 {
  def parseDay12Input(filename: String): Grid[Char] =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    Grid[Char](lines, ch => ch)

  def solvePart1(grid: Grid[Char]): Long =
    // Let's create a second Grid, where each area has a unique number.
    val newGrid = Grid[Option[Int]](grid.nrOfRows, grid.nrOfCols)
    for rowIdx <- 0 until grid.nrOfRows do
      for colIdx <- 0 until grid.nrOfCols do
        newGrid.set(rowIdx, colIdx, None)

    var price: Long = 0

    var regionId = 1
    for rowIdx <- 0 until grid.nrOfRows do
      for colIdx <- 0 until grid.nrOfCols do
        if newGrid.get(rowIdx, colIdx).isEmpty then
          val region = identifyRegion(grid, Coor(rowIdx, colIdx))
          // NOTE that the size of "region" is the area.
          // We could even determine the perimeter!
          val area = region.size
          val xPerimeter = perimeter(region)
          println(s"Area: $area Perimeter: $xPerimeter")
          price = price + area * xPerimeter
          //printRegion(grid, region)
          //scala.io.StdIn.readLine() // <-- wait for user to accept.
          for coor <- region do
            newGrid.set(coor.row, coor.col, Some(regionId))
          regionId = regionId + 1
    price

  /**
   * The perimeter of a region is the sum of all sides that DON'T touch another plot in the region.
   * Note that the region may contain "holes", and that the sides of these holes count towards the total.
   * @param region A list of coordinates that span a region.
   * @return The sum of all sides that don't touch another plot in the region.
   */
  private def perimeter(region: Set[Coor]): Int =
    var perimeter = 0
    for plot <- region do
      val above = Coor(plot.row - 1, plot.col)
      val below = Coor(plot.row + 1, plot.col)
      val left = Coor(plot.row, plot.col - 1)
      val right = Coor(plot.row, plot.col + 1)
      for plot <- List(above, below, left, right) do
      if (!region.contains(plot))
        perimeter = perimeter + 1
    perimeter //TODO!~

  private def printRegion(grid: Grid[Char], plots: Set[Coor]): Unit =
    println
    for rowIdx <- 0 until grid.nrOfRows do
      for colIdx <- 0 until grid.nrOfCols do
        if plots.contains(Coor(rowIdx, colIdx)) then
          print("X")
        else
          print(".")
      println

  private def identifyRegion(grid: Grid[Char], startPos: Coor): Set[Coor] =
    val knownCoordinates = scala.collection.mutable.Set[Coor]()
    var coordinatesToProcess = List(startPos)
    val ch = grid.get(startPos)
    while coordinatesToProcess.nonEmpty do
      val current = coordinatesToProcess.head
      knownCoordinates.add(current)
      coordinatesToProcess = coordinatesToProcess.drop(1)

      val aboveNeighbourCoor = Coor(current.row - 1, current.col)
      val belowNeighbourCoor = Coor(current.row + 1, current.col)
      val rightNeighbourCoor = Coor(current.row, current.col + 1)
      val leftNeighbourCoor = Coor(current.row, current.col - 1)

      for neighbourCoor <- List(rightNeighbourCoor, leftNeighbourCoor, belowNeighbourCoor, aboveNeighbourCoor) do
        if !knownCoordinates.contains(neighbourCoor) then
          if grid.safeGet(neighbourCoor).contains(ch) then
            coordinatesToProcess = neighbourCoor :: coordinatesToProcess

    knownCoordinates.toSet

  def solvePart2(grid: Grid[Char]): Long =
    0 //TODO!~  
}
