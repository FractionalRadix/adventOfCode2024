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
          val area = region.size
          val xPerimeter = perimeter(region)
          //println(s"Area: $area Perimeter: $xPerimeter")
          price = price + area * xPerimeter
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
    perimeter

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
    val newGrid = uniqueRegions(grid)
    val regionIDs = newGrid.findDistinct()
    println(s"Region IDs: ${regionIDs.mkString(",")}")
    for regionID <- regionIDs do
      val plots = newGrid.findCoordinatesOf(regionID).toList
      println(s"Area: ${plots.size}")

    // Approach:
    // First, find all contiguous lines on each row and column.
    // AFTER that we include gaps for "diagonal crossings".
    // And MAYBE this is a case for an "inside/outside" algorithm used in computer graphics.
    val regionID = regionIDs.head //TODO!~ Make that a loop over regionIDs later.
    /*
    val bordersAbove = bordersAtDirection(newGrid, regionID.head, c => Coor(c.row - 1, c.col))
    println(s"ABOVE: [${bordersAbove.mkString(",")}]")

    val bordersBelow = bordersAtDirection(newGrid, regionID.head, c => Coor(c.row + 1, c.col))
    println(s"BELOW: [${bordersBelow.mkString(",")}]")

    val bordersLeft  = bordersAtDirection(newGrid, regionID.head, c => Coor(c.row, c.col - 1))
    println(s"LEFT : [${bordersLeft.mkString(",")}]")

    val bordersRight = bordersAtDirection(newGrid, regionID.head, c => Coor(c.row, c.col + 1))
    println(s"RIGHT : [${bordersRight.mkString(",")}]")
     */
      ;
    0 //TODO!~

  private def followSideAbove(grid: Grid[Option[Int]], startPos: Coor): Coor =
    println
    var coor = startPos
    val valueAbove = grid.safeGet(startPos.row - 1, startPos.col) // Might be None, if the row is 0....
    while (coor.col < grid.nrOfCols && grid.safeGet(coor.row - 1, coor.col) == valueAbove)
      print(s" $Coor")
      coor = Coor(coor.row, coor.col + 1)
    coor

  private def countContiguousHorizontalChunks(list: List[Coor]): Int =
    var sum = 0
    val minRow = list.minBy( coor => coor.row ).row
    val maxRow = list.maxBy( coor => coor.row ).row
    for row <- minRow to maxRow do
      // Determine all the column ID's present in this row.
      val columnIDs = list.filter( coor => coor.row == row ).map( coor => coor.col )
      sum = sum + countContiguousChunks( columnIDs )
    sum

  /**
   * Given a list of integers, determine how many contiguous chunks there are in it.
   * For example, [2,3,5,6,7] has two chunks: [2,3] and [5,6,7].
   * As another example, [1,4,5,7,9] has 4 chunks: [1], [4,5], [7], and [9].
   * As an example of an edge case, [8] has 1 chunck: [8]
   * @param list A list of integers.
   * @return The number of contiguous chunks in the list.
   */
  private def countContiguousChunks(list: List[Int]): Int =
    if list.isEmpty then
      0
    else
      var node = list
      var count = 1
      while node.tail.nonEmpty do
        if node.tail.head != node.head + 1 then
          count = count + 1
          print(s"<${node.head}>|<${node.tail.head}> ")
        node = node.tail
      count

  private def bordersAtDirection(grid1: Grid[Option[Int]], regionId: Int, neighbour: Coor => Coor): List[Coor] =
    var borders: List[Coor] = Nil
    for rowIdx <- 0 until grid1.nrOfRows do
      for colIdx <- 0 until grid1.nrOfCols do
        val current = Coor(rowIdx, colIdx)
        val _neighbour = neighbour(current)
        val currentValue = grid1.get(current)
        val neighbourValue = grid1.safeGet(_neighbour)
        print(s" $current ($currentValue) $_neighbour ($neighbourValue) |")
        if (currentValue.contains(regionId))
          if (neighbourValue.isEmpty)
            borders = current :: borders // In this case you're at the edge of the grid.
          else if !neighbourValue.head.contains(regionId) then
            borders = current :: borders
    println
    borders


  /**
   * Build a new map, where each region has a unique number.
   * So if we have two regions that both cultivate plant 'B', these two regions would have distinct numbers.
   * @param grid A map of the plots, where each letter represents a specific type of plant.
   * @return A map where, instead of letters, each plot is marked by the unique number of the region it is in.
   */
  private def uniqueRegions(grid: Grid[Char]): Grid[Option[Int]] =
    val newGrid = Grid[Option[Int]](grid.nrOfRows, grid.nrOfCols)
    for rowIdx <- 0 until grid.nrOfRows do
       for colIdx <- 0 until grid.nrOfCols do
         newGrid.set(rowIdx, colIdx, None)

    var regionId = 1
    for rowIdx <- 0 until grid.nrOfRows do
      for colIdx <- 0 until grid.nrOfCols do
        if newGrid.get(rowIdx, colIdx).isEmpty then
          val region = identifyRegion(grid, Coor(rowIdx, colIdx))
          for coor <- region do
            newGrid.set(coor.row, coor.col, Some(regionId))
          regionId = regionId + 1

    newGrid
}
