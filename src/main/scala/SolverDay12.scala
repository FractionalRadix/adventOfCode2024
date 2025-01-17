package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay12 extends Solver {

  override def solvePart1(lines: List[String]): String =
    val grid = Grid[Char](lines, ch => ch)
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
    price.toString

  override def solvePart2(lines: List[String]): String =
    val grid = Grid[Char](lines, ch => ch)
    val newGrid = uniqueRegions(grid)
    val regionIDs = newGrid.findDistinct()
    var totalSum = 0
    for regionID <- regionIDs do
      val plots = newGrid.findCoordinatesOf(regionID).toList
      val sidesAbove = plotsSharingSideAbove(newGrid, regionID.head)
      val sidesBelow = plotsSharingSideBelow(newGrid, regionID.head)
      val sidesLeft = plotsSharingLeftSide(newGrid, regionID.head)
      val sidesRight = plotsSharingRightSide(newGrid, regionID.head)
      val sidesForRegion = sidesAbove.keys.size + sidesBelow.keys.size + sidesLeft.keys.size + sidesRight.keys.size
      val costForRegion = plots.size * sidesForRegion
      totalSum = totalSum + costForRegion
    totalSum.toString

  /**
   * The perimeter of a region is the sum of all sides that DON'T touch another plot in the region.
   * Note that the region may contain "holes", and that the sides of these holes count towards the total.
 *
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

  // ALTERNATE IDEA:
  // Create a new Grid, that also looks at the FENCES.
  // It would be twice as wide and twice as high as the original Grid.
  // Its contents would alternate between regionID's and booleans.
  // A boolean `true` would indicate that that field was a fence, `false` would indicate that it was not.

  /**
   * Identify the sides of a given region on the map.
 *
   * @param newGrid The map of plots, where each region has a unique ID instead of a letter.
   * @param regionID The region ID whose sides we want to identify.
   * @return A mapping of all "above" sides for the given region. It maps side ID's to the plots that participate in
   *         each side.
   */
  private def plotsSharingSideAbove(newGrid: Grid[Option[Int]], regionID: Int): Map[Int, Set[Coor]] =
    val map = scala.collection.mutable.Map[Int, Set[Coor]]() // Map from Side ID's to the elements in them.
    val plots = newGrid
      .findCoordinatesOf(Some(regionID))
      .filter( elt => hasSideAbove(newGrid, elt))
    val plotsByRow = plots.groupBy(_.row)
    var sideNr = 1
    for row <- plotsByRow.keys do
      var orderedPlots = plotsByRow(row).sortBy(_.col)
      while orderedPlots.nonEmpty do
        var cur = orderedPlots.head
        val set = scala.collection.mutable.Set[Coor]()
        // Add "cur" to set, remove it from "orderedPlots".
        // We might just use orderedPlots.drop(1) but this is more robust. Optimize later as necessary.
        set.add(cur)
        orderedPlots = orderedPlots.filter( elt => elt != cur )
        while sameSideAboveGoingInDirection(newGrid, cur, regionID, rightward) do
          val next = rightward(cur) //TODO?+ Verify that "next" is in "orderedPlots" ?
          // Add "next" to set, remove it from "orderedPlots".
          // Again we could use orderedPlots.drop(1) but choose, for now, to use a slower but more robust method.
          set.add(next)
          orderedPlots = orderedPlots.filter( elt => elt != next )
          cur = next
        map(sideNr) = set.toSet
        sideNr = sideNr + 1
    map.toMap

  private def plotsSharingSideBelow(newGrid: Grid[Option[Int]], regionID: Int): Map[Int, Set[Coor]] =
    val map = scala.collection.mutable.Map[Int, Set[Coor]]()
    val plots = newGrid
      .findCoordinatesOf(Some(regionID))
      .filter( elt => hasSideBelow(newGrid, elt) )
    val plotsByRow = plots.groupBy(_.row) //TODO?- Do we even need this?
    var sideNr = 1
    for row <- plotsByRow.keys do
      var orderedPlots = plotsByRow(row).sortBy(_.col)
      while orderedPlots.nonEmpty do
        var cur = orderedPlots.head
        val set = scala.collection.mutable.Set[Coor]()
        // Add "cur" to set, remove it from "orderedPlots".
        // We might just use orderedPlots.drop(1) but this is more robust. Optimize later as necessary.
        set.add(cur)
        orderedPlots = orderedPlots.filter( elt => elt != cur )
        while sameSideBelowGoingInDirection(newGrid, cur, regionID, rightward) do
          val next = rightward(cur) //TODO?+ Verify that "next" is in "orderedPos" ?
          // Add "next to set, remove it from "orderedPlots".
          // Again we could use orderedPlots.drop(1) but choose, for now, to use a slower but more robust method.
          set.add(next)
          orderedPlots = orderedPlots.filter(elt => elt != next)
          cur = next
        map(sideNr) = set.toSet
        sideNr = sideNr + 1
    map.toMap

  private def plotsSharingLeftSide(newGrid: Grid[Option[Int]], regionID: Int): Map[Int, Set[Coor]] =
    val map = scala.collection.mutable.Map[Int, Set[Coor]]()
    val plots = newGrid
      .findCoordinatesOf(Some(regionID))
      .filter( elt => hasSideLeft(newGrid, elt) )
    val plotsByCol = plots.groupBy(_.col) //TODO?~ Do we need this?
    var sideNr = 1
    for col <- plotsByCol.keys do
      var orderedPlots = plotsByCol(col).sortBy(_.row)
      while orderedPlots.nonEmpty do
        var cur = orderedPlots.head
        val set = scala.collection.mutable.Set[Coor]()
        // Add "cur" to set, remove it from "orderedPlots".
        // We might just use orderedPlots.drop(1) but this is more robust. Optimize later as necessary.
        set.add(cur)
        orderedPlots = orderedPlots.filter(elt => elt != cur)
        while sameSideLeftGoingInDirection(newGrid, cur, regionID, downward) do
          val next = downward(cur)
          // Add "next" to set, remove it from "orderedPlots".
          // Again we could use orderedPlots.drop(1) but choose, for now, to use a slower but more robust method.
          set.add(next)
          orderedPlots = orderedPlots.filter(elt => elt != next)
          cur = next
        map(sideNr) = set.toSet
        sideNr = sideNr + 1
    map.toMap

  private def plotsSharingRightSide(newGrid: Grid[Option[Int]], regionID: Int): Map[Int, Set[Coor]] =
    val map = scala.collection.mutable.Map[Int, Set[Coor]]()
    val plots = newGrid
      .findCoordinatesOf(Some(regionID))
      .filter( elt => hasSideRight(newGrid, elt) )
    val plotsByCol = plots.groupBy(_.col) //TODO?~ Do we need this?
    var sideNr = 1
    for col <- plotsByCol.keys do
      var orderedPlots = plotsByCol(col).sortBy(_.row)
      while orderedPlots.nonEmpty do
        var cur = orderedPlots.head
        val set = scala.collection.mutable.Set[Coor]()
        // Add "cur" to set, remove it from "orderedPlots".
        // We might just use orderedPlots.drop(1) but this is more robust. Optimize later as necessary.
        set.add(cur)
        orderedPlots = orderedPlots.filter(elt => elt != cur)
        while sameSideRightGoingInDirection(newGrid, cur, regionID, downward) do
          val next = downward(cur)
          // Add "next" to set, remove it from "orderedPlots".
          // Again we could use orderedPlots.drop(1) but choose, for now, to use a slower but more robust method.
          set.add(next)
          orderedPlots = orderedPlots.filter(elt => elt != next)
          cur = next
        map(sideNr) = set.toSet
        sideNr = sideNr + 1
    map.toMap


  private def rightward: Coor => Coor =
    c => Coor(c.row, c.col + 1)
  private def leftward: Coor => Coor =
    c => Coor(c.row, c.col - 1)
  private def upward: Coor => Coor =
    c => Coor(c.row - 1, c.col)
  private def downward: Coor => Coor =
    c => Coor(c.row + 1, c.col)

  private def hasSideAbove(grid: Grid[Option[Int]], pos: Coor) =
    grid.safeGet(pos.row, pos.col) != grid.safeGet(pos.row - 1, pos.col)

  private def hasSideBelow(grid: Grid[Option[Int]], pos: Coor) =
    grid.safeGet(pos.row, pos.col) != grid.safeGet(pos.row + 1, pos.col)

  private def hasSideRight(grid: Grid[Option[Int]], pos: Coor) =
    grid.safeGet(pos.row, pos.col) != grid.safeGet(pos.row, pos.col + 1)

  private def hasSideLeft(grid: Grid[Option[Int]], pos: Coor) =
    grid.safeGet(pos.row, pos.col) != grid.safeGet(pos.row, pos.col - 1)

  /**
   * Determine if the current position `pos` and its successor are part of the same horizontal top side of region `regionID`
   * This is the case if two conditions hold:
   *  - These two plots both belong to the given region
   *  - The two plots above neither belong to the given region
   *
   * @param grid The grid in which the regions are defined.
   * @param pos The plot whose upper border might be a horizontal side.
   * @param direction The function that gives the coordinates of the neighbour we're interested in (rightward or leftward).
   * @return `true` if and only if the given position and its successor share a side above them.
   */
  private def sameSideAboveGoingInDirection(grid: Grid[Option[Int]], pos: Coor, regionID: Int, direction: Coor => Coor): Boolean =
    val abovePos = Coor(pos.row - 1, pos.col)
    val next = direction(pos)
    val aboveNext = Coor(next.row - 1, next.col)
    val condition1 = grid.safeGet(pos).contains(Some(regionID)) && grid.safeGet(next).contains(Some(regionID))
    val condition2 = !grid.safeGet(abovePos).contains(Some(regionID)) && !grid.safeGet(aboveNext).contains(Some(regionID))
    condition1 && condition2

  /**
   * Determine if the current position `pos` and its successor are part of the same horizontal lower side of region `regionID`
   * This is the case if two conditions hold:
   *  - These two plots both belong to the given region
   *  - The two plots below neither belong to the given region
   *
   * @param grid      The grid in which the regions are defined.
   * @param pos       The plot whose lower border might be a horizontal side.
   * @param direction The function that gives the coordinates of the neighbour we're interested in (rightward or leftward).
   * @return `true` if and only if the given position and its successor share a side below them.
   */
  private def sameSideBelowGoingInDirection(grid: Grid[Option[Int]], pos: Coor, regionID: Int, direction: Coor => Coor): Boolean =
    val belowPos = Coor(pos.row + 1, pos.col)
    val next = direction(pos)
    val aboveNext = Coor(next.row + 1, next.col)
    val condition1 = grid.safeGet(pos).contains(Some(regionID)) && grid.safeGet(next).contains(Some(regionID))
    val condition2 = !grid.safeGet(belowPos).contains(Some(regionID)) && !grid.safeGet(aboveNext).contains(Some(regionID))
    condition1 && condition2

  /**
   * Determine if the current position `pos` and its successor are part of the same vertical left side of region `regionID`
   * This is the case if two conditions hold:
   *  - These two plots both belong to the given region
   *  - The two plots to their left neither belong to the given region
   *
   * @param grid      The grid in which the regions are defined.
   * @param pos       The plot whose left border might be a vertical side.
   * @param direction The function that gives the coordinates of the neighbour we're interested in (upward or downward).
   * @return `true` if and only if the given position and its successor share a side below them.
   */
  private def sameSideLeftGoingInDirection(grid: Grid[Option[Int]], pos: Coor, regionID: Int, direction: Coor => Coor): Boolean =
    val leftOfPos = Coor(pos.row, pos.col - 1)
    val next = direction(pos)
    val leftOfNext = Coor(next.row, next.col - 1)
    val condition1 = grid.safeGet(pos).contains(Some(regionID)) && grid.safeGet(next).contains(Some(regionID))
    val condition2 = !grid.safeGet(leftOfPos).contains(Some(regionID)) && !grid.safeGet(leftOfNext).contains(Some(regionID))
    condition1 && condition2

  /**
   * Determine if the current position `pos` and its successor are part of the same vertical right side of region `regionID`
   * This is the case if two conditions hold:
   *  - These two plots both belong to the given region
   *  - The two plots to their right neither belong to the given region
   *
   * @param grid      The grid in which the regions are defined.
   * @param pos       The plot whose right border might be a vertical side.
   * @param direction The function that gives the coordinates of the neighbour we're interested in (upward or downward).
   * @return `true` if and only if the given position and its successor share a side below them.
   */
  private def sameSideRightGoingInDirection(grid: Grid[Option[Int]], pos: Coor, regionID: Int, direction: Coor => Coor): Boolean =
    val rightOfPos = Coor(pos.row, pos.col + 1)
    val next = direction(pos)
    val rightOfNext = Coor(next.row, next.col + 1)
    val condition1 = grid.safeGet(pos).contains(Some(regionID)) && grid.safeGet(next).contains(Some(regionID))
    val condition2 = !grid.safeGet(rightOfPos).contains(Some(regionID)) && !grid.safeGet(rightOfNext).contains(Some(regionID))
    condition1 && condition2


  //TODO?-
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
      // Determine all the column IDs present in this row.
      val columnIDs = list.filter( coor => coor.row == row ).map( coor => coor.col )
      println(s"   column ID's for row $row: [${columnIDs.mkString(",")}]")
      sum = sum + countContiguousChunks( columnIDs )
    sum

  private def countContiguousVerticalChunks(list: List[Coor]): Int =
    var sum = 0
    val minCol = list.minBy( coor => coor.col ).col
    val maxCol = list.maxBy( coor => coor.col ).col
    for col <- minCol to maxCol do
      // Determine all the row IDs present in this column.
      val rowIDs = list.filter( coor => coor.col == col ).map( coor => coor.row )
      println(s"   row ID's for column $col: [${rowIDs.mkString(",")}]")
      sum = sum + countContiguousChunks( rowIDs )
    sum

  /**
   * Given a list of integers, determine how many contiguous chunks there are in it.
   * For example, [2,3,5,6,7] has two chunks: [2,3] and [5,6,7].
   * As another example, [1,4,5,7,9] has 4 chunks: [1], [4,5], [7], and [9].
   * As an example of an edge case, [8] has 1 chunk: [8]
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
