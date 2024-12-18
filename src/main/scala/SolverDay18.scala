package com.cormontia.adventOfCode2024

import scala.collection.mutable

class SolverDay18 extends Solver {
  override def solvePart1(lines: List[String]): String =
    val blocks = parseLines(lines)
    val grid = buildGrid(blocks)
    if grid.nrOfRows > 7 then
      dropBlocks(grid, blocks.take(1024))
    else
      dropBlocks(grid, blocks.take(12))

    val (minimalDistances: Grid[Option[Int]], end: Coor) = findShortestPath(grid)
    minimalDistances.get(end).head.toString

  override def solvePart2(lines: List[String]): String =
    val corrupted = parseLines(lines)
    val grid = buildGrid(corrupted)
    val first = if grid.nrOfRows > 7 then 1024 else 12
    // Sequentially try all options. This is an inefficient solution.
    // To speed it up, one might consider using binary search.
    var result: Option[Coor] = None
    var size = first - 1
    while result.isEmpty do
      size = size + 1
      dropBlocks(grid, corrupted.take(size))
      val (minimalDistances: Grid[Option[Int]], end: Coor) = findShortestPath(grid)
      if minimalDistances.get(end).isEmpty then
        val coor = corrupted(size-1)
        result = Some(corrupted(size - 1))
    s"${result.head.col},${result.head.row}"

  private def parseLines(lines: List[String]): List[Coor] =
    // Parse the input to a list of coordinates.
    val corrupted = for line <- lines yield
      val parts = line.split(",")
      Coor(parts(1).toInt /* x = col */ , parts(0).toInt /* y = row */)
    corrupted

  private def findShortestPath(grid: Grid[Char]) = {
    // Build a grid to keep track of the shortest route to any point.
    val minimalDistances = Grid[Option[Int]](grid.nrOfRows, grid.nrOfCols)
    for row <- 0 until minimalDistances.nrOfRows do
      for col <- 0 until minimalDistances.nrOfCols do
        minimalDistances.set(row, col, None)

    val start = Coor(0, 0)
    val end = Coor(grid.nrOfRows - 1, grid.nrOfCols - 1)
    val stack = mutable.Stack[Coor]()
    stack.push(start)
    minimalDistances.set(start, Some(0))
    while stack.nonEmpty do
      val cur = stack.pop()
      move(cur, Direction.Right, grid, minimalDistances, end, stack)
      move(cur, Direction.Down, grid, minimalDistances, end, stack)
      move(cur, Direction.Up, grid, minimalDistances, end, stack)
      move(cur, Direction.Left, grid, minimalDistances, end, stack)
    (minimalDistances, end)
  }

  private def move(
    position: Coor,
    direction: Direction,
    grid: Grid[Char],
    minimalDistances: Grid[Option[Int]],
    end: Coor,
    stack: mutable.Stack[Coor]
  ): Unit = {
    val neighbour = direction match
      case Direction.Right => Coor(position.row, position.col + 1)
      case Direction.Left => Coor(position.row, position.col - 1)
      case Direction.Up => Coor(position.row + 1, position.col)
      case Direction.Down => Coor(position.row - 1, position.col)

    val minDist = minimalDistances.get(position).head
    if grid.withinBounds(neighbour) then
      if grid.get(neighbour) == '.' then
        minimalDistances.get(neighbour) match
          case None =>
            minimalDistances.set(neighbour, Some(minDist + 1))
            // Add to the stack
            if neighbour != end then
              stack.push(neighbour)
          case Some(n) =>
            minimalDistances.set(neighbour, Some(math.min(n, minDist + 1)))
            // Add to the stack but ONLY if the path is shorter.
            if minDist + 1 < n then
              if neighbour != end then
                stack.push(neighbour)
  }

  /**
   * Build the grid, but do not yet fill in the "corrupted" spaces.
   * We do pass them to determine how big the grid is.
   * @param corrupted The full list of "corrupted" spaces.
   * @return A grid big enough to fit all the given coordinates.
   */
  private def buildGrid(corrupted: List[Coor]): Grid[Char] = {
    // Determine how big the maze is: 7 x 7 or 71 by 71?
    val maxRow = corrupted.maxBy(coor => coor.row).row
    val maxCol = corrupted.maxBy(coor => coor.col).col
    val bigGrid = maxRow > 6 || maxCol > 6

    // Allocate the grid accordingly.
    val grid = if bigGrid then
      Grid[Char](71, 71)
    else
      Grid[Char](7, 7)

    // Fill the grid with "."
    for row <- 0 until grid.nrOfRows do
      for col <- 0 until grid.nrOfCols do
        grid.set(row, col, '.')

    // Return the result
    grid
  }

  private def dropBlocks(grid: Grid[Char], blocks: Seq[Coor]): Unit =
    for block <- blocks do
      grid.set(block, '#')
}
