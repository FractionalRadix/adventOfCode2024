package com.cormontia.adventOfCode2024

import scala.collection.mutable

class SolverDay18 extends Solver {
  override def solvePart1(lines: List[String]): Long =
    val grid = determineMaze(lines)

    // Build a grid to keep track of the shortest route to any point.
    val minimalDistances = Grid[Option[Int]](grid.nrOfRows, grid.nrOfCols)
    for row <- 0 until minimalDistances.nrOfRows do
      for col <- 0 until minimalDistances.nrOfCols do
        minimalDistances.set(row, col, None)

    val start = Coor(0, 0)
    val end = Coor(grid.nrOfRows - 1, grid.nrOfCols - 1)
    val stack = mutable.Stack[StackElt]()
    stack.push(StackElt(start, Direction.Right))
    //stack.push(StackElt(start, Direction.Down))
    minimalDistances.set(start, Some(0))
    while stack.nonEmpty do
      val cur = stack.pop()
      move(cur.position, Direction.Right, grid, minimalDistances, end, stack)
      move(cur.position, Direction.Down, grid, minimalDistances, end, stack)
      move(cur.position, Direction.Up, grid, minimalDistances, end, stack)
      move(cur.position, Direction.Left, grid, minimalDistances, end, stack)

    minimalDistances.get(end).head

  private def move(
    position: Coor,
    direction: Direction,
    grid: Grid[Char],
    minimalDistances: Grid[Option[Int]],
    end: Coor,
    stack: mutable.Stack[StackElt]
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
              stack.push(StackElt(neighbour, direction))
          case Some(n) =>
            minimalDistances.set(neighbour, Some(math.min(n, minDist + 1)))
            // Add to the stack but ONLY if the path is shorter.
            if minDist + 1 < n then
              if neighbour != end then
                stack.push(StackElt(neighbour, direction))

  }


  private case class StackElt(position: Coor, direction: Direction)

  private def determineMaze(lines: List[String]): Grid[Char] = {
    // Parse the input to a list of coordinates.
    val corrupted = for line <- lines yield
      val parts = line.split(",")
      Coor(parts(1).toInt /* x = col */ , parts(0).toInt /* y = row */)

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

    // Fill the "corrupted" spaces with '#'
    val blocks = if bigGrid then 1024 else 12
    for coor <- corrupted.take(blocks) do
      grid.set(coor, '#')

    // Return the result
    grid
  }

  override def solvePart2(lines: List[String]): Long =
    0 //TODO!~
}
