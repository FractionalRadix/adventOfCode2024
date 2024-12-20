package com.cormontia.adventOfCode2024

import scala.collection.mutable
import scala.math.abs

class SolverDay20 extends Solver {
  override def solvePart1(lines: List[String]): String = {
    val maze = Grid[Char](lines, ch => ch)

    val (baseCost, distanceMap) = runMaze(maze)
    val endPos = maze.findCoordinatesOf('E').head

    // Next: for every element on the distance map, find its (up to) four two-step neighbours.
    // For every neighbour that is on the path, IF it is higher, determine the difference
    // This difference is subtracted from the baseline, but two is added to the cost.

    val cheatCount = mutable.Map[Long, Int]()
    val visited = distanceMap.findAll( c => distanceMap.get(c).isDefined )
    for pos <- visited do

      val up = timeSaved(maze, distanceMap, pos, Direction.Up)
      if up.isDefined then
        if cheatCount.contains(up.head) then cheatCount(up.head) = cheatCount(up.head) + 1 else cheatCount(up.head) = 1

      val down = timeSaved(maze, distanceMap, pos, Direction.Down)
      if down.isDefined then
        if cheatCount.contains(down.head) then cheatCount(down.head) = cheatCount(down.head) + 1 else cheatCount(down.head) = 1

      val right = timeSaved(maze, distanceMap, pos, Direction.Left)
      if right.isDefined then
        if cheatCount.contains(right.head) then cheatCount(right.head) = cheatCount(right.head) + 1 else cheatCount(right.head) = 1

      val left = timeSaved(maze, distanceMap, pos, Direction.Right)
      if left.isDefined then
        if cheatCount.contains(left.head) then cheatCount(left.head) = cheatCount(left.head) + 1 else cheatCount(left.head) = 1

    // cheatCount maps, for every "time saved", how many cheats have it.
    var sum = 0L
    for (timeSaved, howOften) <- cheatCount do
      if timeSaved >= 100 then
        sum = sum + howOften

    sum.toString
  }

  override def solvePart2(lines: List[String]): String = {
    val maze = Grid[Char](lines, ch => ch)
    val (baseCost, distanceMap) = runMaze(maze)
    val trackPositions = distanceMap.findAll(pos => distanceMap.get(pos).isDefined)

    var pairs = List[(Coor, Coor)]()
    for startPos <- trackPositions do
      val startValue = distanceMap.get(startPos).head
      for endPos <- trackPositions do
        val endValue = distanceMap.get(endPos).head
        if endValue > startValue /*&& manhattanDistance(startPos, endPos) <= 120*/ then
          pairs = (startPos, endPos) :: pairs

    var allCheats = List[(Coor, Coor, Long)]()
    for pair <- pairs do
      val originalCost = distanceMap.get(pair._2).head - distanceMap.get(pair._1).head
      val newCost = manhattanDistance(pair._1, pair._2)
      if newCost <= 20 then
        allCheats = (pair._1, pair._2, originalCost - newCost) :: allCheats

    // For verification. Note that in the example, the maximum cost of 20 was NOT considered...
    // You have to remove the "newCost <= 20" to get the values shown in the example....
    //for exactCost <- List(50,52,54,56,58,60,62,64,66,68,70,72,74,76) do
    //  println(s"Nr of cheats that save exactly $exactCost picoseconds: ${allCheats.count((_, _, cost) => cost == exactCost)}")

    val answer = allCheats.count((_,_,saved) => saved >= 100)
    answer.toString
  }

  //TODO?~ Move to Util?
  private def manhattanDistance(pos1: Coor, pos2: Coor): Long = abs(pos1.row - pos2.row) + abs(pos1.col - pos2.col)

  private def printDistanceMap(distanceMap: Grid[Option[Long]]): Unit = {
    //FOR DEBUGGING...
    for row <- 0 until distanceMap.nrOfRows do
      println()
      for col <- 0 until distanceMap.nrOfCols do
        if distanceMap.get(row, col).isDefined then
          print(String.format("%02d|", distanceMap.get(row, col).head))
        else
          print("__|")
  }

  private def printFoundWalls(maze: Grid[Char], trackPosition: Coor, walls: Set[Coor]): Unit = {
    println(s"Walls surrounding $trackPosition: ${walls.mkString(",")}")
    val copyGrid = Grid(maze)
    for row <- 0 until copyGrid.nrOfRows; col <- 0 until copyGrid.nrOfCols do
      copyGrid.set(row, col, '_')
    for coor <- walls do
      copyGrid.set(coor, 'O')
    copyGrid.print()
  }

  /**
   * Given a coordinate, give its direct horizontal and vertical neighbours.
   *
   * @param pos A coordinate (row, column) on a grid.
   * @return A list containing the lower neighbour, upper neighbour, left neighbour, and right neighbour.
   */
  private def directNeighbours(pos: Coor): List[Coor] =
    List(
      Coor(pos.row - 1, pos.col),
      Coor(pos.row + 1, pos.col),
      Coor(pos.row, pos.col - 1),
      Coor(pos.row, pos.col + 1)
    )

  private def timeSaved(maze: Grid[Char], distanceMap: Grid[Option[Long]], curPos: Coor, direction: Direction): Option[Long] = {
    val (n1, n2) = direction match
      case Direction.Up => (Coor(curPos.row - 1, curPos.col), Coor(curPos.row - 2,curPos.col))
      case Direction.Down => (Coor(curPos.row + 1, curPos.col), Coor(curPos.row + 2,curPos.col))
      case Direction.Left => (Coor(curPos.row, curPos.col - 1), Coor(curPos.row, curPos.col - 2))
      case Direction.Right => (Coor(curPos.row, curPos.col + 1), Coor(curPos.row, curPos.col + 2))

    if !maze.withinBounds(n1) || !maze.withinBounds(n2) then
      return None
    if maze.get(n1) != '#' then
      return None

    if distanceMap.get(n2).isDefined then
      val nextValue = distanceMap.get(n2).head
      val posValue = distanceMap.get(curPos).head
      if nextValue > posValue then
        val timeSaved = nextValue - posValue - 2
        //println(s"Cheat found: $curPos, $n2 saves $timeSaved")
        return Some(timeSaved)
    None
  }

  private def distanceMap(grid: Grid[Char]): Grid[Option[Long]] = {
    val result = Grid[Option[Long]](grid.nrOfRows, grid.nrOfCols)
    for row <- 0 until result.nrOfRows; col <- 0 until result.nrOfCols do
      result.set(row, col, None)
    result
  }

  private def runMaze(maze: Grid[Char]): (Long, Grid[Option[Long]]) = {
    val startPos = maze.findCoordinatesOf('S').head
    val endPos = maze.findCoordinatesOf('E').head

    val distances = distanceMap(maze)

    val visited = mutable.Set[Coor]()
    val stack = mutable.Stack[StackElement]()
    var pos = startPos
    stack.push(StackElement(startPos,0))
    while pos != endPos do {
      val elt = stack.pop()
      pos = elt.pos
      distances.set(pos, Some(elt.cost))
      visited.add(pos)
      if maze.get(elt.pos) == 'E' then
        return (elt.cost, distances)
      else
        val moves = neighbours(maze, pos).filter( x => !visited.contains(x))
        for move <- moves do
          stack.push(StackElement(move, elt.cost + 1L))
    }

    (0L, distances) // Should never reach here...
  }

  private case class StackElement(pos: Coor, cost: Long)

  private def neighbours(maze: Grid[Char], position: Coor): List[Coor] = {
    val list = List[Coor](
      Coor(position.row, position.col + 1),
      Coor(position.row - 1, position.col),
      Coor(position.row, position.col - 1),
      Coor(position.row + 1, position.col),
    )

    list.filter(pos => maze.get(pos) == '.' || maze.get(pos) == 'E')
  }


}
