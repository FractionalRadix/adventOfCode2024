package com.cormontia.adventOfCode2024

import scala.collection.mutable

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

    // Approach: for every element on the path, find the '#' paths that lead to up to 10 blocks away.
    val trackPositions = distanceMap.findAll(pos => distanceMap.get(pos).isDefined)

    var allCheats = List[(Coor, Coor, Long)]()

    for trackPosition <- trackPositions do
      val walls = allWallsWithinNsteps(maze, trackPosition, 5) //TODO!~ N should be 9 (off-by-one thing makes it 10). Set to a lower value for debugging.

      //TODO!- FOR DEBUGGING.
      if trackPosition == Coor(3,1) then
        println(s"Walls surrounding $trackPosition: ${walls.mkString(",")}")
        val copyGrid = Grid(maze)
        for row <- 0 until copyGrid.nrOfRows; col <- 0 until copyGrid.nrOfCols do
          copyGrid.set(row,col,'_')
        for coor <- walls do
          copyGrid.set(coor, 'O')
        copyGrid.print()

      // First, find the direct neighbours of the track position that contain a '#'.
      // These are the start points for our cheat.
      val cheatStarts = directNeighbours(trackPosition)
        .filter( pos => maze.safeGet(pos).contains('#') )

      //println(s"Cheat starts: $cheatStarts")
      //println(s"That's ${cheatStarts.size} candidates.")

      // Second, find the endpoints of our cheats.
      // For this, look at the "wall" elements we found.
      // If any of them has a neighbour that is part of the track, that NEIGHBOUR is an endpoint.
      val cheatEnds = walls
        .flatMap( elt => directNeighbours(elt) )
        .filter( elt => trackPositions.contains(elt) )

      //println(s"Cheat ends: $cheatEnds")
      //println(s"That's ${cheatEnds.size} candidates.")

      for cheatStart <- cheatStarts do
        for cheatEnd <- cheatEnds do
          //println(s"Checking: $cheatStart - $cheatEnd")
          val gain = timeBetween(distanceMap, trackPosition, cheatEnd)
          if gain.isDefined then
            allCheats = (cheatStart, cheatEnd, gain.head) :: allCheats

    // FOR DEBUGGING...
    for cheat <- allCheats do
      if cheat._1 == Coor(4,1) && cheat._2 == Coor(7,3) then
        println(s"Cheat: $cheat")

    //println(s"${allCheats.mkString(",")}")
    val atLeast50 = allCheats.filter((_,_,picoseconds) => picoseconds >= 50)
    atLeast50.length.toString
    // "" //TODO!+
  }

  /**
   * Given a coordinate, give its direct horizontal and vertical neighbours.
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


  private def allWallsWithinNsteps(maze: Grid[Char], pos: Coor, nrOfSteps: Int): Set[Coor] = {

    var initialSet = mutable.Set[Coor]()
    initialSet.add(pos)

    for i <- 1 to nrOfSteps do
      //println(s"Iteration: $i")
      val nextSet = mutable.Set[Coor]()
      //println(s"Size of `initialSet` is ${initialSet.size}")
      for pos <- initialSet do
        val neighbourList = directNeighbours(pos)
          .filter( neighbour => maze.withinBounds(neighbour) )
          .filter( neighbour => !initialSet.contains(neighbour) )
          .filter( neighbour => maze.get(neighbour) == '#')
        //println(s"Neighbours for $pos are ${neighbourList.mkString(",")}")
        nextSet.addAll(neighbourList)

      //initialSet = nextSet
      initialSet.addAll(nextSet)

    initialSet.toSet
  }

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

  private def timeBetween(distanceMap: Grid[Option[Long]], from: Coor, to: Coor): Option[Long] = {
    if !distanceMap.withinBounds(from) || !distanceMap.withinBounds(to) then
      return None
    val n1 = distanceMap.get(from)
    val n2 = distanceMap.get(to)
    if n1.isDefined && n2.isDefined then
      val diff = n2.head - n1.head
      if diff > 0 then
        return Some(diff - 2)
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
        val moves = neighbours(maze, pos).filter( x => !visited.contains(x)) //TODO?+ Keep track of where you've already been?
        for move <- moves do
          stack.push(StackElement(move, elt.cost + 1L))
    }

    (0L, distances) //TODO?~ Should never reach here...
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
