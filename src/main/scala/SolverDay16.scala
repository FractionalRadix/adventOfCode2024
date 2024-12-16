package com.cormontia.adventOfCode2024

import Direction.{Down, Left, Right, Up}

import scala.collection.mutable

class SolverDay16 extends Solver {
  override def solvePart1(lines: List[String]): Long =
    val maze = Grid[Char](lines, ch => ch)
    val startPos = maze.findCoordinatesOf('S').head
    val answer = move_iterative(maze, startPos)
    answer

  override def solvePart2(lines: List[String]): Long =
    val maze = Grid[Char](lines, ch => ch)
    val startPos = maze.findCoordinatesOf('S').head
    val endPos = maze.findCoordinatesOf('E').head
    val maxScore = move_iterative(maze, startPos) // The answer to part 1 (!)
    val foundPaths = find_paths(maze, startPos, maxScore)
    println(s"Found ${foundPaths.size} paths.")

    for path <- foundPaths do
      println(s"${path._1} : ${path._2.size} elements.")

    val lowestCost = foundPaths.map( p => p._1 ).min
    println(s"lowest score=$lowestCost")
    val shortestPaths = foundPaths
      .filter( p => p._1 == lowestCost )
      .filter( p => p._2.contains( endPos ) )
    println(s"...${shortestPaths.size} have the lowest cost.")

    val seats = scala.collection.mutable.Set[Coor]()
    for path <- shortestPaths do
      //printSeats(maze, path._2.toSet)
      for pos <- path._2 do
        seats.add(pos)
    //println(s"${seats.mkString(",")}")
    //printSeats(maze, seats.toSet)
    seats.size

  private var lowestScore: Option[Long] = None

  private case class StackElement(
    current: PositionAndDirection,
    score: Long
  )

  private def printSeats(maze: Grid[Char], seats: Set[Coor]): Unit =
    for row <- 0 until maze.nrOfRows do
      for col <- 0 until maze.nrOfRows do
        if seats.contains(Coor(row, col)) then
          print("O")
        else
          val ch = maze.get(row, col)
          print(ch)
      println

  private def move_iterative(maze: Grid[Char], startPos: Coor): Long =
    val stack = mutable.Stack[StackElement]()
    val start = StackElement(PositionAndDirection(startPos, Direction.Right), /*Set[PositionAndDirection](),*/ 0)
    var lowestScore: Option[Long] = None
    val visitedMap = Grid[Option[Long]](maze.nrOfRows, maze.nrOfCols)
    for row <- 0 until visitedMap.nrOfRows; col <- 1 until visitedMap.nrOfCols do
      visitedMap.set(row, col, None)

    stack.push(start)
    while stack.nonEmpty do
      val currentState = stack.pop()
      val currentPosAndDir = currentState.current
      val scoreForPosition = visitedMap.get(currentPosAndDir.position)
      var shouldProcess = false
      if scoreForPosition.isEmpty then
        shouldProcess = true
      else if scoreForPosition.get >= currentState.score then
        shouldProcess = true

      if shouldProcess then
        visitedMap.set(currentState.current.position, Some(currentState.score))

        val nextPositions = availableNeighbours3(currentPosAndDir.position, currentPosAndDir.direction)

        for nextPosAndDir <- nextPositions do
          val contents = maze.get(nextPosAndDir.position)
          val deltaScore = if nextPosAndDir.direction == currentPosAndDir.direction then 1 else 1001
          val newScore = currentState.score + deltaScore

          if contents == 'E' then
             lowestScore = Some(optionMin(newScore, lowestScore))
          else if contents == '.' then
             val nextState = StackElement(nextPosAndDir, newScore)
             stack.push(nextState)
          end if
        end for
      end if

    lowestScore.get

  // New version of Stack Element that also keeps track of the path taken.
  //TODO?~ Does this work given the necessary optimization of keeping track of the lowest value for any position on the maze?
  private case class StackElement2(
    position: Coor,
    direction: Direction,
    score: Long,
    path: List[Coor]
  )

  /**
   * Find all paths that have a score equal to or below the given maximum.
   * @param maze The maze to traverse.
   * @param startPos Starting position on the maze.
   * @return a list of all paths starting at the given start position, ending at a tile marked 'E', whose score does
   *         not exceed the given maximum.
   */
  private def find_paths(maze: Grid[Char], startPos: Coor, maxScore: Long): Set[(Long, List[Coor])] =

    val result = scala.collection.mutable.Set[(Long, List[Coor])]()

    val stack = mutable.Stack[StackElement2]()
    val start = StackElement2(startPos, Direction.Right, 0, Nil)

    stack.push(start)
    while stack.nonEmpty do
      val currentState = stack.pop()
      val currentPos = currentState.position
      val currentDir = currentState.direction

      val shouldProcess = currentState.score < maxScore // + 1001

      if shouldProcess then

        print(s"$currentPos ")
        // Use the "availableNeighbours" that increases the cost the fastest.
        // This way we cut off search paths early.
        val nextPositions = availableNeighbours(currentPos, currentDir)

        for nextPosAndDir <- nextPositions do
          val contents = maze.get(nextPosAndDir.position)
          val deltaScore = if nextPosAndDir.direction == currentDir then 1 else 1001
          val newScore = currentState.score + deltaScore

          if contents == 'E' then
            //lowestScore = Some(optionMin(newScore, lowestScore))
            val pathAndScore = (newScore, nextPosAndDir.position :: currentState.position :: currentState.path)
            result.add(pathAndScore)
          else if contents == '.' then
            val pathSoFar = currentPos :: currentState.path
            val nextState = StackElement2(nextPosAndDir.position, nextPosAndDir.direction, newScore, pathSoFar )
            stack.push(nextState)
          end if
        end for
      end if

    result.toSet


  /**
   * The minimum of two values, one of which is an Option.
   * The minimum of (v1,None) is v1; the minimum of (v1,Some(v2)) is min(v1,v2)
   * @param v1 The first value to compare
   * @param v2 The possibly empty second value to compare
   * @return The minimum of v1 and v2, if v2 is defined; v1 otherwise.
   */
  private def optionMin(v1: Long, v2: Option[Long]): Long =
    if v2.isEmpty then v1 else math.min(v1, v2.get)

  // Like `availableNeighbours(...)`, but we always try to go to the right first, up second, down third, left last.
  private def availableNeighbours2(pos: Coor, direction: Direction) =
      direction match
        case Down => List(
          PositionAndDirection(neighbour(pos, Right), Right),
          PositionAndDirection(neighbour(pos, Down), Down),
          PositionAndDirection(neighbour(pos, Left), Left),
        )
        case Up => List(
          PositionAndDirection(neighbour(pos, Right), Right),
          PositionAndDirection(neighbour(pos, Up), Up),
          PositionAndDirection(neighbour(pos, Left), Left),
        )
        case Left => List(
          PositionAndDirection(neighbour(pos, Up), Up),
          PositionAndDirection(neighbour(pos, Down), Down),
          PositionAndDirection(neighbour(pos, Left), Left),
        )
        case Right => List(
          PositionAndDirection(neighbour(pos, Right), Right),
          PositionAndDirection(neighbour(pos, Up), Up),
          PositionAndDirection(neighbour(pos, Down), Down)
        )

  // Like `availableNeighbours2(...)`, but in reverse: left first, down second, up third, right last.
  // This is because the iterative version reverses it: it puts the elements on a stack. Last element on the stack
  // will be first to be tried out. Hence, if we prioritize "going rightwards", the rightwards frame should be last,
  // so it ends up being on top of the stack.
  private def availableNeighbours3(pos: Coor, direction: Direction) =
    direction match
      case Down => List(
        PositionAndDirection(neighbour(pos, Left), Left),
        PositionAndDirection(neighbour(pos, Down), Down),
        PositionAndDirection(neighbour(pos, Right), Right),
      )
      case Up => List(
        PositionAndDirection(neighbour(pos, Left), Left),
        PositionAndDirection(neighbour(pos, Up), Up),
        PositionAndDirection(neighbour(pos, Right), Right),
      )
      case Left => List(
        PositionAndDirection(neighbour(pos, Left), Left),
        PositionAndDirection(neighbour(pos, Down), Down),
        PositionAndDirection(neighbour(pos, Up), Up),
      )
      case Right => List(
        PositionAndDirection(neighbour(pos, Down), Down),
        PositionAndDirection(neighbour(pos, Up), Up),
        PositionAndDirection(neighbour(pos, Right), Right),
      )

  private def availableNeighbours(pos: Coor, direction: Direction) = {
      direction match
        case Down => List(
          PositionAndDirection(neighbour(pos, Down), Down),
          PositionAndDirection(neighbour(pos, Left), Left),
          PositionAndDirection(neighbour(pos, Right), Right)
        )
        case Up => List(
          PositionAndDirection(neighbour(pos, Up), Up),
          PositionAndDirection(neighbour(pos, Right), Right),
          PositionAndDirection(neighbour(pos, Left), Left)
        )
        case Left => List(
          PositionAndDirection(neighbour(pos, Left), Left),
          PositionAndDirection(neighbour(pos, Up), Up),
          PositionAndDirection(neighbour(pos, Down), Down)
        )
        case Right => List(
          PositionAndDirection(neighbour(pos, Right), Right),
          PositionAndDirection(neighbour(pos, Up), Up),
          PositionAndDirection(neighbour(pos, Down), Down)
        )
  }

  private case class PositionAndDirection(position: Coor, direction: Direction)

  /**
   * Given a position and a direction, determine the next position in that direction.
   * For example, "Up" for (row=3,col=4) yields (row=2, col=4).
   * @param coor The starting position.
   * @param direction The direction.
   * @return The neighbour of the given position in the given direction.
   */
  def neighbour(coor: Coor, direction: Direction): Coor =
    direction match
      case Up => Coor(coor.row - 1, coor.col)
      case Down => Coor(coor.row + 1, coor.col)
      case Right => Coor(coor.row, coor.col + 1)
      case Left => Coor(coor.row, coor.col - 1)
}