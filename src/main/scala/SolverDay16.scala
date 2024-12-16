package com.cormontia.adventOfCode2024

import Direction.{Down, Left, Right, Up}

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

class SolverDay16 extends Solver {
  override def solvePart1(lines: List[String]): Long =
    val map = Grid[Char](lines, ch => ch)
    val startPos = map.findCoordinatesOf('S').head
    // OLD recursive solution that gives a Stack Overflow
    //move(map, startPos, Direction.Right, Set[PositionAndDirection](), 0, 0)
    //lowestScore.get
    val answer = move_iterative(map, startPos)
    println
    answer


  private var lowestScore: Option[Long] = None

  private case class StackElement(
    current: PositionAndDirection,
    visited: Set[PositionAndDirection],
    score: Long
  )

  private def move_iterative(maze: Grid[Char], startPos: Coor): Long =
    val stack = mutable.Stack[StackElement]()
    val start = StackElement(PositionAndDirection(startPos, Direction.Right), Set[PositionAndDirection](), 0)
    var lowestScore: Option[Long] = None

    stack.push(start)
    while stack.nonEmpty do
      print(s"${stack.size} ")
      val currentState = stack.pop()
      val currentPosAndDir = currentState.current
      var nextPositions = availableNeighbours(currentPosAndDir.position, currentPosAndDir.direction)
      nextPositions = nextPositions.filter( pd => !currentState.visited.contains(pd) )
      for nextPosAndDir <- nextPositions do
        val contents = maze.get(nextPosAndDir.position)

        val deltaScore = if nextPosAndDir.direction == currentPosAndDir.direction then 1 else 1001
        val newScore = currentState.score + deltaScore
        if lowestScore.isDefined && newScore >= lowestScore.head then
          ;
        else {
          if contents == 'E' then
            lowestScore = Some(optionMin(newScore, lowestScore))
          else if contents == '.' then
            val nextState = StackElement(nextPosAndDir, currentState.visited + currentPosAndDir, newScore)
            stack.push(nextState)
        }

    lowestScore.get

  /**
   * The minimum of two values, one of which is an Option.
   * The minimum of (v1,None) is v1; the minimum of (v1,Some(v2)) is min(v1,v2)
   * @param v1 The first value to compare
   * @param v2 The possibly empty second value to compare
   * @return The minimum of v1 and v2, if v2 is defined; v1 otherwise.
   */
  def optionMin(v1: Long, v2: Option[Long]): Long =
    if v2.isEmpty then v1 else math.min(v1, v2.get)

  def move(maze: Grid[Char], pos: Coor, direction: Direction, visited: Set[PositionAndDirection], score: Long, recursiveDepth: Int): Long =
    //CRUDE!!!!
    if recursiveDepth > 500 then
      return score

    if lowestScore.isDefined then
      if score > lowestScore.get then
        return score

    println(s"Lowest score so far: $lowestScore")

    //println(s"In $pos  direction=$direction score=$score depth=$recursiveDepth")
    var nextPositions: List[PositionAndDirection] = availableNeighbours(pos, direction)

    nextPositions = nextPositions.filter( pos => !visited.contains(pos) )

    boundary {
      for nextPos <- nextPositions do
        val contents = maze.get(nextPos.position)
        val deltaScore = if nextPos.direction == direction then 1 else 1001
        if contents == 'E' then
          println(s"FOUND IT!!! $score")
          val newScore = score + deltaScore
          if lowestScore.isEmpty then
            lowestScore = Some(newScore)
          else if newScore < lowestScore.get then
            lowestScore = Some(newScore)
          break()
        else if contents == '.' then
          move(maze, nextPos.position, nextPos.direction, visited + PositionAndDirection(pos, direction), score + deltaScore, recursiveDepth + 1)
    }
    score

  private def availableNeighbours(pos: Coor, direction: Direction) = {
    var nextPositions: List[PositionAndDirection] =
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
    nextPositions
  }

  case class PositionAndDirection(position: Coor, direction: Direction)


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



  override def solvePart2(lines: List[String]): Long =
    0 //TODO!+
}