package com.cormontia.adventOfCode2024

import Direction.{Down, Left, Right, Up}

import scala.util.boundary
import scala.util.boundary.break

class SolverDay16 extends Solver {
  override def solvePart1(lines: List[String]): Long =
    val map = Grid[Char](lines, ch => ch)
    val startPos = map.findCoordinatesOf('S').head
    val endPos = map.findCoordinatesOf('E').head
    move(map, startPos, Direction.Right, Set[PositionAndDirection](), 0)
    println(s"scores: ${scores.mkString(",")}")
    scores.min()

  val scores = scala.collection.mutable.Set[Long]()

  def move(maze: Grid[Char], pos: Coor, direction: Direction, visited: Set[PositionAndDirection], score: Long): Long =
    println(s"In $pos  direction=$direction score=$score")
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

    nextPositions = nextPositions.filter( pos => !visited.contains(pos) )
    //println(s"Candidates: ${nextPositions.mkString(",")}")

    boundary {
      for nextPos <- nextPositions do
        //println(s"...Trying next: $nextPos")
        val contents = maze.get(nextPos.position)
        val deltaScore = if nextPos._1 == direction then 1 else 1001
        if contents == 'E' then
          println(s"FOUND IT!!! $score")
          //TODO?+ Add the last step to the score?
          scores.add(score)
          break()
        else if contents == '.' then
          //println(s"...Going to: $nextPos")
          move(maze, nextPos.position, nextPos.direction, visited + PositionAndDirection(pos, direction), score + deltaScore)
          //println(s"...Back from: $nextPos")
    }
    println(s"Returning with score: $score")
    score

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