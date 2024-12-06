package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay06Part2Alt {
  def parseDay06Input(filename: String): CharacterBlock2 = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    CharacterBlock2(lines)
  }

  def solvePart2(block: CharacterBlock2): Int =
    val startPosition = block.findCoordinatesOf('^').head
    markVisits(block, startPosition)
    block.print()
    val blockingPositions = block.findCoordinatesOf('X').toList.filter(pos => pos != startPosition)

    val freshBlock = CharacterBlock2(block)
    println()
    val loopingPositions = blockingPositions.map( position => resultsInLoop(CharacterBlock2(freshBlock), position, startPosition))
    val answer = loopingPositions.count( elem => elem )
    answer

  private def markVisits(input: CharacterBlock2, startPosition: (Int, Int)): Unit =
    var rowIdx = startPosition._1
    var colIdx = startPosition._2
    var direction: Direction = Direction.Up
    var finished = false

    while !finished do
      input.setCharAt(rowIdx, colIdx, 'X')
      direction match
        case Direction.Up =>
          val nextRowIdx = rowIdx - 1
          if nextRowIdx < 0 then
            finished = true
          else if input.getCharAt(nextRowIdx, colIdx) == '#' then
            direction = Direction.Right
          else
            rowIdx = nextRowIdx
        case Direction.Right =>
          val nextColIdx = colIdx + 1
          if nextColIdx >= input.getNrOfCols then
            finished = true
          else if input.getCharAt(rowIdx, nextColIdx) == '#' then
            direction = Direction.Down
          else
            colIdx = nextColIdx
        case Direction.Down =>
          val nextRowIdx = rowIdx + 1
          if nextRowIdx >= input.getNrOfRows then
            finished = true
          else if input.getCharAt(nextRowIdx, colIdx) == '#' then
            direction = Direction.Left
          else
            rowIdx = nextRowIdx
        case Direction.Left =>
          val nextColIdx = colIdx - 1
          if nextColIdx < 0 then
            finished = true
          else if input.getCharAt(rowIdx, nextColIdx) == '#' then
            direction = Direction.Up
          else
            colIdx = nextColIdx

  private enum Direction:
    case Up, Right, Down, Left

  private def resultsInLoop(block: CharacterBlock2, blockedPosition: (Int, Int), startPosition: (Int, Int)): Boolean = {
    //println(s"Blocking position (${blockedPosition._1}, ${blockedPosition._2}).")
    var direction = Direction.Up
    var rowIdx = startPosition._1
    var colIdx = startPosition._2
    var visited: List[(Int, Int, Direction)] = Nil
    block.setCharAt(blockedPosition._1, blockedPosition._2, '#')
    var outOfBounds = false
    
    //while !outOfBounds && withinBounds(block, rowIdx, colIdx) do 
    //  move()
    
    def move(): Unit = {
      direction match
        case Direction.Up =>
          println("Moving up.")
          val nextChar = block.safeGetCharAt(rowIdx - 1, colIdx)
          nextChar match
            case Some('#') => direction = Direction.Right
            case None => outOfBounds = true // Out of bounds! We're done moving!
            case _ =>
              visited = (rowIdx, colIdx, Direction.Up) :: visited
              rowIdx = rowIdx - 1
        case Direction.Right =>
          println("Moving right.")
          val nextChar = block.safeGetCharAt(rowIdx, colIdx + 1)
          nextChar match
            case Some('#') => direction = Direction.Down
            case None => outOfBounds = true // Out of bounds! we're done moving!
            case _ =>
              visited = (rowIdx, colIdx, Direction.Right) :: visited
              colIdx = colIdx + 1
        case Direction.Down =>
          println("Moving down.")
          val nextChar = block.safeGetCharAt(rowIdx + 1, colIdx)
          nextChar match
            case Some('#') => direction = Direction.Left
            case None => outOfBounds = true // Out of bounds! We're done moving!
            case _ =>
              visited = (rowIdx, colIdx, Direction.Down) :: visited
              rowIdx = rowIdx + 1
        case Direction.Left =>
          println("Moving left.")
          val nextChar = block.safeGetCharAt(rowIdx, colIdx - 1)
          nextChar match
            case Some('#') => direction = Direction.Up
            case None => outOfBounds = true // Out of bounds! We're done moving!
            case _ =>
              visited = (rowIdx, colIdx, Direction.Left) :: visited
              colIdx = colIdx - 1
    }

    def withinBounds(block: CharacterBlock2, rowIdx: Int, colIdx: Int): Boolean = {
      rowIdx >= 0 && rowIdx < block.getNrOfRows && colIdx >= 0 && colIdx < block.getNrOfCols
    }

    var visitingUniques = true
    while withinBounds(block, rowIdx, colIdx) && visitingUniques do {
      move()
      val positionAndDirection = (rowIdx, colIdx, direction)
      if visited.contains(positionAndDirection) then
        visitingUniques = false
      else
        visited = positionAndDirection :: visited
    }
    !visitingUniques

  }

}
