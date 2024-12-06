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
    val blockingPositions = block.findCoordinatesOf('X').toList.filter(pos => pos != startPosition)

    val freshBlock = CharacterBlock2(block)
    println()
    val loopingPositions = blockingPositions.map( position => resultsInLoop(CharacterBlock2(freshBlock), position, startPosition))
    val answer = loopingPositions.count( elem => elem )
    answer

  private def markVisits(input: CharacterBlock2, startPosition: (Int, Int)): Unit = {
    var rowIdx = startPosition._1
    var colIdx = startPosition._2

    while (rowIdx >= 0 && rowIdx < input.getNrOfRows && colIdx >= 0 && colIdx < input.getNrOfCols) {
      moveUpward()
      moveRightward()
      moveDownward()
      moveLeftward()
    }

    def moveUpward(): Unit = {
      while rowIdx >= 0 && input.getCharAt(rowIdx, colIdx) != '#' do
        input.setCharAt(rowIdx, colIdx, 'X')
        rowIdx = rowIdx - 1
      if rowIdx >= 0 && input.getCharAt(rowIdx, colIdx) == '#' then
        rowIdx = rowIdx + 1
    }

    def moveDownward(): Unit = {
      while rowIdx < input.getNrOfRows && input.getCharAt(rowIdx, colIdx) != '#' do
        input.setCharAt(rowIdx, colIdx, 'X')
        rowIdx = rowIdx + 1
      if rowIdx < input.getNrOfRows && input.getCharAt(rowIdx, colIdx) == '#' then
        rowIdx = rowIdx - 1
    }

    def moveRightward(): Unit = {
      while colIdx < input.getNrOfCols && input.getCharAt(rowIdx, colIdx) != '#' do
        input.setCharAt(rowIdx, colIdx, 'X')
        colIdx = colIdx + 1
      if colIdx < input.getNrOfCols && input.getCharAt(rowIdx, colIdx) == '#' then
        colIdx = colIdx - 1
    }

    def moveLeftward(): Unit = {
      while colIdx >= 0 && input.getCharAt(rowIdx, colIdx) != '#' do
        input.setCharAt(rowIdx, colIdx, 'X')
        colIdx = colIdx - 1
      if colIdx >= 0 && input.getCharAt(rowIdx, colIdx) == '#' then
        colIdx = colIdx + 1
    }
  }

  private enum Direction:
    case Up, Right, Down, Left

  private def resultsInLoop(block: CharacterBlock2, blockedPosition: (Int, Int), startPosition: (Int, Int)): Boolean = {
    //println(s"Blocking position (${blockedPosition._1}, ${blockedPosition._2}).")
    var direction = Direction.Up
    var rowIdx = startPosition._1
    var colIdx = startPosition._2
    var visited: List[(Int, Int, Direction)] = Nil
    //val block = CharacterBlock(lines)
    block.setCharAt(blockedPosition._1, blockedPosition._2, '#')

    def move(): Unit = {
      direction match
        case Direction.Up =>
          //println("Moving up.")
          if block.getCharAt(rowIdx - 1, colIdx) == '#' then
            direction = Direction.Right
          else
            visited = (rowIdx, colIdx, Direction.Up) :: visited
            block.setCharAt(rowIdx, colIdx, '*')
            rowIdx = rowIdx - 1
        case Direction.Right =>
          //println("Moving right.")
          if block.getCharAt(rowIdx, colIdx + 1) == '#' then
            direction = Direction.Down
          else
            visited = (rowIdx, colIdx, Direction.Right) :: visited
            block.setCharAt(rowIdx, colIdx, '*')
            colIdx = colIdx + 1
        case Direction.Down =>
          //println("Moving down.")
          if block.getCharAt(rowIdx + 1, colIdx) == '#' then
            direction = Direction.Left
          else
            visited = (rowIdx, colIdx, Direction.Down) :: visited
            block.setCharAt(rowIdx, colIdx, '*')
            rowIdx = rowIdx + 1
        case Direction.Left =>
          //println("Moving left.")
          if block.getCharAt(rowIdx, colIdx - 1) == '#' then
            direction = Direction.Up
          else
            visited = (rowIdx, colIdx, Direction.Left) :: visited
            block.setCharAt(rowIdx, colIdx, '*')
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
