package com.cormontia.adventOfCode2024

class SolverDay06 extends Solver {

  override def solvePart1(lines: List[String]): Long = {
    val block = CharacterBlock(lines)
    val startPosition = block.findCoordinatesOf('^').head

    markVisits(block, startPosition)

    val list = for (rowIdx <- 0 until block.getNrOfRows; colIdx <- 0 until block.getNrOfColumns; if block.getCharAt(rowIdx, colIdx).contains('X'))
      yield 1
    val uniquePositionsVisited = list.sum()
    uniquePositionsVisited
  }

  private def markVisits(input: CharacterBlock, startPosition: (Int, Int)): Unit = {
    var rowIdx = startPosition._1
    var colIdx = startPosition._2
    var distance = 0

    while (rowIdx >= 0 && rowIdx < input.getNrOfRows && colIdx >= 0 && colIdx < input.getNrOfColumns) {
      moveUpward()
      moveRightward()
      moveDownward()
      moveLeftward()
    }

    def moveUpward(): Unit = {
      while !input.getCharAt(rowIdx, colIdx).contains('#') && rowIdx >= 0 do
        input.setCharAt(rowIdx, colIdx, 'X')
        rowIdx = rowIdx - 1
        distance = distance + 1
      if input.getCharAt(rowIdx, colIdx).contains('#') then
        rowIdx = rowIdx + 1
        distance = distance - 1
    }

    def moveDownward(): Unit = {
      while !input.getCharAt(rowIdx, colIdx).contains('#') && rowIdx <= input.getNrOfRows do
        input.setCharAt(rowIdx, colIdx, 'X')
        rowIdx = rowIdx + 1
        distance = distance + 1
      if input.getCharAt(rowIdx, colIdx).contains('#') then
        rowIdx = rowIdx - 1
        distance = distance - 1
    }

    def moveRightward(): Unit = {
      while !input.getCharAt(rowIdx, colIdx).contains('#') && colIdx <= input.getNrOfColumns do
        input.setCharAt(rowIdx, colIdx, 'X')
        colIdx = colIdx + 1
        distance = distance + 1
      if input.getCharAt(rowIdx, colIdx).contains('#') then
        colIdx = colIdx - 1
        distance = distance - 1
    }

    def moveLeftward(): Unit = {
      while !input.getCharAt(rowIdx, colIdx).contains('#') && colIdx >= 0 do
        input.setCharAt(rowIdx, colIdx, 'X')
        colIdx = colIdx - 1
        distance = distance + 1
      if input.getCharAt(rowIdx, colIdx).contains('#') then
        colIdx = colIdx + 1
        distance = distance - 1
    }
  }

  override def solvePart2(lines: List[String]): Long = {
    val block = CharacterBlock(lines)
    val startPosition = block.findCoordinatesOf('^').head
    markVisits(block, startPosition)
    val blockingPositions = block.findCoordinatesOf('X').toList.filter(pos => pos != startPosition)

    val freshBlock  = CharacterBlock(lines)
    println()
    val loopingPositions = blockingPositions.map( position => resultsInLoop(CharacterBlock(freshBlock), position, startPosition))
    val answer = loopingPositions.count( elem => elem )
    answer
  }

  private def resultsInLoop(block: CharacterBlock, blockedPosition: (Int, Int), startPosition: (Int, Int)): Boolean = {
    //println(s"Blocking position (${blockedPosition._1}, ${blockedPosition._2}).")
    var direction = Direction.Up
    var rowIdx = startPosition._1
    var colIdx = startPosition._2
    //var visited: List[(Int, Int, Direction)] = Nil
    val visited = scala.collection.mutable.Set[(Int, Int, Direction)]()
    block.setCharAt(blockedPosition._1, blockedPosition._2, '#')

    def move(): Unit = {
      direction match
        case Direction.Up =>
          //println("Moving up.")
          if block.getCharAt(rowIdx - 1, colIdx).contains('#') then
            direction = Direction.Right
          else
            val triple = (rowIdx, colIdx, Direction.Up)
            visited += triple // With Java Sets it would tell you right away if it was contained already...
            rowIdx = rowIdx - 1
        case Direction.Right =>
          //println("Moving right.")
          if block.getCharAt(rowIdx, colIdx + 1).contains('#') then
            direction = Direction.Down
          else
            val triple = (rowIdx, colIdx, Direction.Right)
            visited += triple
            colIdx = colIdx + 1
        case Direction.Down =>
          //println("Moving down.")
          if block.getCharAt(rowIdx + 1, colIdx).contains('#') then
            direction = Direction.Left
          else
            val triple = (rowIdx, colIdx, Direction.Down)
            visited += triple
            rowIdx = rowIdx + 1
        case Direction.Left =>
          //println("Moving left.")
          if block.getCharAt(rowIdx, colIdx - 1).contains('#') then
            direction = Direction.Up
          else
            val triple = (rowIdx, colIdx, Direction.Left)
            visited += triple
            colIdx = colIdx - 1
    }

    def withinBounds(block: CharacterBlock, rowIdx: Int, colIdx: Int): Boolean = {
      rowIdx >= 0 && rowIdx < block.getNrOfRows && colIdx >= 0 && colIdx < block.getNrOfColumns
    }

    var visitingUniques = true
    while withinBounds(block, rowIdx, colIdx) && visitingUniques do {
      move()
      val positionAndDirection = (rowIdx, colIdx, direction)
      if visited.contains(positionAndDirection) then
        visitingUniques = false
      else
        visited += positionAndDirection
    }
    !visitingUniques

  }
}