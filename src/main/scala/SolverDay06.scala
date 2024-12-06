package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay06 {
  def parseDay06Input(filename: String): CharacterBlock = {
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    val block = CharacterBlock(lines)
    block.print()
    block
  }

  def solvePart1(input: CharacterBlock): Int = {
    val startPosition = input.findCoordinatesOf('^').head
    println(startPosition)

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

    val list = for (rowIdx <- 0 until input.getNrOfRows; colIdx <- 0 until input.getNrOfColumns; if input.getCharAt(rowIdx, colIdx).contains('X'))
      yield 1
    val uniquePositionsVisited = list.sum()
    uniquePositionsVisited
  }


  def solvePart2(input: Any): Int = {
    0 //TODO!~ Replace with a solution...
  }
}