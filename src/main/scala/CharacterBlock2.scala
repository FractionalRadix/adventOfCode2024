package com.cormontia.adventOfCode2024

/**
 * A block of characters, implemented using an array.
 */
class CharacterBlock2 {

  /**
   * The array that contains our block.
   * Initialized to a dummy value so we can make it private.
   */
  private var arr = Array.ofDim[Char](2, 2)

  private var nrOfRows = 0
  private var nrOfCols = 0

  def this(lines: List[String]) =
    // PRE: at least one line
    // PRE: all lines equally long
    this()
    this.nrOfRows = lines.size
    this.nrOfCols = lines.head.length
    arr = Array.ofDim[Char](nrOfRows, nrOfCols)
    var rowIdx = 0
    for line <- lines do
      var colIdx = 0
      for ch <- line do
        arr(rowIdx)(colIdx) = ch
        colIdx = colIdx + 1
      rowIdx = rowIdx + 1

  def print(): Unit =
    for (rowIdx <- 0 until nrOfRows) do
      for (colIdx <- 0 until nrOfCols) do
        val ch = arr(rowIdx)(colIdx)
        Predef.print(ch)
      println

  def findCoordinatesOf(ch: Char): Seq[(Int, Int)] =
    for rowIdx <- 0 until nrOfRows; colIdx <- 0 until nrOfCols if arr(rowIdx)(colIdx) == ch
      yield (rowIdx, colIdx)

  def getCharAt(rowIdx: Int, colIdx: Int): Char = arr(rowIdx)(colIdx)
  
  def setCharAt(rowIdx: Int, colIdx: Int, ch: Char): Unit =
    arr(rowIdx)(colIdx) = ch  
}