package com.cormontia.adventOfCode2024

class CharacterBlock {
  private val block = scala.collection.mutable.Map[(Int, Int), Char]()
  private var maxRow = 0
  private var maxCol = 0

  def this(lines: List[String]) = {
    this()
    //TODO?+ Add an assert that all rows are equally long.
    //TODO?+ Add an assert that all columns are equally long.
    val nrOfRows = lines.length
    val nrOfCols = lines.head.length // Bluntly assuming that all lines are the same length...!
    maxRow = nrOfRows - 1
    maxCol = nrOfCols - 1

    for row <- 0 until nrOfRows do
      for col <- 0 until nrOfCols do
        val ch = lines(row).charAt(col)
        block += (row, col) -> ch
  }

  def print(): Unit = {
    val keys = block.keys

    for rowIdx <- Range(0, maxRow + 1) do
      for colIdx <- Range(0, maxCol + 1) do
        val key = (rowIdx, colIdx)
        val ch = block.getOrElse(key, '.')
        scala.Predef.print(ch)
      println
  }
  
  def getNrOfRows: Int = maxRow + 1
  def getNrOfColumns: Int = maxCol + 1
  def getCharAt(rowIdx: Int, colIdx: Int): Option[Char] = block.get(rowIdx, colIdx)
  def setCharAt(rowIdx: Int, colIdx: Int, ch: Char): Option[Char] = block.put((rowIdx, colIdx), ch)

  def findCoordinatesOf(ch: Character): Seq[(Int, Int)] = {
    for { row <- 0 until getNrOfRows; col <- 0 until getNrOfColumns if getCharAt(row, col).contains(ch) }
        yield (row, col)
  }

  def getRows: List[String] = {
    val rowSequence = for rowIdx <- Range(0, maxRow + 1)
      yield getRow(rowIdx)
    rowSequence.toList
  }

  def getRow(rowIdx: Int): String = {
    val characters = for colIdx <- Range(0, maxCol + 1)
      coor = (rowIdx, colIdx)
      yield block(coor)
    characters.mkString
  }

  def getColumns: List[String] = {
    val colSequence = for colIdx <- Range(0, maxCol + 1)
      yield getCol(colIdx)
    colSequence.toList
  }

  def getCol(colIdx: Int): String = {
    val characters = for rowIdx <- Range(0, maxRow + 1)
      yield block(rowIdx, colIdx)
    characters.mkString
  }

  def getForwardDiagonals: List[String] = {
    val diagonals1 = for rowIdx <- Range(0, maxRow + 1)
      yield forwardDiagonalFromRow(rowIdx)
    val diagonals2 = for colIdx <- Range(1, maxCol + 1)
      yield forwardDiagonalFromColumn(colIdx)
    diagonals1.toList ++ diagonals2.toList
  }

  def forwardDiagonalFromRow(rowIdx: Int): String = {
    //TODO?~ Does this work in blocks that are NOT square?
    var str = ""
    var curColIdx = 0
    for curRowIdx <- Range(rowIdx, maxRow + 1) do
      val pair = (curRowIdx, curColIdx)
      val ch = block(pair)
      str = str + ch
      curColIdx = curColIdx + 1
    str
  }

  def forwardDiagonalFromColumn(colIdx: Int): String = {
    var str = ""
    var curRowIdx = 0
    for curColIdx <- Range(colIdx, maxCol + 1) do
      val pair = (curRowIdx, curColIdx)
      val ch = block(pair)
      str = str + ch
      curRowIdx = curRowIdx + 1
    str
  }

  def getBackwardsDiagonals: List[String] = {
    var diagonals: List[String] = Nil
    
    // First, the diagonals for increasing row numbers.
    // We assume a square block!
    // This makes the length of the first diagonal maxRow/maxCol.
    // Every subsequent diagonal is one less.
    var colIdx = maxCol
    var length = maxCol + 1
    for rowIdx <- Range(0, maxRow + 1) do
      val pos = (rowIdx, colIdx)
      val coordinates = backwardsDiagonal(rowIdx, colIdx, length)
      length = length - 1
      //println(coordinates)
      //println(readFromCoordinates(coordinates))
      val str = readFromCoordinates(coordinates)
      diagonals = str :: diagonals

    // Next, the diagonals for decreasing column numbers.
    // The diagonal for the highest column number is already included in the previous set, so we can skip that one.
    colIdx = maxCol - 1
    length = maxCol
    for colIdx <- maxCol - 1 to 0 by -1 do
      val coordinates = backwardsDiagonal(0, colIdx, length)
      length = length - 1
      val str = readFromCoordinates(coordinates)
      diagonals = str :: diagonals
      
    diagonals  
  }


  /**
   * Yields the fields of a diagonal whose length is `length`, that starts at (`rowIdx`, `colIdx`),
   * and moves downwards and backwards.
   * For example, if we start at (6,6) and have length 3, we get: [(6,6),(7,5),(8,4)]
   */
  def backwardsDiagonal(rowIdx: Int, colIdx: Int, length: Int): List[(Int,Int)] = {
    val pairs = for i <- Range(0, length)
      yield (rowIdx + i, colIdx - i)
    pairs.toList
  }

  /**
   * Given a list of coordinates, read the characters at these coordinates and form them into a String.
   * @param coordinates A list of coordinates in the block.
   * @return The String formed by the characters at these coordinates.
   */
  def readFromCoordinates(coordinates: List[(Int,Int)]): String = {
    val characters = for coordinate <- coordinates
      yield block(coordinate)
    characters.mkString
  }
}
