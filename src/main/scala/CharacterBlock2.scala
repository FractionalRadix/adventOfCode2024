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
  def getNrOfRows: Int = nrOfRows
  private var nrOfCols = 0
  def getNrOfCols: Int = nrOfCols

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

  def this(source: CharacterBlock2) =
    this()
    this.nrOfRows = source.nrOfRows
    this.nrOfCols = source.nrOfCols
    arr = Array.ofDim[Char](nrOfRows, nrOfCols)
    for rowIdx <- 0 until nrOfRows; colIdx <- 0 until nrOfCols do
      this.arr(rowIdx)(colIdx) = source.arr(rowIdx)(colIdx)

  def print(): Unit =
    for (rowIdx <- 0 until nrOfRows) do
      for (colIdx <- 0 until nrOfCols) do
        val ch = arr(rowIdx)(colIdx)
        Predef.print(ch)
      println

  def findCoordinatesOf(ch: Char): Seq[(Int, Int)] =
    for rowIdx <- 0 until nrOfRows; colIdx <- 0 until nrOfCols if arr(rowIdx)(colIdx) == ch
      yield (rowIdx, colIdx)

  /**
   * Get the character at the position (rowIdx, colIdx).
   * This is the safe but slow method - it checks the bounds.
   * @param rowIdx Row index of the character, 0-based.
   * @param colIdx Column index of the character, 0-based.
   * @return The character at column "colIdx" of row "rowIdx", or None if the given position is out of bounds.
   */
  def safeGetCharAt(rowIdx: Int, colIdx: Int): Option[Char] =
    if 0 <= rowIdx && rowIdx < nrOfRows && 0 <= colIdx && colIdx < nrOfCols then
      Some(arr(rowIdx)(colIdx))
    else
      None

  /**
   * Get the character at the position (rowIdx, colIdx).
   * This is the fastest method because it does not check the bounds.
   * It is the caller's responsibility to make sure that the position exists on this block!
   * @param rowIdx Row index of the character, 0-based.
   * @param colIdx Column index of the character, 0-based.
   * @return The character at column "colIdx" of row "rowIdx".
   */
  def getCharAt(rowIdx: Int, colIdx: Int): Char = arr(rowIdx)(colIdx)

  def setCharAt(rowIdx: Int, colIdx: Int, ch: Char): Unit =
    arr(rowIdx)(colIdx) = ch
}