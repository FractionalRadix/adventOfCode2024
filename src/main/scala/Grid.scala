package com.cormontia.adventOfCode2024

import scala.reflect.ClassTag

//class Grid[T](using m: ClassTag[T]) {
class Grid[T: ClassTag] {

  /**
   * The array that contains our block.
   * Initialized to a dummy value so we can make it private.
   */
  private var arr = Array.ofDim[T](2, 2)

  private var _nrOfRows = 0
  def nrOfRows: Int = _nrOfRows
  private var _nrOfCols = 0
  def nrOfCols: Int = _nrOfCols

  def this(nrOfRows: Int, nrOfCols: Int) =
    this()
    this._nrOfRows = nrOfRows
    this._nrOfCols = nrOfCols
    arr = Array.ofDim[T](_nrOfRows, _nrOfCols)

  def this(lines: List[String], transform: Char => T) =
    // PRE: at least one line
    // PRE: all lines equally long
    this()
    this._nrOfRows = lines.size
    this._nrOfCols = lines.head.length
    arr = Array.ofDim[T](_nrOfRows, _nrOfCols)
    var rowIdx = 0
    for line <- lines do
      var colIdx = 0
      for ch <- line do
        arr(rowIdx)(colIdx) = transform(ch)
        colIdx = colIdx + 1
      rowIdx = rowIdx + 1

  def this(source: Grid[T]) =
    this()
    this._nrOfRows = source._nrOfRows
    this._nrOfCols = source._nrOfCols
    arr = Array.ofDim[T](_nrOfRows, _nrOfCols)
    for rowIdx <- 0 until _nrOfRows; colIdx <- 0 until _nrOfCols do
      this.arr(rowIdx)(colIdx) = source.arr(rowIdx)(colIdx)

  def print(): Unit =
    for (rowIdx <- 0 until _nrOfRows) do
      for (colIdx <- 0 until _nrOfCols) do
        val ch = arr(rowIdx)(colIdx)
        Predef.print(ch)
      println

  def findCoordinatesOf(elt: T): Seq[Coor] =
    for rowIdx <- 0 until _nrOfRows; colIdx <- 0 until _nrOfCols if arr(rowIdx)(colIdx) == elt
      yield Coor(rowIdx, colIdx)

  def withinBounds(row: Int, col: Int): Boolean =
    0 <= row && row < nrOfRows && 0 <= col && col < nrOfCols

  def withinBounds(coordinate: Coor): Boolean =
    0 <= coordinate.row && coordinate.row < nrOfRows && 0 <= coordinate.col && coordinate.col < nrOfCols

  /**
   * Get the element at the position (rowIdx, colIdx).
   * This is the safe but slow method - it checks the bounds.
   * @param rowIdx Row index of the character, 0-based.
   * @param colIdx Column index of the character, 0-based.
   * @return The element at column "colIdx" of row "rowIdx", or None if the given position is out of bounds.
   */
  def safeGet(rowIdx: Int, colIdx: Int): Option[T] =
    if 0 <= rowIdx && rowIdx < _nrOfRows && 0 <= colIdx && colIdx < _nrOfCols then
      Some(arr(rowIdx)(colIdx))
    else
      None

  /**
   * Get the element at the position (rowIdx, colIdx).
   * This is the safe but slow method - it checks the bounds.
   * @param coor The (row, column) pair that locates the value. Note that row- and column-numbers are 0-based.
   * @return The element at column "colIdx" of row "rowIdx", or None if the given position is out of bounds.
   */
  def safeGet(coor: Coor): Option[T] =
    if 0 <= coor.row && coor.row < _nrOfRows && 0 <= coor.col && coor.col < _nrOfCols then
      Some(arr(coor.row)(coor.col))
    else
      None

  /**
   * Get the element at the position (rowIdx, colIdx).
   * This is the fastest method because it does not check the bounds.
   * It is the caller's responsibility to make sure that the position exists on this block!
   * @param rowIdx Row index of the element, 0-based.
   * @param colIdx Column index of the element, 0-based.
   * @return The element at column "colIdx" of row "rowIdx".
   */
  def get(rowIdx: Int, colIdx: Int): T = arr(rowIdx)(colIdx)

  /**
   * Get the element at the position (rowIdx, colIdx).
   * This is the fastest method because it does not check the bounds.
   * It is the caller's responsibility to make sure that the position exists on this block!
   * @param coor The (row, column) pair that locates the value. Note that row- and column-numbers are 0-based.
   * @return The element at column "colIdx" of row "rowIdx".
   */
  def get(coor: Coor): T = arr(coor.row)(coor.col)

  def set(rowIdx: Int, colIdx: Int, elt: T): Unit =
    arr(rowIdx)(colIdx) = elt

  def set(coor: Coor, elt: T): Unit =
    arr(coor.row)(coor.col) = elt
  
  /**
   * Find the distinct elements in the grid.
   * For example, if the grid consisted of the rows 'zzA', 'zAz', and 'zzA', it would return {'z', 'A'}.
   * Note that this requires our value to have an equality operator.
   * @return A set of all the distinct elements in the list.
   */
  def findDistinct(): Set[T] =
    val mutableSet = scala.collection.mutable.Set[T]()
    for (rowIdx <- 0 until _nrOfRows) do
      for (colIdx <- 0 until _nrOfCols) do
        val elt = arr(rowIdx)(colIdx)
        mutableSet += elt
    mutableSet.toSet

  /**
   * Find all coordinates in the grid that satisfy a user-supplied predicate.
   * @param predicate The function that determines if a specific coordinate belongs to the result.
   * @return The list of all coordinates c for which `predicate(c)` returns `true`.
   */
  def findAll(predicate: Coor => Boolean): List[Coor] =
    var result: List[Coor] = Nil
    for (rowIdx <- 0 until _nrOfRows) do
      for (colIdx <- 0 until _nrOfCols) do
        val coor = Coor(rowIdx, colIdx)
        if predicate(coor) then
          result = coor :: result
    result
}
