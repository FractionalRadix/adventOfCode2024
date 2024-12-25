package com.cormontia.adventOfCode2024

class SolverDay25 extends Solver {
  override def solvePart1(lines: List[String]): String = {

    for line <- lines do
      println(s"    $line")

    val nrOfColumns = lines.head.length

    var grids = List[LockOrKey]()

    var currentLockOrKey = LockOrKey()
    var newLockOrKey = true
    for line <- lines do
      if line.isEmpty then
        grids = grids ++ List(currentLockOrKey)
        currentLockOrKey = new LockOrKey
      else
        currentLockOrKey.addLine(line)
    grids = grids ++ List(currentLockOrKey)

    for grid <- grids do
      val cols = grid.countColumns()
      println(cols)

    val maxHeight = grids.head.maxHeight()
    println(s"Max height is $maxHeight")

    var count = 0
    for lk1 <- grids do
      for lk2 <- grids do
        if lk1.isKey && !lk2.isKey then
          val cols1 = lk1.countColumns()
          val cols2 = lk2.countColumns()
          val sum = cols1.zip(cols2).map((a,b) => a + b)
          if sum.max <= maxHeight then
            count = count + 1

    count.toString

  }

  private class LockOrKey {
    private var lines: List[String] = Nil

    def addLine(line: String): Unit = {
      lines = lines ++ List(line)
    }

    def printLockOrKey(): Unit = {
      for line <- lines do
        println(line)
    }

    def isKey: Boolean = lines.head.head == '.'

    def maxHeight(): Int = lines.length - 2

    def countColumns(): List[Int] = {
      val useLines = if isKey then lines.reverse else lines
      val grid = Grid[Char](lines, ch => ch)
      var columns = List[Int]()
      for col <- 0 until grid.nrOfCols do
        val blocksInColumn = grid.findAll( coor => coor.col == col && grid.get(coor) == '#')
        columns = (blocksInColumn.length - 1) :: columns
      columns.reverse
    }
  }



  override def solvePart2(lines: List[String]): String = {
    //TODO!+
    ""
  }
}
