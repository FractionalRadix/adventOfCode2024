package com.cormontia.adventOfCode2024

class SolverDay15 extends Solver {

  private def gridAndMovements(lines: List[String]): (Grid[Char], String) =
    var directions = ""
    var gridLines = List[String]()
    for line <- lines if line.nonEmpty do
      val firstChar = line(0)
      if "<v>^".contains(firstChar) then
        directions = directions ++ line
      else
        gridLines = line :: gridLines
    val grid = Grid[Char](gridLines.reverse, ch => ch)
    (grid, directions)

  private def gridAndMovementsPart2(lines: List[String]): (Grid[Char], String) =
    var directions = ""
    var gridLines = List[String]()
    for line <- lines if line.nonEmpty do
      val firstChar = line(0)
      if "<v>^".contains(firstChar) then
        directions = directions ++ line
      else
        gridLines = doubleLine(line) :: gridLines
    val grid = Grid[Char](gridLines.reverse, ch => ch)
    (grid, directions)

  private def doubleLine(line: String): String =
    var res: String = ""
    for ch <- line do
      res = res + doubleChar(ch)
    res

  private def doubleChar(ch: Char): String =
    ch match
      case '#' => "##"
      case 'O' => "[]"
      case '@' => "@."
      case '.' => ".."
      case _ => println("ERROR IN INPUT!"); "??"

  private var robotPos: Coor = Coor(0,0) // Temporary initial value
  private var grid: Grid[Char] = Grid[Char]()
  private var directions = ""

  override def solvePart1(lines: List[String]): Long =
    val (grid0, directions0) = gridAndMovements(lines)
    grid = grid0
    directions = directions0

    robotPos = grid.findCoordinatesOf('@').head
    for ch <- directions do
      ch match
        case '<' => moveRobot(coor => Coor(coor.row, coor.col - 1))
        case 'v' => moveRobot(coor => Coor(coor.row + 1, coor.col))
        case '>' => moveRobot(coor => Coor(coor.row, coor.col + 1))
        case '^' => moveRobot(coor => Coor(coor.row - 1, coor.col))
        case _ => println("OOPS, invalid instruction!")

    val boxes = grid.findCoordinatesOf('O').toList
    //TODO?~ Find a nice way to do this with a fold.
    //val sum = boxes.fold(0)((acc: Int, coor) => acc + 100 * coor.row + coor.col)
    boxes.map( coor => 100 * coor.row + coor.col ).sum


  private def moveRobot(move: Coor => Coor): Unit =
    val nextPos = move(robotPos)
    if grid.get(nextPos) == '.' then
      grid.set(robotPos, '.')
      grid.set(nextPos, '@')
      robotPos = nextPos
    else if grid.get(nextPos) == '#' then
      ;
    else if grid.get(nextPos) =='O' then
      val firstAvailableSpace = listOfO(grid, nextPos, move)
      if grid.get(firstAvailableSpace) == '.' then
        grid.set(firstAvailableSpace, 'O')
        grid.set(nextPos, '@')
        grid.set(robotPos, '.')
        robotPos = nextPos

  /**
   * Given a grid, a position, and a direction - determine how many 'O's there are on the grid going in the given direction.
   * @param grid A grid of characters.
   * @param firstBox First position of an 'O'
   * @param move A function to determine the position where the next 'O' might be.
   * @return The location of the first element AFTER the last 'O'.
   */
  private def listOfO(grid: Grid[Char], firstBox: Coor, move: Coor => Coor): Coor =
    var endCoor = firstBox
    while grid.get(endCoor) == 'O' do
      endCoor = move(endCoor)
    endCoor

  private def rowOfBigBoxes(grid: Grid[Char], firstBox: Coor, move: Coor => Coor) =
    var endCoor = firstBox
    while "[]".contains(grid.get(endCoor)) do
      endCoor = move(endCoor)
    endCoor

  override def solvePart2(lines: List[String]): Long =
    val (grid0, directions0) = gridAndMovementsPart2(lines)
    grid = grid0
    directions = directions0
    robotPos = grid.findCoordinatesOf('@').head
    for ch <- directions do
      ch match
        case '<' => moveLeft()
        case '>' => moveRight()
        case '^' => moveUp()
        case 'v' => moveDown()
        case _ => println(s"ERROR: Unexpected symbol in input: $ch.")
    val boxes = grid.findCoordinatesOf('[').toList
    //TODO?~ Find a nice way to do this with a fold.
    //val sum = boxes.fold(0)((acc: Int, coor) => acc + 100 * coor.row + coor.col)
    boxes.map( coor => 100 * coor.row + coor.col ).sum

  private def moveUp(): Unit =
    val nextPos = Coor(robotPos.row - 1, robotPos.col)
    if grid.get(nextPos) == '.' then
      grid.set(nextPos, '@')
      grid.set(robotPos, '.')
      robotPos = nextPos
    else if grid.get(nextPos) == '#' then
      ; // Do nothing
    else if canPyramidMove(grid, robotPos, -1) then
      grid = movePyramidVertically(grid, robotPos, -1)

  private def moveDown(): Unit =
    val nextPos = Coor(robotPos.row + 1, robotPos.col)
    if grid.get(nextPos) == '.' then
      grid.set(nextPos, '@')
      grid.set(robotPos, '.')
      robotPos = nextPos
    else if grid.get(nextPos) == '#' then
      ; // Do nothing
    else if canPyramidMove(grid, robotPos, + 1) then
      grid = movePyramidVertically(grid, robotPos, +1)

  private def movePyramidVertically(grid: Grid[Char], startPos: Coor, direction: Int): Grid[Char] =
    val newGrid = Grid(grid)
    // Look at the next row in the "old" grid.
    // Find all boxes that will be pushed up/down.
    // Add these to the list of things to move.
    // Then move these up in the new grid: first make them "."  in the new grid, then move them up.
    var row = startPos.row
    var cols = List(startPos.col)
    val affectedPositions = scala.collection.mutable.Set[Coor]()
    while cols.nonEmpty do
      // Move all relevant boxes up.
      for col <- cols do
        val toMove = grid.get(row,col)
        affectedPositions.add(Coor(row, col))

      // Determine the columns for the next round.
      var newCols: List[Int] = Nil
      for col <- cols do
        val positionAbove = Coor(row + direction, col)
        if grid.get(positionAbove) == '[' then
          newCols = col :: (col + 1) :: newCols
        else if grid.get(positionAbove) == ']' then
          newCols = (col - 1) :: col :: newCols

      // Ready for the next iteration.
      cols = newCols
      row = row + direction

    // Now we should have all positions to move...
    affectedPositions.foreach( c => newGrid.set(c.row, c.col, '.'))
    affectedPositions.foreach( c => newGrid.set(c.row + direction, c.col, grid.get(c)))

    newGrid.set(robotPos, '.')
    newGrid.set(robotPos.row + direction, robotPos.col, '@')
    robotPos = Coor(robotPos.row + direction, robotPos.col)
    newGrid

  private def moveLeft(): Unit =
    val nextPos = Coor(robotPos.row, robotPos.col - 1)
    if grid.get(nextPos) == '.' then
      grid.set(robotPos, '.')
      grid.set(nextPos, '@')
      robotPos = nextPos
    else if grid.get(nextPos) == '#' then
      ; // Do nothing
    else
      val spaceBehindBoxes = rowOfBigBoxes(grid, nextPos, coor => Coor(coor.row, coor.col - 1))
      if grid.get(spaceBehindBoxes) == '.' then
        val leftmostColumn = spaceBehindBoxes.col
        val rightmostColumn = nextPos.col
        // Move the entire row.
        for col <- leftmostColumn to rightmostColumn do
          val neighbour = grid.get(nextPos.row, col + 1)
          grid.set(nextPos.row, col, neighbour)
        grid.set(robotPos, '.')
        grid.set(nextPos, '@')
        robotPos = nextPos

  private def moveRight(): Unit =
    val nextPos = Coor(robotPos.row, robotPos.col + 1)
    if grid.get(nextPos) == '.' then
      grid.set(robotPos, '.')
      grid.set(nextPos, '@')
      robotPos = nextPos
    else if grid.get(nextPos) == '#' then
      ; // Do nothing
    else
      val spaceBehindBoxes = rowOfBigBoxes(grid, nextPos, coor => Coor(coor.row, coor.col + 1))
      if grid.get(spaceBehindBoxes) == '.' then
        val leftmostColumn = nextPos.col
        val rightmostColumn = spaceBehindBoxes.col
        // Move the entire row.
        for col <- rightmostColumn to leftmostColumn by -1 do
          val neighbour = grid.get(nextPos.row, col - 1)
          grid.set(nextPos.row, col, neighbour)
        grid.set(robotPos, '.')
        grid.set(nextPos, '@')
        robotPos = nextPos

  private def canPyramidMove(grid: Grid[Char], robotPos: Coor, direction: Int): Boolean =
    canPyramidMove(grid, robotPos.row, Set(robotPos.col), direction)

  private def canPyramidMove(grid: Grid[Char], row: Int, affectedColumns: Set[Int], direction: Int): Boolean =
    val newColumns = scala.collection.mutable.Set[Int]()
    var result = true
    for col <- affectedColumns do
      val above = Coor(row + direction, col)
      val contentsAbove = grid.get(above)
      contentsAbove match
        case '.' =>
          result = result
        case '#' =>
          result = false
        case '[' =>
          newColumns.add(col)
          newColumns.add(col + 1)
          result = result && canPyramidMove(grid, row + direction, newColumns.toSet, direction)
        case ']' =>
          newColumns.add(col)
          newColumns.add(col - 1)
          result = result && canPyramidMove(grid, row + direction, newColumns.toSet, direction)
    result

}
