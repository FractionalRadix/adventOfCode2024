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

    println("GRID:")
    grid.print()
    println("INSTRUCTIONS:")
    println(directions)
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

    println("GRID:")
    grid.print()
    println("INSTRUCTIONS:")
    println(directions)
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
    //TODO!~ Find a nice way to do this with a fold.
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

  override def solvePart2(lines: List[String]): Long =
    val (grid0, directions0) = gridAndMovementsPart2(lines)
    grid = grid0
    directions = directions0
    grid.print()
    0 //TODO!~

  /**
   * Can a set of boxes be moved vertically?
   * @param grid The grid to inspect
   * @param robotPos Position of the robot that moves the boxes. Or of a box being pushed BY the robot, directly or indirectly.
   * @param direction The direction in which to move - upwards (-1) or downwards (+1)
   * @return <code>true</code> if and only if the boxes can be moved in the given direction.
   */
  private def canPyramidMove(grid: Grid[Char], robotPos: Coor, direction: Int): Boolean =
    val aboveRobot = Coor(robotPos.row + direction, robotPos.col)
    var result = false
    if grid.get(aboveRobot) == '[' then {

      var case1 = false
      val aboveAbove1 = Coor(aboveRobot.row + direction, aboveRobot.col)
      if grid.get(aboveAbove1) == '.' then
        case1 = true
      else
        case1 = canPyramidMove(grid, aboveAbove1, direction)

      var case2 = false
      val aboveAbove2 = Coor(aboveRobot.row + direction, aboveRobot.col + 1)
      if grid.get(aboveAbove2) == '.' then
        case2 = true
      else
        case2 = canPyramidMove(grid, aboveAbove2, direction)

      result = case1 && case2
    }
    else if grid.get(aboveRobot) == ']' then {
      var case1 = false
      val aboveAbove1 = Coor(aboveRobot.row + direction, aboveRobot.col)
      if grid.get(aboveAbove1) == '.' then
        case1 = true
      else
        case1 = canPyramidMove(grid, aboveAbove1, direction)

      var case2 = false
      val aboveAbove2 = Coor(aboveRobot.row + direction, aboveRobot.col - 1)
      if grid.get(aboveAbove2) == '.' then
        case2 = true
      else
        case2 = canPyramidMove(grid, aboveAbove2, direction)
      var result = case1 && case2
    }
    else if grid.get(aboveRobot) == '.' then
      result = true
    else if grid.get(aboveRobot) == '#' then
      result = false
    else
      println(s"ERROR! Grid position contains unexpected value: ${grid.get(aboveRobot)}")
      result = false
    result
}
