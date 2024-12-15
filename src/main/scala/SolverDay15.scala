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

  private var robotPos: Coor = Coor(0,0) // Temporary initial value
  private var grid: Grid[Char] = Grid[Char]()
  private var directions = ""

  override def solvePart1(lines: List[String]): Long =
    val (grid0, directions0) = gridAndMovements(lines)
    grid = grid0
    directions = directions0

    robotPos = grid.findCoordinatesOf('@').head
    for ch <- directions do
      println(s"Robot at $robotPos")
      ch match
        case '<' => moveRobot(coor => Coor(coor.row, coor.col - 1))
        case 'v' => moveRobot(coor => Coor(coor.row + 1, coor.col))
        case '>' => moveRobot(coor => Coor(coor.row, coor.col + 1))
        case '^' => moveRobot(coor => Coor(coor.row - 1, coor.col))
        case _ => println("OOPS, invalid instruction!")
      grid.print()
    0 //TODO!~

  private def moveRobot(move: Coor => Coor): Unit =
    val nextPos = move(robotPos)
    if grid.get(nextPos) == '.' then
      grid.set(robotPos, '.')
      grid.set(nextPos, '@')
      robotPos = nextPos
    else if grid.get(nextPos) == '#' then
      ;
    else if grid.get(nextPos) =='O' then
      val nextPos2 = move(nextPos)
      if grid.get(nextPos2) == '.' then
        grid.set(nextPos2, 'O')
        grid.set(nextPos, '@')
        grid.set(robotPos, '.')
        robotPos = nextPos

  override def solvePart2(lines: List[String]): Long =
    0 //TODO!~
}
