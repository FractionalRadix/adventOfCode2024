package com.cormontia.adventOfCode2024

class SolverDay21 extends Solver {
  override def solvePart1(lines: List[String]): String = {
    var totalComplexity = 0
    for line <- lines do
      print(line + " ")
      val firstRobotInstructions = generateFirstSequence(line)
      //println(firstRobotInstructions)
      val secondRobotInstructions = generateDirectionalKeypadSequence(firstRobotInstructions)
      //println(secondRobotInstructions)
      val finalInstructions = generateDirectionalKeypadSequence(secondRobotInstructions)
      //println(finalInstructions)

      println(s"Performing first robot instructions for $line:")
      performInstructions(firstRobotInstructions, Coor(3,2), Coor(3,0))
      println(s"Performing second robot instructions for $line:")
      performInstructions(secondRobotInstructions, Coor(0,2), Coor(0,0))
      println(s"Performing final instructions for $line:")
      performInstructions(finalInstructions, Coor(0,2), Coor(0,0))

      val length = finalInstructions.length
      val numericPart = line.dropRight(1).toInt
      val complexity = length * numericPart
      println(s"...Complexity=$complexity ($length * $numericPart)")

      totalComplexity += complexity
    totalComplexity.toString
  }

  private def performInstructions(instructions: String, startPos: Coor, forbiddenPos: Coor): Unit = {
    var curPos = startPos
    for instruction <- instructions do
      curPos = instruction match
        case '^' => Coor(curPos.row - 1, curPos.col)
        case 'v' => Coor(curPos.row + 1, curPos.col)
        case '>' => Coor(curPos.row, curPos.col + 1)
        case '<' => Coor(curPos.row, curPos.col - 1)
        case 'A' => curPos
      //print(s"$curPos ")
      if curPos == forbiddenPos then
        println("Kaboom!")
  }


  private def generateDirectionalKeypadSequence(input: String) = {
    val pad = Map[Char, Coor](
      '^' -> Coor(0,1),
      'A' -> Coor(0,2),
      '<' -> Coor(1,0),
      'v' -> Coor(1,1),
      '>' -> Coor(1,2)
    )
    var curPos = Coor(0,2)
    var result = ""
    for ch <- input do
      val nextPos = pad(ch)
      // For now, let's ignore the "forbidden" area because we only look at distance travelled.
      //val horizontalMoves = moveHorizontally(curPos, nextPos)
      //val verticalMoves = moveVertically(curPos, nextPos)
      //result = result + horizontalMoves + verticalMoves + "A"
      result = result + safePath(curPos, nextPos) + "A"
      curPos = nextPos
    result
  }

  /**
   * Safe paths on the directional keypad
   */
  private def safePath(startPos: Coor, endPos: Coor): String = {
    //val positions = List(Coor(0,1), Coor(0,2), Coor(1,0), Coor(1,1), Coor(1,2))

    // If you're staying in the same row, you should never reach the forbidden area.
    // Either it's not on your row, or it's outside your movement range.
    val path = if startPos.row == endPos.row then
      val str1 = moveHorizontally(startPos, endPos)
      val str2 = moveVertically(startPos, endPos)
      str1 + str2
    else
      // We need to move vertically and perhaps horizontally.
      // If we are under the forbidden area, we should move horizontally FIRST.
      // But if we are NOT under the forbidden area, we should move vertically FIRST.
      if startPos.col == 0 then
        val str1 = moveHorizontally(startPos, endPos)
        val str2 = moveVertically(startPos, endPos)
        str1 + str2
      else
        val str1 = moveVertically(startPos, endPos)
        val str2 = moveHorizontally(startPos, endPos)
        str1 + str2

    path
  }

  private def generateFirstSequence(input: String): String = {

    val pad = Map[Char,Coor](
      '7' -> Coor(0,0),
      '8' -> Coor(0,1),
      '9' -> Coor(0,2),
      '4' -> Coor(1,0),
      '5' -> Coor(1,1),
      '6' -> Coor(1,2),
      '1' -> Coor(2,0),
      '2' -> Coor(2,1),
      '3' -> Coor(2,2),
      '0' -> Coor(3,1),
      'A' -> Coor(3,2)
    )

    var curPos = Coor(3,2)
    var result = ""
    for ch <- input do
      val nextPos = pad(ch)

      // For now, let's ignore the "forbidden" area because we only look at distance travelled.
      val ignoreForbidden = false
      result = if ignoreForbidden then {
        val horizontalMoves = moveHorizontally(curPos, nextPos)
        val verticalMoves = moveVertically(curPos, nextPos)
        result + horizontalMoves + verticalMoves + "A"
      } else {
        // Avoid position (3,0).
        // If you are in the "danger row", move vertically first.
        // If you are in the "danger column", move horizontally first.
        // (Note that these conditions are exclusive).
        // If you are in neither, move as you please.
        val horizontalMoves = moveHorizontally(curPos, nextPos)
        val verticalMoves = moveVertically(curPos, nextPos)
        if curPos.row == 3 then
          result + verticalMoves + horizontalMoves + "A"
        else if curPos.col == 0 then
          result + horizontalMoves + verticalMoves + "A"
        else
          result + horizontalMoves + verticalMoves + "A"
      }

      curPos = nextPos
    result
  }

  private def moveVertically(curPos: Coor, nextPos: Coor) = {
    if nextPos.row > curPos.row then "v" * (nextPos.row - curPos.row) else "^" * (curPos.row - nextPos.row)
  }

  private def moveHorizontally(curPos: Coor, nextPos: Coor) = {
    if nextPos.col > curPos.col then ">" * (nextPos.col - curPos.col) else "<" * (curPos.col - nextPos.col)
  }

  override def solvePart2(lines: List[String]): String = {
    "" //TODO!~
  }
}
