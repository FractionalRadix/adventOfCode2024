package com.cormontia.adventOfCode2024

import scala.collection.{immutable, mutable}
import scala.util.boundary
import scala.util.boundary.break

class SolverDay21 extends Solver {

  private val numericPad = Map[Char,Coor](
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

  private val directionalPad = Map[Char, Coor](
    '^' -> Coor(0, 1),
    'A' -> Coor(0, 2),
    '<' -> Coor(1, 0),
    'v' -> Coor(1, 1),
    '>' -> Coor(1, 2)
  )

  private class PossiblePaths(private val pathSections: List[List[String]]) {
    // A possible path is defined by taking one string from the first list,
    // followed by taking one from the second list,
    // followed by taking one form the third list... and so on.
    // Let's call these things sections: first list is the first section, second list is the second section,
    // and so on.
    def nrOfSections: Int = pathSections.size
    def getSection(i: Int): List[String] = pathSections(i)
    def nrOfPossiblePaths: Long = pathSections.map( list => list.length ).product
    def print(title: String): Unit = {
      val sectionsAsString = pathSections.map( l => showSection(l) )
      println(s"$title: ${sectionsAsString.mkString(" * ")}")
    }
    def lengthOfShortestPath(): Long = {
      var minLength = 0L
      for section <- pathSections do
        val minLen = section.map( l => l.length ).min
        minLength += minLen
      minLength
    }
    private def showSection(l: List[String]): String = l.map( s => "\"" + s + "\"").mkString("[", ",", "]")
  }

  private class KeyPad(buttons: Map[Char, Coor], val forbidden: Coor) {
    private val paths = mutable.Map[(Coor, Coor), List[String]]()

    private def getPosition(button: Char): Coor = buttons(button)

    /**
     * Determine all (non-cyclic) paths between from a starting position to an ending position.
     * The paths should be as short as possible; no cycles or detours.
     * The paths must avoid the "forbidden" position.
     * @param startPos The position that the robot arm is currently pointed at.
     * @param endPos The position that we want the robot arm to arrive at.
     * @return The list of all paths that take you from the starting position to the ending position.
     */
    private def getPathsBetween(startPos: Coor, endPos: Coor): List[String] = {
      // This method uses memoization.
      if paths.contains((startPos, endPos)) then
        paths((startPos, endPos))
      else
        val verticalDifference = endPos.row - startPos.row
        val verticalMoves = if (endPos.row - startPos.row) > 0 then "v" * verticalDifference else "^" * -verticalDifference
        val horizontalDifference = endPos.col - startPos.col
        val horizontalMoves = if horizontalDifference > 0 then ">" * horizontalDifference else "<" * -horizontalDifference
        val moveSet = verticalMoves + horizontalMoves
        val allPaths = Util.permutations(moveSet)
        val allSafePaths = allPaths
          .filter( path => avoidsPosition(startPos, path, forbidden) )
          .map( path => path ++ "A" )
        paths((startPos, endPos)) = allSafePaths
        allSafePaths
    }

    /**
     * Determine all (non-cyclic) paths between from a starting button to an ending button
     * The paths should be as short as possible; no cycles or detours.
     * The paths must avoid the "forbidden" position.
     * @param startButton The button that the robot arm is currently pointed at.
     * @param endButton The button that we want the robot arm to arrive at.
     * @return The list of all paths that take you from the starting button to the ending button.
     */
    private def getPathsBetween(startButton: Char, endButton: Char): List[String] = {
      getPathsBetween(getPosition(startButton), getPosition(endButton))
    }

    private def avoidsPosition(startPos: Coor, sequence: String, forbidden: Coor): Boolean = {
      if startPos == forbidden then
        return false
      if sequence == "" then
        return true
      var curPos = startPos
      var answer = true
      boundary {
        for ch <- sequence do
          val nextPos = determineNextPosition(curPos, ch)
          if nextPos == forbidden then
            answer = false
            break()
          curPos = nextPos
      }
      answer
    }

    def determineNextPosition(curPos: Coor, ch: Char): Coor = {
      val nextPos = ch match
        case '^' => Coor(curPos.row - 1, curPos.col)
        case 'v' => Coor(curPos.row + 1, curPos.col)
        case '>' => Coor(curPos.row, curPos.col + 1)
        case '<' => Coor(curPos.row, curPos.col - 1)
        case 'A' => curPos
      nextPos
    }

    private val sequences = mutable.Map[String, Map[Int, List[String]]]()

    def getPathsForSequence(line: String): Map[Int, List[String]] = {
      if sequences.contains(line) then
        sequences(line)
      else
        val pathComponents = mutable.Map[Int, List[String]]()
        var curChar = 'A' // The initial position of the robot arm is on the 'A' position.
        var i = 0
        for nextChar <- line do
          val paths = getPathsBetween(curChar, nextChar)
          pathComponents(i) = paths
          curChar = nextChar
          i = i + 1
        sequences(line) = pathComponents.toMap
        pathComponents.toMap
    }
  }

  override def solvePart1(lines: List[String]): String = {

    val numericKeyPad = KeyPad(numericPad, Coor(3,0))
    val directionalKeyPad = KeyPad(directionalPad, Coor(0,0))

    var totalComplexity = 0L

    for line <- lines do
      val pathComponents1 = numericKeyPad.getPathsForSequence(line)

      // At this point, the number of possible sequences is still tractable.
      // So let's generate all possible sequences for this line.
      // We call it "instructions2" because it's the set of instructions for the second robot.
      // (Technically "line" would be "instructions1").
      val sortedKeys = pathComponents1.keys.toList.sorted
      var instructions2 = List("")
      for key <- sortedKeys do
        instructions2 = Util.crossProduct(instructions2, pathComponents1(key))

      // Similarly, the number of sequences for the third robot is still tractable.
      var shortest: Option[Long] = None
      for instr <- instructions2 do
        val instructions3 = instructionsForThirdRobot(directionalKeyPad, List(instr))
        for instr <- instructions3 do
          val instructions4 = instructionsForThirdRobot(directionalKeyPad, List(instr))
          val minLength = instructions4.map( l => l.length ).min
          if shortest.isEmpty then
            shortest = Some(minLength)
          else if minLength < shortest.head then
            shortest = Some(minLength)

      val numericPart = line.dropRight(1).toLong
      val length = shortest.head
      val complexity = numericPart * length

      totalComplexity += complexity

    totalComplexity.toString
  }

  private def instructionsForThirdRobot(directionalKeyPad: KeyPad, instructions2: List[String]): List[String] = {
    // With all possible sequences for the second robot, let's find all possible sequences for the third robot.
    var instructions3 = List[String]()
    for instr <- instructions2 do
      val pathComponents2 = directionalKeyPad.getPathsForSequence(instr)
      val sortedKeys = pathComponents2.keys.toList.sorted
      var instructions_tmp = List("")
      for key <- sortedKeys do
        instructions_tmp = Util.crossProduct(instructions_tmp, pathComponents2(key))
      instructions3 = instructions3 ++ instructions_tmp
    val lengths = instructions3.map(l => l.length)
    val shortestLength = lengths.min
    // Filter out the sequences that are longer than the shortest sequence.
    instructions3.filter(l => l.length == shortestLength)
  }

  /**
   * Given a list of list of String, yield the list of list of the lengths of these strings.
   * For example, [["Hello", "world"],["Cat", "Dog,"Fish"]] will yield [[5,5], [3,3,4]]
   * @param l The list of lists of strings.
   * @return The list of lists of the lengths of the strings.
   */
  private def findLengths(l: List[List[String]]): List[List[Int]] = {
    for l1 <- l yield
      l1.map( l2 => l2.length )
  }

  private def performInstructions(instructions: String, startPos: Coor, keyPad: KeyPad): Unit = {
    var curPos = startPos
    for instruction <- instructions do
      curPos = keyPad.determineNextPosition(curPos, instruction)
      if curPos == keyPad.forbidden then
        println("Kaboom!")
  }

  override def solvePart2(lines: List[String]): String = {
    "" //TODO!~
  }


}
