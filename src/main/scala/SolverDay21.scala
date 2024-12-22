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

  override def solvePart2(lines: List[String]): String = {
    val numericKeyPad = KeyPad(numericPad, Coor(3,0))
    val directionalKeyPad = KeyPad(directionalPad, Coor(0,0))

    var totalComplexity = 0L

    // All sequences always start at 'A'...
    // And to go from (for example) '2' to '9' should always be the same sequence.
    // So... there's just 121 ('0'-'A' x '0'-'A') sequence components?
    // BUT! Keep in mind that while the first robot goes from "number" to "number", every SUBSEQUENT
    // robot hits "A" after stuff is done!

    val numericPaths = mutable.Map[(Char, Char), List[String]]()
    val numericKeys = "0123456789A"
    for key1 <- numericKeys do
      for key2 <- numericKeys do
        val moves = numericKeyPad.getPathsBetween(key1, key2)
        println(s"From $key1 to $key2: $moves")
        numericPaths((key1, key2)) = moves
    // Now the catch. Doing this path via a second directional keypad means we must prepend every line with "A",
    // the starting position. (<--- not sure if that's correct ??)

    val directionalPaths = mutable.Map[(Char, Char), List[String]]()
    val dirKeys = "<>^vA"
    for key1 <- dirKeys do
      for key2 <- dirKeys do
        print(s"From $key1 to $key2: ")
        val moves = directionalKeyPad.getPathsBetween(key1, key2)
        println(s"From $key1 to $key2: $moves")
        directionalPaths((key1, key2)) = moves

    // Now let's look at the example expansion: 029A
    // We look at 0-2, 2-9, and 9-A separately.
    // 1. Determine the paths on the first numeric keypad.
    val p02step1 = numericKeyPad.getPathsBetween('0', '2')
    // 2. Determine the paths on the first directional keypad.
    for path <- p02step1 do
      println(path)
      // Replace "^" with the shortest sequence for "A^" ?







    // Now have a look at sequences that "zigzag".
    // For example, (0,4) can be "^^<A" or "^<^A". (Note that "<^^A" is filtered because it enters the forbidden zone).
    // "<^<A" MIGHT result in needing to press "A" more often than "<^^A" because it changes direction twice, while
    // "<^^A" only changes direction once?

/*
    // What is (the length of) the shortest path from 'A' to '0'?
    // 1. On the numeric keypad?
    val dist1 = numericKeyPad.getPathsBetween('1', '9')
    println(dist1)
    // 2. On the first directional keypad?
    for dist2part <- dist1 do
      println(dist2part)
      val correctedDist2Part = "A" + dist2part
      for i <- 0 until correctedDist2Part.length - 1 do
        val part = directionalKeyPad.getPathsBetween(correctedDist2Part(i), correctedDist2Part(i + 1))
        //val firstPart = directionalKeyPad.getPathsBetween('A',dist2part(0))
        //val secondPart = directionalKeyPad.getPathsBetween(dist2part(0), dist2part(1))
        // Happens not to be longer...
        println(s"..$part")
*/
    for line <- lines do
      val numericPart = line.dropRight(1).toLong
      val length = shortestSequenceLength(line)
      val complexity = numericPart * length
      totalComplexity += complexity

    totalComplexity.toString
  }

  /**
   * Determine the shortest sequence to type, if a chain of 25 robots has result in typing this code.
   * @param accessCode The code that should be typed on the numeric keypad by the final robot.
   * @return The length of the shortest sequence to type on the first keypad, to let the final robot type the code.
   */
  private def shortestSequenceLength(accessCode: String): Long = {



    0L //TODO!~
  }

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

    def getPosition(button: Char): Coor = buttons(button)

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
    def getPathsBetween(startButton: Char, endButton: Char): List[String] = {
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

    //TODO!~
    val skipPart1 = true
    if skipPart1 then
      "<Part 1 temporarily skipped>"
    else {

      val numericKeyPad = KeyPad(numericPad, Coor(3, 0))
      val directionalKeyPad = KeyPad(directionalPad, Coor(0, 0))

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
            val minLength = instructions4.map(l => l.length).min
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
}
