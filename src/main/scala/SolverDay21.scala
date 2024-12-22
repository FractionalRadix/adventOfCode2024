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

  private val numericPaths = generateNumericPadSequences()

  private val directionalPaths = generateDirectionalPadSequences()

  class PossiblePaths(private val pathSections: List[List[String]]) {
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
    def allPaths(): List[String] = {
      //TODO!~ Find a way to do this INCREMENTALLY. (And then start memoizing, too).
      explodeListOfLists(pathSections, (str1, str2) => str1 + str2, "")
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

  /**
   * The memoized instructions that you should type on a directional pad,
   * to direct a robot to type on a further directional pad.
   */
  private val directionalPadToDirectionalPad = mutable.Map[(Coor, Coor), List[String]]()

  /**
   * Given the path that a robot should follow, give the possible ways to generate them.
   * @param firstRobotPath A path to follow, such as ">>A"
   * @return The sequences that will result in ">>A", assuming you start at (0,2).
   */
  private def determineDirectionalPadToDirectionalPad(firstRobotPath: String): PossiblePaths = {
    var secondRobotSequences = List[List[String]]()
    var curPos = Coor(0, 2)
    for ch <- firstRobotPath do
      val nextPos = directionalPad(ch)
      val sequences = directionalPaths(curPos, nextPos)
      directionalPadToDirectionalPad((curPos, nextPos)) = sequences
      secondRobotSequences = secondRobotSequences :+ sequences
      curPos = nextPos
    PossiblePaths(secondRobotSequences)
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

    val numericKeyPad = KeyPad(numericPad, Coor(3,0))
    val directionalKeyPad = KeyPad(directionalPad, Coor(0,0))

    for line <- lines do
      println(s"Paths for $line:")
      val pathComponents1 = numericKeyPad.getPathsForSequence(line)
      //println(pathComponents1)

      // At this point, the number of possible sequences is still tractable.
      // So let's generate all possible sequences for this line.
      // We call it "instructions2" because it's the set of instructions for the second robot.
      // (Technically "line" would be "instructions1").
      val sortedKeys = pathComponents1.keys.toList.sorted
      var instructions2 = List("")
      for key <- sortedKeys do
        instructions2 = crossProduct(instructions2, pathComponents1(key))
      println(s"Cross product is $instructions2")

      // Similarly, the number of sequences for the third robot is still tractable.
      for instr <- instructions2 do
        val instructions3 = instructionsForThirdRobot(directionalKeyPad, List(instr))
        println(instructions3.map(l => l.length).min)

      val testcase      = instructionsForThirdRobot(directionalKeyPad, List("v<<A>>^A<A>AvA<^AA>A<vAAA>^A"))

      //println(testcase)
      val testCaseLengths = testcase.map(l => l.length)
      println(s"Testcase lengths: $testCaseLengths")
      //val instructions3Lengths = instructions3.map(l => l.length)
      //println(s"instructions3 lengths: $instructions3Lengths")

      /*

      // Now to find what we should type on the "manual" keypad.
      for sequence <- instructions3 do
        val options = directionalKeyPad.getPathsForSequence(sequence)
        println(s"Current version has ${options.size} components.")
        val componentValues = options.values.toList
        val componentLengths = findLengths(componentValues)
        val shortestLengths = componentLengths.map( l => l.min )
        println(shortestLengths)


       */

    ""
  }

  private def instructionsForThirdRobot(directionalKeyPad: KeyPad, instructions2: List[String]): List[String] = {
    // With all possible sequences for the second robot, let's find all possible sequences for the third robot.
    var instructions3 = List[String]()
    for instr <- instructions2 do
      val pathComponents2 = directionalKeyPad.getPathsForSequence(instr)
      val sortedKeys = pathComponents2.keys.toList.sorted
      var instructions_tmp = List("")
      for key <- sortedKeys do
        instructions_tmp = crossProduct(instructions_tmp, pathComponents2(key))
      instructions3 = instructions3 ++ instructions_tmp
    //println(s"Possible instruction sequences for third robot: $instructions3")
    println(s"There are ${instructions3.size} possible sequences for the third robot.")
    val lengths = instructions3.map(l => l.length)
    val shortestLength = lengths.min
    println(s"  The shortest of these consists of ${lengths.min} characters.")
    println(s"  The longest of these consists of ${lengths.max} characters.")
    // Filter out the sequences that are longer than the shortest sequence.
    instructions3 = instructions3.filter(l => l.length == shortestLength)
    println(s"  Filtering out the sequences over $shortestLength characters leaves ${instructions3.length} sequences.")
    instructions3
  }

  /**
   * Given two lists of Strings, determine all combinations of them.
   * For example, ["hello ","bye "] and ["world", "cat", "dog"] will yield:
   * ["hello world", "bye world", "hello cat", "bye cat", "hello dog", "bye dog"]
   * Note that if either list is empty, the result will also be empty.
   * To get the identity operation, do a cross product with List("").
 *
   * @param l1 A list of Strings.
   * @param l2 A list of Strings.
   * @return A list containing all combinations of the elements of the input lists.
   */
  private def crossProduct(l1: List[String], l2: List[String]): List[String] = {
    for e1 <- l1; e2 <- l2 yield e1 + e2
  }

  /**
   * Given a list of list of String, yield the list of list of the lenghts of these strings.
   * For example, [["Hello", "world"],["Cat", "Dog,"Fish"]] will yield [[5,5], [3,3,4]]
   * @param l The list of lists of strings.
   * @return The list of lists of the lengths of the strings.
   */
  private def findLengths(l: List[List[String]]): List[List[Int]] = {
    for l1 <- l yield
      l1.map( l2 => l2.length )
  }


  private def allPossiblePaths(pathComponents: List[List[String]]): List[String] = {
    if pathComponents.isEmpty then
      List()
    else
      val head = pathComponents.head
      val tail = allPossiblePaths( pathComponents.tail )
      val new1 = head.map( str => str :: tail )

      val result = for headList <- head yield
        headList.map( str => str :: tail )
      val l = result.toList

    Nil // TODO!~
  }


  def OLD_solvePart1(lines: List[String]): String = {
    var totalComplexity = 0

    for line <- lines do
      println(line)

      // First robot instructions:
      val firstRobotPathsSections = allPathsThatResultIn(Coor(3,2), line, numericPad, numericPaths)
      var firstRobotPaths = explodeListOfLists[String](firstRobotPathsSections, (s1, s2) => s1 + s2, "")
      val shortestLength = firstRobotPaths.map( l => l.length ).min
      firstRobotPaths = firstRobotPaths.filter( l => l.length == shortestLength )

      // With the first robot paths and the directional sequences determined,
      // we can incrementally generate a new mapping from "directional keypad button" to "directional keypad button".
      // In this mapping we only keep the shortest distances.
      for firstRobotPath <- firstRobotPaths do
        println(s"First robot path: $firstRobotPath")

        val secondRobotSequences = determineDirectionalPadToDirectionalPad(firstRobotPath)
        //secondRobotSequences.print("Second robot sequences:")

        // Note that we always start at "A" (0,2). Because every sequence ends with "A", so that's where the next one takes off.

        var len = 0L
        for i <- 0 until secondRobotSequences.nrOfSections do
          val currentSection = secondRobotSequences.getSection(i) // This is a List[String].
          var minLengthsForSection: List[Long] = Nil

          //println
          //println(s"Current section: $i -  ${currentSection.mkString(",")}")
          for currentOptionOfCurrentSection <- currentSection do

            // "currentOptionOFFirstSection" is a String. A sequence of instructions that will end in 'A'.
            val waysToDoCurrentOptionOfCurrentSection = determineDirectionalPadToDirectionalPad(currentOptionOfCurrentSection)
            val minLen = waysToDoCurrentOptionOfCurrentSection.lengthOfShortestPath()
            minLengthsForSection = minLen :: minLengthsForSection

          len += minLengthsForSection.min
        println(s"Shortest possible length: $len")
        val numericPart = line.dropRight(1).toInt
        val complexity = len * numericPart
        println(s"...Complexity=$complexity ($len * $numericPart)")


      val firstRobotInstructions = generateFirstSequence(line)
      //println(firstRobotInstructions)
      val secondRobotInstructions = generateDirectionalKeypadSequence(firstRobotInstructions)
      //println(secondRobotInstructions)
      val finalInstructions = generateDirectionalKeypadSequence(secondRobotInstructions)
      //println(finalInstructions)

      val numericKeyPad = KeyPad(numericPad, Coor(3, 0))
      val directionalKeyPad = KeyPad(directionalPad, Coor(0, 0))
      println(s"Performing first robot instructions for $line:")
      performInstructions(firstRobotInstructions, Coor(3,2), numericKeyPad)
      println(s"Performing second robot instructions for $line:")
      performInstructions(secondRobotInstructions, Coor(0, 2), directionalKeyPad)
      println(s"Performing final instructions for $line:")
      performInstructions(finalInstructions, Coor(0,2), directionalKeyPad)

      val length = finalInstructions.length
      val numericPart = line.dropRight(1).toInt
      val complexity = length * numericPart
      println(s"...Complexity=$complexity ($length * $numericPart)")

      generateNumericPadSequences()

      totalComplexity += complexity


    totalComplexity.toString
  }

  /**
   * Given a set of instructions and a starting position, determine all paths that could lead to executing these instructions.
   * However, do not give them as a list. Instead, give them as a list of sections.
   * @param startPos Starting position for the sequence.
   * @param instructions The instructions to be generated.
   * @param keypad A mapping from characters to coordinates. This can represent the numeric keypad or the directional keypad, as desired.
   * @param paths A mapping of all the valid paths between two coordinates.
   * @return All the paths that result in the given set of instructions (implicit in the data structure returned).
   */
  private def allPathsThatResultIn(
     startPos: Coor,
     instructions: String,
     keypad: Map[Char, Coor],
     paths: Map[(Coor, Coor), List[String]]
  ): List[List[String]] = {
    var curPos = startPos
    var firstRobotPaths = List[List[String]]()
    for ch <- instructions do
      val nextPos = keypad(ch)
      val validPaths = paths((curPos, nextPos)).distinct
      firstRobotPaths = firstRobotPaths :+ validPaths
      curPos = nextPos
    firstRobotPaths
  }

  /**
   * Given a list of lists of elements, find all combinations that preserve the order.
   * For example, [["a","b"], ["c","d"], ["e","f"]] yields ("ace","acf", "ade", "adf", "bce","bcf", "bde", "bdf")
   * @param l The list of lists.
   * @param f The function to combine two elements. For example, if we have a list of list of String, this would be
   *          the String concatenation operation.
   * @param nil The identity element for the element. For example, if we have a list of list of String, this would be
   *            the empty String.
   * @return All permutations of l that maintain the original order.
   */
  private def explodeListOfLists[T](l: List[List[T]], f: (T,T) => T, nil: T): List[T] = {
    if l.isEmpty then
      List(nil)
    else
      var result = List[T]()
      // Assume that l is: [["a", "b"],["c","d"],["e","f"]].
      val headList = l.head  // Then headList is ["a"].
      val tailList = explodeListOfLists(l.tail,f, nil) // Our tailList is ["ce", "cf", "de", "df"].
      for headElt <- headList do
        val newList = tailList.map( str => f(headElt,str) )
        // newList is ["ace", "acf", "ade", "adf"] in the first iteration,
        // and ["bce", "bcf", "bde", "bdf"] the second iteration.
        result = result ++ newList
      result
  }

  private def performInstructions(instructions: String, startPos: Coor, keyPad: KeyPad): Unit = {
    var curPos = startPos
    for instruction <- instructions do
      curPos = keyPad.determineNextPosition(curPos, instruction)
      if curPos == keyPad.forbidden then
        println("Kaboom!")
  }

  //TODO?-
  private def generateDirectionalKeypadSequence(input: String) = {
    var curPos = Coor(0,2)
    var result = ""
    for ch <- input do
      val nextPos = directionalPad(ch)
      result = result + safePath(curPos, nextPos) + "A"
      curPos = nextPos
    result
  }



  /**
   * Generate all the shortest sequences from one position to another on the directional keypad.
   * But remove those sequences that visit the forbidden position (0,0).
   * @return All the safe shortest sequences from one position to another on the directional keypad.
   */
  private def generateDirectionalPadSequences(): Map[(Coor, Coor), List[String]] = {
    val positions = List(Coor(0,1), Coor(0,2), Coor(1,0), Coor(1,1), Coor(1,2))
    val map = collection.mutable.Map[(Coor,Coor), List[String]]()
    for startPos <- positions do
      for endPos <- positions do
        val availableMoves = moveHorizontally(startPos, endPos) + moveVertically(startPos, endPos)
        // Generate all variants of this sequence, but filter the ones that hit (3,0).
        val sequences = Util.permutations(availableMoves)
        val validSequences = sequences
          .filter(sequence => avoidsPosition(startPos, sequence, Coor(3, 0)))
          .map(str => str + "A")
        map((startPos, endPos)) = validSequences
    map.toMap
  }

  /**
   * Generate all the shortest sequences to go from one position to another on the numeric keypad.
   * But remove those sequences that visit the forbidden position (3,0).
   * @return All the safe shortest sequences from one position to another on the numeric keypad.
   */
  private def generateNumericPadSequences(): Map[(Coor,Coor), List[String]] = {
    // First, all positions on the numeric keypad.
    val positionSeq = for row <- 0 to 3; col <- 0 to 2 if !(row==3 && col == 0) yield Coor(row,col)
    val positions = positionSeq.toList
    // Then, for every two positions on the pad, determine all shortest paths (that avoid the forbidden position).
    val map = collection.mutable.Map[(Coor,Coor), List[String]]()
    for startPos <- positions do
      for endPos <- positions do
          val availableMoves = moveHorizontally(startPos, endPos) + moveVertically(startPos, endPos)
          // Generate all variants of this sequence, but filter the ones that hit (3,0).
          val sequences = Util.permutations(availableMoves)
          val validSequences = sequences
            .filter( sequence => avoidsPosition(startPos, sequence, Coor(3,0)) )
            .map(str => str + "A")
          map((startPos, endPos)) = validSequences
    map.toMap
  }

  //TODO?~ Replace with the implementation in class KeyPad?
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

  private def generateVariants(curPos: Coor, moves: String, forbiddenPos: Coor): List[String] = {
    if curPos == forbiddenPos then
      return Nil

    if moves.isEmpty then
      return List("")

    var result = List[String]()
    for i <- moves.indices do
      //print(s" $ch ")
      val ch = moves(i)
      val nextPos: Coor = determineNextPosition(curPos, ch)
      val nextSequences1 = generateVariants(nextPos, moves.take(i) + moves.drop(i+1), forbiddenPos)
      val nextSequences2 = nextSequences1.map( str => ch.toString + str)
      result = result ++ nextSequences2
    result
  }

  private def determineNextPosition(curPos: Coor, ch: Char) = {
    val nextPos = ch match
      case '^' => Coor(curPos.row - 1, curPos.col)
      case 'v' => Coor(curPos.row + 1, curPos.col)
      case '>' => Coor(curPos.row, curPos.col + 1)
      case '<' => Coor(curPos.row, curPos.col - 1)
      case 'A' => curPos
    nextPos
  }

  private def generateFirstSequence(input: String): String = {

    var curPos = Coor(3,2)
    var result = ""
    for ch <- input do
      val nextPos = numericPad(ch)

      // For now, let's ignore the "forbidden" area because we only look at distance travelled.
      val ignoreForbidden = false
      val horizontalMoves = moveHorizontally(curPos, nextPos)
      val verticalMoves = moveVertically(curPos, nextPos)
      result = if ignoreForbidden then {
        result + horizontalMoves + verticalMoves + "A"
      } else {
        // Avoid position (3,0).
        // If you are in the "danger row", move vertically first.
        // If you are in the "danger column", move horizontally first.
        // (Note that these conditions are exclusive).
        // If you are in neither, move as you please.
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

  //TODO!-
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


  override def solvePart2(lines: List[String]): String = {
    "" //TODO!~
  }


}
