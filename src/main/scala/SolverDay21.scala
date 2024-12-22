package com.cormontia.adventOfCode2024

import scala.collection.{immutable, mutable}

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
    def getPathsBetween(startPos: Coor, endPos: Coor): List[String] = {
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
        val allSafePaths = allPaths.filter( path => avoidsPosition(startPos, path, forbidden))
        paths((startPos, endPos)) = allSafePaths
        allSafePaths
    }

    private def avoidsPosition(startPos: Coor, sequence: String, forbidden: Coor): Boolean = {
      if startPos == forbidden then
        return false
      if sequence == "" then
        return true
      for ch <- sequence do
        val nextPos = determineNextPosition(startPos, ch)
        if nextPos == forbidden then
          return false
      true
    }
  }

  override def solvePart1(lines: List[String]): String = {
    //TODO!+

    // New approach: brute force, but with pruning.
    // Type any character on the first pad.
    // See what it does to the second, third, and fourth pads.
    // WHENEVER
    //  ANY of these pads revisits the same button before it hit 'A', STOP.
    //  You hit the 'A' button for the right

    var firstPadPos = Coor(3,2) // Pad inside the depressurized room, gives access to the area with the missing Historian.
    var secondPadPos = Coor(0,2) // Pad inside the highly radiated room, gives access to the depressurized room.
    val thirdPadPos = Coor(0,2) // Pad inside the -40 degrees room, gives access to the radiated room.
    val fourthPadPos = Coor(0,2) // Pad that you use, in the room full of Historians. Gives access to the -40 degrees room.

    // Let's try to type "0".






    ""
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
        val newList = tailList.map( str => f(headElt,str) ) // ["ace", "acf", "ade", "adf"] or ["bce", "bcf", "bde", "bdf"]
        result = result ++ newList
      result
  }

  private def performInstructions(instructions: String, startPos: Coor, forbiddenPos: Coor): Unit = {
    var curPos = startPos
    for instruction <- instructions do
      curPos = determineNextPosition(curPos, instruction)
      if curPos == forbiddenPos then
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

  private def avoidsPosition(startPos: Coor, sequence: String, forbidden: Coor): Boolean = {
    if startPos == forbidden then
      return false
    if sequence == "" then
      return true
    for ch <- sequence do
      val nextPos = determineNextPosition(startPos, ch)
      if nextPos == forbidden then
        return false
    true
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
