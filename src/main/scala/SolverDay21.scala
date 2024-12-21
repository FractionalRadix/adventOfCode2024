package com.cormontia.adventOfCode2024

import scala.collection.immutable

class SolverDay21 extends Solver {
  override def solvePart1(lines: List[String]): String = {
    var totalComplexity = 0

    val numericPaths = generateNumericPadSequences()
    val directionalPaths = generateDirectionalPadSequences()

    for line <- lines do
      println(line)

      val powder: List[List[String]] = List(List("a", "b"), List("c", "d"), List("e", "f"))
      val boom = explodeListOfLists[String](powder, (str1,str2) => str1 + str2 , "")
      println(boom)

      // First robot instructions:
      var curPos = Coor(3,2)
      var firstRobotPaths = List[List[String]]()
      for ch <- line do
        val nextPos = numericPad(ch)
        val validPaths = numericPaths((curPos, nextPos)).distinct
        firstRobotPaths = firstRobotPaths :+ validPaths
        curPos = nextPos
      println(firstRobotPaths)
      val firstRobotPathsBoom = explodeListOfLists[String](firstRobotPaths, (str1,str2) => str1 + str2, "")
      println(firstRobotPathsBoom)

/*
      // Second robot instructions:
      curPos = Coor(0, 2)
      for line <- firstRobotPaths do
        ;

       */


      /*
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

       */
    totalComplexity.toString
  }

  /**
   * Given a list of lists of elements, find all combinations that preserve the order.
   * For example, [["a","b"],["c","d"],["e","f"]] yields ["ace","acf", "ade", "adf", "bce","bcf", "bde", "bdf"].
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
        if startPos != endPos then {
          val availableMoves = moveHorizontally(startPos, endPos) + moveVertically(startPos, endPos)
          // Generate all variants of this sequence, but filter the ones that hit (3,0).
          val sequences = permutations(availableMoves)
          val validSequences = sequences.filter(sequence => avoidsPosition(startPos, sequence, Coor(3, 0)))
          map((startPos, endPos)) = validSequences
          //println(s"Mapping: from $startPos to $endPos: $sequences")
        }
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
        if startPos != endPos then {
          val availableMoves = moveHorizontally(startPos, endPos) + moveVertically(startPos, endPos)
          // Generate all variants of this sequence, but filter the ones that hit (3,0).
          val sequences = permutations(availableMoves)
          val validSequences = sequences.filter( sequence => avoidsPosition(startPos, sequence, Coor(3,0)) )
          map((startPos, endPos)) = validSequences
          //println(s"Mapping: from $startPos to $endPos: $sequences")
        }
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

  private def permutations(str: String): List[String] = {
    if str.isEmpty then
      List("")
    else
      var result = List[String]()
      // For every character in the string, yield: that character plus all permutations of the rest.
      for i <- str.indices do
        val remainingCharacters = str.take(i) + str.drop(i+1)
        val list0 = permutations(remainingCharacters)
        val list1 = list0.map( x => str(i).toString + x )
        result = result ++ list1
      result
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
