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

  /**
   * Going from A, "v<" results in 5 steps, "<v" results in 6.
   * Let's find all sequences that start with "<v" and see if there are equivalent sequences that start with "v<".
   * @param paths A list of paths on the directional keypad, that are supposed to start at 'A'.
   * @return A copy of the input, but with all "v<" paths removed if they had a corresponding "<v" paths.
   */
  private def removeLeftDownMoves(paths: List[String]): List[String] = {
    var result = paths
    val leftDownStrings = paths.filter(str => str.startsWith("<v"))
    for candidateForRemoval <- leftDownStrings do
      val startsWithDownLeft = "v<" + candidateForRemoval.drop(2)
      if paths.contains(startsWithDownLeft) then
        result = result.filter(str => str != candidateForRemoval)
    result
  }

  override def solvePart2(lines: List[String]): String = {
    val numericKeyPad = KeyPad(numericPad, Coor(3,0))
    val directionalKeyPad = KeyPad(directionalPad, Coor(0,0))

    var totalComplexity = 0L

    val numericPaths = mutable.Map[(Char, Char), List[String]]()
    val numericKeys = "0123456789A"
    for key1 <- numericKeys do
      for key2 <- numericKeys do
        val moves = numericKeyPad.getOptimalPathsBetween(key1, key2)
        numericPaths((key1, key2)) = moves
    println(s"Numeric paths: $numericPaths.")
    // We can optimize this further: A-Down-Left becomes 5 steps, A-Left-Down becomes 6 steps.
    // So if any sequence starts with "Left-Down" and there is a corresponding sequence that starts with "Down-Left",
    // then the sequence that stars with "Left-Down" can be removed.
    for (k,v) <- numericPaths do
      numericPaths(k) = removeLeftDownMoves(v)
    println(s"Numeric paths after optimization: $numericPaths.")
    println(s"Numeric paths that still result in more than one subsequent path: ${numericPaths.filter((k,v) => v.length > 1)}")
    // Result of that last in is still a long list, but more optimizations might be available.

    val directionalPaths = mutable.Map[(Char, Char), List[String]]()
    val dirKeys = "<>^vA"
    for key1 <- dirKeys do
      for key2 <- dirKeys do
        val moves = directionalKeyPad.getOptimalPathsBetween(key1, key2)
        directionalPaths((key1, key2)) = moves
    println(s"Directional paths: $directionalPaths.")
    for (k,v) <- directionalPaths do
      directionalPaths(k) = removeLeftDownMoves(v)
    println(s"Directional paths after optimization: $directionalPaths")
    println(s"Directional paths that still result in more than one subsequent path: ${directionalPaths.filter((k,v) => v.length > 1)}")
    //  Result of that last one:  HashMap((v,A) -> List(^>A, >^A), (>,^) -> List(^<A, <^A), (^,>) -> List(v>A, >vA))



    //val twoRobotsDown: mutable.Map[String, List[Int]] = precalculateAttempt1(directionalKeyPad, dirKeys)

    for line <- lines do
      val numericPart = line.dropRight(1).toLong
      //val length = shortestSequenceLength(line, numericKeyPad, directionalKeyPad, twoRobotsDown.toMap)
      val length = 0L //TODO!~
      val complexity = numericPart * length
      totalComplexity += complexity

    totalComplexity.toString
  }

  private def precalculateAttempt1(directionalKeyPad: KeyPad, dirKeys: String) = {
    // On optimizing paths:
    // The next robot does NOT always start on "A".
    // Let's say we have 3 robots, all on starting A.
    // The first must type ">v".
    // So the second must type "vA" followed by "<A".
    // After typing "vA", the arm of the first robot is on ">".


    // Now here's the plan: every sequence on the DIRECTIONAL keypad is a sequence of 1, 2, or 3 characters, followed by an 'A'.
    // So we pre-calculate these sequences, and keep track of their SHORTEST lengths.
    val twoRobotsDown = mutable.Map[String, List[Int]]()


    // For all sequences of 1 (followed by the usual 'A') on the directional keypad, find their shortest version 2 robots down the line.
    for ch1 <- dirKeys do
      val sequence = s"${ch1}A"
      var iter = List(sequence)
      for i <- 1 to 2 do
        iter = iterate(iter, directionalKeyPad)
      twoRobotsDown(sequence) = iter.map(l => l.length).distinct

    // For all sequences of 2 (followed by the usual 'A') on the directional keypad, find their shortest version 2 robots down the line.
    for ch1 <- dirKeys; ch2 <- dirKeys do
      val sequence = s"$ch1${ch2}A"
      var iter = List(sequence)
      for i <- 1 to 2 do
        iter = iterate(iter, directionalKeyPad)
      twoRobotsDown(sequence) = iter.map(l => l.length).distinct

    //TODO!~ Runs out of heap space for 3 robots.
    // For all sequences of 3 (followed by the usual 'A') on the directional keypad, find their shortest version 2 robots down the line.
    for ch1 <- dirKeys; ch2 <- dirKeys; ch3 <- dirKeys do
      val sequence = s"$ch1$ch2${ch3}A"
      var iter = List(sequence)
      for i <- 1 to 2 do
        iter = iterate(iter, directionalKeyPad)
      twoRobotsDown(sequence) = iter.map(l => l.length).distinct

    println(twoRobotsDown)
    // Is there any mapping in "twoRobotsDown" where the value contains more than one number?
    val multipleOptions = twoRobotsDown.filter((k, v) => v.length > 1)
    println(multipleOptions)
    twoRobotsDown
  }

  private def iterate(list: List[String], keypad: KeyPad): List[String] = {
    val res1 = for str <- list yield
      //explicate(keypad.getOptimalPathsForSequence(str))
      explicate(keypad.getPathsForSequence(str))
    res1.flatten()
  }

  /**
   * Determine the shortest sequence to type, if a chain of 25 robots has result in typing this code.
   *
   * @param accessCode The code that should be typed on the numeric keypad by the final robot.
   * @return The length of the shortest sequence to type on the first keypad, to let the final robot type the code.
   */
  private def shortestSequenceLength(
    accessCode: String,
    numericKeyPad: KeyPad,
    directionalKeyPad: KeyPad,
    twoRobotsDown: Map[String, List[Int]]
  ): Long = {
    println(s"Finding shortest sequence length for $accessCode.")
    val accessPairs = accessCode.sliding(2).toList
    for pair <- accessPairs do
      val firstPaths = numericKeyPad.getOptimalPathsBetween(pair(0), pair(1))
      //TODO!~ Find the shortest...
      // Note that you don't need to know the shortest string, only its LENGTH!
      println(s"--From ${pair(0)} to ${pair(1)}.")
      val secondPaths = firstPaths
        .flatMap( str => explicate(directionalKeyPad.getOptimalPathsForSequence(str)) )
        .distinct
      println(s"--$secondPaths")


      ;

    0L //TODO!~
  }
  // Memoization:
  //private val pathLengths = mutable.Map[(Char,Char), Long]()


  private def findSequencesWithMultipleLengths(dirKeys: String, directionalKeyPad: KeyPad): Unit = {
    // For every combination of 2 on the directional keypad, let's see how it works out in 3 robots.
    for ch1 <- dirKeys; ch2 <- dirKeys do
      val sequence = s"$ch1${ch2}A"
      var iter = List(sequence)
      for i <- 1 to 3 do
        iter = iterate(iter, directionalKeyPad)
        val lengths = iter.map(l => l.length).distinct
        if lengths.length > 1 then
          println(s"Iteration $i. $sequence $lengths")


    // For every combination of 3 on the directional keypad, let's see how it works out in 3 robots.
    for ch1 <- dirKeys; ch2 <- dirKeys; ch3 <- dirKeys do
      val sequence = s"$ch1$ch2$ch3"
      var iter = List(sequence)
      for i <- 1 to 3 do
        //println(s"Sequence of 3. Iteration $i")
        iter = iterate(iter, directionalKeyPad)
        val lengths = iter.map(l => l.length).distinct
        if lengths.length > 1 then
          println(s"Iteration $i. $sequence $lengths")

    // For every combination of 4 on the directional keypad, let's see how it works out in 3 robots.
    for ch1 <- dirKeys; ch2 <- dirKeys; ch3 <- dirKeys; ch4 <- dirKeys do
      val sequence = s"$ch1$ch2$ch3$ch4"
      var iter = List(sequence)
      for i <- 1 to 2 do
        //println(s"Sequence of 4. Iteration $i")

        iter = iterate(iter, directionalKeyPad)
        val lengths1 = iter.map(l => l.length).distinct
        if lengths1.length > 1 then
          println(s"Iteration $i. $sequence $lengths1")
  }

  private def experimentOnNumericKeypad(numericKeyPad: KeyPad, directionalKeyPad: KeyPad): Unit = {
    // Another test to see if some paths become longer than others.
    // For example, "From 2 to 4" will give you options of 25 long and of 21 long.
    // Conversely, "From 4 to 2" will give you options of 17 long and of 21 long.
    // ....one would think these paths were equivalent...
    // At the level of paths2, "2 to 4" is nine commands, "4 to 2" is only 7 ??
    for source <- List('2') /* numericKeys */ do
      for target <- List('4') /* numericKeys */ do
        val paths1 = numericKeyPad.getOptimalPathsBetween(source, target)
        println(s"From $source to $target: ${paths1.length} paths.")
        for path1 <- paths1 do
          val paths2 = explicate(directionalKeyPad.getOptimalPathsForSequence(path1))
          //val lengths = paths2.map( l => l.length )
          //print(s" ${lengths.mkString(",")}")
          for path2 <- paths2 do
            println(s"Path lvl 2: $path2")
            val paths3 = explicate(directionalKeyPad.getOptimalPathsForSequence(path2))
            //for path3 <- paths3 do
            //  println(s"...$path3")
            val lengths = paths3.map(l => l.length).distinct
            print(s" ${lengths.mkString(",")}");
        println
  }



  private def execute(instructions: String, keyPad: KeyPad, startPos: Char): String = {
    var curPos = keyPad.getPosition(startPos)
    var result = ""
    for ch <- instructions do
      if ch == 'A' then
        result = result + keyPad.getButton(curPos)
      curPos = keyPad.determineNextPosition(curPos, ch)

    result
  }

  private def findDifferentPathLengths(numericKeyPad: KeyPad, directionalKeyPad: KeyPad): Unit = {
    println()
    for sourceKey <- List('0') do
      for targetKey <- List('1') do
        println(s"From $sourceKey to $targetKey:")
        val paths = numericKeyPad.getPathsBetween(sourceKey, targetKey)
        for path <- paths do
          //println(s"..First robot: $path")
          val paths2 = explicate(directionalKeyPad.getPathsForSequence(path))
          for path2 <- paths2 do
            println(s"....Second robot: $path2")
            val paths3 = explicate(directionalKeyPad.getPathsForSequence(path2))
            println(s"${paths3.length} different paths.")
            val paths3lengths = paths3.map(l => l.length)
            val minLen = paths3lengths.min
            val maxLen = paths3lengths.max
            if minLen != maxLen then
              println(s"Same start and destination, different length: $minLen / $maxLen .")
            else
              println(s" $minLen")
        println()
  }

  /**
   * Given the components of a set of paths, determine all paths implicit in this set.
   * WARNING! This has more or less exponential complexity!
 *
   * @param implicitPaths A map from positions to path components. For example,
   *    (0 -> ["hello", "bye"]), (1->["silly","friendly"]), (2->["cat","dog"])
   *    This would turn into the list [ ["hello", "silly", "cat"], ["hello", "silly", "dog"],
   *    ["hello", "friendly", "cat"], ["hello", "friendly", "dog"],["bye", "silly", "cat"], ["bye", "silly", "dog"],
   *    ["bye", "friendly", "cat"], ["bye", "friendly", "dog"].
   * @return The list of all paths generated by this mapping.
   */
  private def explicate(implicitPaths: Map[Int, List[String]]): List[String] = {
    var allPaths = List("")
    val keys = implicitPaths.keys.toList.sorted
    for key <- keys do
      val pathComponents = implicitPaths(key)
      allPaths = Util.crossProduct(allPaths, pathComponents)
    allPaths
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
    def getButton(coor: Coor): Char = buttons.find( mapping => mapping._2 == coor ).head._1

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

    /**
     * Memoization of "optimal paths".
     */
    private val optimalPaths = mutable.Map[(Char, Char), List[String]]()

    /**
     * An "optimal" path is one that avoids needless moves for the NEXT robot.
     * For example, "^^>" is better than "^>^".
     * The first can be done with  the sequence: "<A", "A","v>A" (6 moves in total).
     * The second can be done with the sequence: "<A", "v>A", "<^A" (8 moves in total).
     * @param startButton The button that the robot arm is currently pointed at.
     * @param endButton The button that we want the robot arm to arrive at.
     * @return The ways to go from the start button to the end button that require the least steps for the next robot.
     */
    def getOptimalPathsBetween(startButton: Char, endButton: Char): List[String] = {
      if optimalPaths.contains((startButton, endButton)) then
        optimalPaths((startButton, endButton))
      else
        val paths = getPathsBetween(startButton, endButton)
        val startPos = getPosition(startButton)
        val endPos = getPosition(endButton)
        val verticalDifference = endPos.row - startPos.row
        val verticalMoves = if (endPos.row - startPos.row) > 0 then "v" * verticalDifference else "^" * -verticalDifference
        val horizontalDifference = endPos.col - startPos.col
        val horizontalMoves = if horizontalDifference > 0 then ">" * horizontalDifference else "<" * -horizontalDifference
        //TODO?+ Since the next robot starts at 'A', always start with the button closest to 'A' ?
        var result = List[String]()

        // First, avoid the moves that "zigzag". Optimize for getting the same button in a row.
        val str1 = horizontalMoves + verticalMoves + "A"
        if avoidsPosition(startPos, str1, forbidden) then
          result = str1 :: result
        val str2 = verticalMoves + horizontalMoves + "A"
        if avoidsPosition(startPos, str2, forbidden) then
          result = str2 :: result

        /*
        // Second, going from A, "v<" results in 5 steps, "<v" results in 6.
        // Let's find all sequences that start with "<v" and see if there are equivalent sequences that start with "v<".
        val startLeftDown = result.filter( str => str.startsWith("<v"))
        for candidateForRemoval <- startLeftDown do
          val startDownLeft = "v<" + candidateForRemoval.drop(2)
          if result.contains(startDownLeft) then
            result = result.filter( str => str != candidateForRemoval )
         */

        optimalPaths((startButton, endButton)) = result
        result.distinct
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

    //TODO?~ Memoize?
    def getOptimalPathsForSequence(line: String): Map[Int, List[String]] = {
      val pathComponents = mutable.Map[Int, List[String]]()
      var curChar = 'A' //TODO?~ Parameterize?
      var i = 0
      for nextChar <- line do
        val paths = getOptimalPathsBetween(curChar, nextChar)
        pathComponents(i) = paths
        curChar = nextChar
        i = i + 1
      pathComponents.toMap
    }
  }

  override def solvePart1(lines: List[String]): String = {

    //TODO!~
    val skipPart1 = false
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
