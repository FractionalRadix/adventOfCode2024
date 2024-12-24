package com.cormontia.adventOfCode2024

import scala.util.boundary
import scala.util.boundary.break

class SolverDay24 extends Solver {

  override def solvePart2(lines: List[String]): String = {
    val mapping = parseInitialAssignment(lines)
    val rules = parseRules(lines, mapping)

    // Note that we don't have to CORRECT the device!
    // We only need to find 8 gates that are involved in ERRONEOUS calculations!

    // Let's find the erroneous calculations, then find the gates that they all have in common.

    // First let's see what happens if we add numbers that each only contain 1 bit, in different places.
    val xGates = mapping.keySet.filter(str => str(0) == 'x')
    val yGates = mapping.keySet.filter(str => str(0) == 'y')
    val zGates = mapping.keySet.filter(str => str(0) == 'z')

    val allWires = mapping.keySet.toSet

    // Alternative approach:
    //  1. Find a wrong calculation.
    //  2. Find the bits you should change to make it correct.
    //  3. ??? We don't know what should have been in the previous layer....
    //    --> But we COULD try and find out how many layers there ARE.
    // Number of combinations to try each round in sample 1: 362880.
    // Number of combinations to try each round in sample 2: 10520811100800.
    // Number of combinations to try each round in actual input: 8237886784735607936.
    // That won't work...


    // Alternative approach:
    //  1. Find a wrong calculation
    //  2. For every FOUR pairs of gates, swap the output values.
    //      (So that's N * (N-1) .. * (N-7) combinations....
    //  3. See which changes result1 in the correct calculation.
    //      (Won't this take forever on the actual input?)

    // Approach:
    //  - An input of all zeroes can never make a 1: 0 AND 0 = 0, 0 OR 0 = 0, 0 XOR 0 = 0.
    //  - Therefore, let's add a number to zero. Every 1 in the input must come, directly or indirectly, from the non-zero input.
    //  - Find an "addition" that is wrong in only 1 bit, preferably a bit set to 1 that shouldn't be.
    //  - Find the chain of inputs to this output bit.


    var allInvolvedWires = allWires
    val zSize = findRegisterSize(mapping.toMap, 'z')
    println(s"Z-register has $zSize bits.")
    for xBitPos <- 0 until zSize do //TODO?~ "to zSize" or "until zSize" ?
      val x = 1L << xBitPos
      val assignment = setXY(x, 0L, allWires)
      val expectedResult = x
      val (output, involvedWires1) = applyRules2(rules, assignment)
      allInvolvedWires = allInvolvedWires intersect involvedWires1
      // Now let's do a bit-wise comparison of the output and the expected result:
      val diff = x ^ output
      // Let's look at the cases where exactly ONE bit was wrong.
      if countBits(diff) == 1 then
        // Then find the gates that were involved in obtaining that result.
        // And see which ones we should change to get a different result.
        // (Note that the gate we find may not YET be "THE" culprit - it might itself have received wrong inputs!)

        val zWireName = "z" + String.format("%02d", xBitPos)
        val xWireName = "x" + String.format("%02d", xBitPos)

        if mapping.contains(xWireName) then {

          val inputs = rules.filter(rule => rule.target == zWireName)
          println(s"Inputs to $zWireName: ${inputs.mkString(",")}")
          println(s"$xWireName is ${mapping(xWireName)}")
          val inputRules = findRuleTree(zWireName, rules)
          println(inputRules) //TODO!~ Print the rules in layered form.
          // Now, up to four pairs should have their outputs switched.
          // But it's likely that in this case only 1 pair is involved.
              //printRulesLayered(inputRules.toList)
          // Now, for every pair of rules, see if the output is corrected when we swap their outputs.
          // These are candidate pairs for the solution. (We may still have both false negatives AND false positives!)
          val nrOfPairs = inputRules.size * (inputRules.size - 1)
          println(s"Checking $nrOfPairs pairs...")
          val outputs = inputRules.map( rule => rule.target )
          for rule1 <- inputRules; rule2 <- inputRules if rule1 != rule2 do
            print(".")
            var alternativeRules = rules.filter(rule => rule != rule1 && rule != rule2)
            // THIS MAY RESULT IN AN ENDLESS LOOP! (Which WOULD very effectively rule out that particular pair...).
            //TODO!+ Detect endless loops (or cycles in general?) and remove them.
            val swappedRule1 = Rule(rule1.source1, rule1.operation, rule1.source2, rule2.target)
            val swappedRule2 = Rule(rule2.source1, rule2.operation, rule2.source2, rule1.target)
            alternativeRules = swappedRule1 :: swappedRule2 :: alternativeRules
            val result = applyRules(alternativeRules, mapping.toMap)
            if result == expectedResult then
              println()
              println(s"Possible pair: $rule1, $rule2")




          println(s"Faulty result for bit at position $xBitPos")
          //println(s"X-register input: 0x${hexWithTrailingZeroes(x, zSize >> 2)}")
          //println(s"Y-register input: 0x${hexWithTrailingZeroes(0, zSize >> 2)}")
          println(s"Expected output : 0x${hexWithTrailingZeroes(x, zSize >> 2)}")
          println(s"Actual output   : 0x${hexWithTrailingZeroes(output, zSize >> 2)}")
          println(s"Bits that differ: 0x${hexWithTrailingZeroes(diff, zSize >> 2)}")



          //TODO!+ Find the culprit!


          //println(result1.toBinaryString)
          //println(s"Nr of wires involved: ${involvedWires1.size} (out of ${allWires.size}).")
        }

    println(s"Nr of involved wires: ${allInvolvedWires.size}")

    //TODO!+
    ""
  }

  private def printRulesLayered(rules: List[Rule]): Unit = {
    var indent = 3
    var currentLayer = rules.filter(rule => rule.source1(0) == 'x' || rule.source1(0) == 'y' || rule.source2(0) == 'x' || rule.source2(0) == 'y')
    var going = true
    while going do
      printLayer(currentLayer, indent)
      val currentLayerOutputs = currentLayer.map( rule => rule.target )
      currentLayer = rules.filter( rule => currentLayerOutputs.contains(rule.source1) || currentLayerOutputs.contains(rule.source2) )
      indent = indent + 3
      going = currentLayer.nonEmpty

    def printLayer(layer: List[Rule], indent: Int): Unit = {
      for rule <- layer do
        print(" " * indent)
        println(rule)
    }

    //TODO!+
  }

  /**
   * Find all rules that EVENTUALLY (directly or indirectly) go to the specified wire.
   * @param wireName A wire that appears in the set of rules.
   * @param rules The combination rules that transform two wire inputs to a third.
   * @return All rules involved in determining the value of the wire.
   */
  private def findRuleTree(wireName: String, rules: List[Rule]): Set[Rule] = {
    val ruleSet = scala.collection.mutable.Set[Rule]()


    val firstRules = rules.filter( rule => rule.target == wireName )
    ruleSet.addAll(firstRules)
    var growing = true
    while growing do
      val inputs1 = ruleSet.map( rule => rule.source1 )
      val inputs2 = ruleSet.map( rule => rule.source2 )
      val allInputs = inputs1 union inputs2 // ALL inputs for the current set of rules.
      val nextRules = rules.filter( rule => allInputs.contains(rule.target) )
      val sizeBefore = ruleSet.size
      ruleSet.addAll(nextRules)
      val sizeAfter = ruleSet.size
      growing = sizeAfter > sizeBefore

    ruleSet.toSet
  }

  /**
   * Given a power of 2, determine which bit it is. (In other words, the base 2 logarithm).
   * E.g. bitNumber(4) = 2, bitNumber(32)=5, bitNumber(1)=0
   * This method is only defined for values that are a proper power of 2.
   * This method is not defined for 0.
   * @param l A number that is a positive power of 2.
   * @return The base-2 logarithm of l.
   */
  private def bitNumber(l: Long): Int = {
    if l == 0 then
      throw RuntimeException("There are no 1-bits in 0.")

    var pos = -1
    var shifted = l
    while shifted > 0 do
      shifted = shifted >> 1
      pos += 1
    pos
  }

  private def hexWithTrailingZeroes(l: Long, n: Int): String = {
    val hexName = l.toHexString
    val nrOfZeroes = n - hexName.length
    val zeroes = "0" * nrOfZeroes
    zeroes + hexName
  }

  /**
   * Determine the number of bits in the given register ('x', 'y', or 'z').
   * We do this by counting the number of wires that are part of that register.
   * For example, if there are 40 different wires whose names start with 'z', then the z-register is 40 bits.
   * @param assignment A mapping from wire names to (optional) boolean values.
   * @param register Name of the register: 'x', 'y', or 'z'.
   * @return The number of bits that the chosen register is able to hold.
   */
  private def findRegisterSize(assignment: Map[String, Option[Boolean]], register: Char): Int = {
    assignment.keySet.count( str => str(0) == register )
  }

  /**
   * Given a Long value, count the number of "1" bits in it.
   * For example, 9 has binary representation 1001 - hence this function would return 2.
   * @param l A (positive) Long value.
   * @return The number of bits set to "1" in the binary representation of the input.
   */
  private def countBits(l: Long): Int = {
    var sum = 0
    for currentBit <- 0 until 64 do
      val mask = 1L << currentBit
      if (l & mask) != 0L then
        sum = sum + 1
    sum
  }

  //TODO!~ This won't work... it seems all gates are involved in all calculations...
  private def determineInvolvedGates1(rules: List[Rule], allWires: Set[String]): Unit = {
    var allInvolvedGates = allWires
    boundary {
      for x <- 0L to 50L; y <- 0L to 50L do
        val initialMapping = setXY(x, y, allWires)
        val expectedSum = x + y
        val calculation = applyRules2(rules, initialMapping)
        val actualSum = calculation._1
        if actualSum != expectedSum then
          allInvolvedGates = allInvolvedGates.intersect(calculation._2)
        val size = allInvolvedGates.size
        print(s"..$size")
        if allInvolvedGates.size <= 8 then
          break()
    }
    println()
    println(s"All involved gates: $allInvolvedGates")
  }

  private def verifyRegisterValue(mapping: Map[String, Option[Boolean]], registerName: Char): String = {
    val wiresForRegister = mapping.keySet.filter( name => name(0) == registerName ).toList.sorted
    var result = ""
    for wire <- wiresForRegister do
      val ch = if mapping(wire).isEmpty then
        'X'
      else
        if mapping(wire).head then
          '1'
        else
          '0'
      result = ch + result
    result
  }

  /**
   * Get the value in any register ('x', 'y', or 'z').
   * This method assumes that ALL wires in the given register have been set!
   * @param mapping An assignment from wires to three-valued booleans: Some(false), Some(true), None.
   * @param registerName Name of the register ('x', 'y', or 'z').
   * @return The value held by the given register.
   */
  private def getRegisterValue(mapping: Map[String, Option[Boolean]], registerName: Char): Long = {
    val wiresForRegister = mapping.keySet.filter( name => name(0) == registerName ).toList.sorted
    var result = 0L
    for wire <- wiresForRegister do
      result = result << 1
      if mapping(wire).head then
        result = result + 1
    result
  }


  private def applyRules(rules: List[Rule], originalMapping: Map[String, Option[Boolean]]): Long = {
    val mapping = scala.collection.mutable.Map[String, Option[Boolean]]()
    mapping.addAll(originalMapping)

    var allZGatesDefined = mapping.filter((k,v) => k(0) == 'z').forall((k,v) => v.isDefined)

    while !allZGatesDefined do
      for rule <- rules do
        val result = rule.applyRule(mapping)
        if mapping(rule.target).isEmpty then
          mapping(rule.target) = result
        allZGatesDefined = mapping.filter((k,v) => k(0) == 'z').forall((k,v) => v.isDefined)

    val zGates = mapping.keySet.filter( name => name(0) == 'z')
    var result = 0L
    for zGate <- zGates.toList.sorted.reverse do
      val bit = mapping.get(zGate).head.head
      result = 2L * result + (if bit then 1 else 0)

    result
  }

  /**
   * Apply the set of rules to a given mapping, keep track of which gates were involved.
   * @param rules The list of rules (gates) to apply to the given mapping.
   * @param originalMapping The values on the wires at the start of the calculation.
   * @return The value of the z-register, and the set of all gates that were involved in determining it.
   */
  private def applyRules2(rules: List[Rule], originalMapping: Map[String, Option[Boolean]]): (Long, Set[String]) = {
    val involvedWires = scala.collection.mutable.Set[String]()

    val mapping = scala.collection.mutable.Map[String, Option[Boolean]]()
    mapping.addAll(originalMapping)

    var allZGatesDefined = mapping.filter((k,v) => k(0) == 'z').forall((k,v) => v.isDefined)

    while !allZGatesDefined do
      for rule <- rules do
        val result = rule.applyRule(mapping)
        if mapping(rule.target).isEmpty then
          mapping(rule.target) = result
          involvedWires.add(rule.target)
        allZGatesDefined = mapping.filter((k,v) => k(0) == 'z').forall((k,v) => v.isDefined)

    val zGates = mapping.keySet.filter( name => name(0) == 'z')
    var result = 0L
    for zGate <- zGates.toList.sorted.reverse do
      val bit = mapping.get(zGate).head.head
      result = 2L * result + (if bit then 1 else 0)

    (result, involvedWires.toSet)
  }

  /**
   * A mapping where bit N of x is set to TRUE, M of y is set to TRUE,
   * every other value of x and y is set to FALSE, and everything else is set to None.
   */
  private def setXYBits(n: Int, m: Int, wires: Set[String]): scala.collection.mutable.Map[String, Option[Boolean]] = {
    val mapping = scala.collection.mutable.Map[String, Option[Boolean]]()
    for wire <- wires do
      mapping(wire) = None
    for tens <- 0 to 9; ones <- 0 to 9 do
      val value = 10 * tens + ones
      val xName = s"x$tens$ones"
      val yName = s"y$tens$ones"

      if wires.contains(xName) then
        mapping(xName) = Some(n == value)
      if wires.contains(yName) then
        mapping(yName) = Some(m == value)

    mapping
  }

  /**
   * Set the x and y registers to the given values.
   * @param x Value to be placed on the X register.
   * @param y Value to be placed on the Y register.
   * @param wires The set of all wires in the device.
   * @return An assignment where the X and Y registers contain the specified values,
   *         and every other wire is `None` (uninitialized).
   */
  private def setXY(x: Long, y: Long, wires: Set[String]): Map[String, Option[Boolean]] = {
    val mapping = scala.collection.mutable.Map[String, Option[Boolean]]()

    for wire <- wires do
      mapping(wire) = None

    for bitIndex <- 0 until 64 do
      val name = "%02d".format(bitIndex)
      val xName = s"x$name"
      val yName = s"y$name"
      val mask: Long = 1 << bitIndex

      if wires.contains(xName) then
        mapping(xName) = Some((x & mask) > 0)
      if wires.contains(yName) then
        mapping(yName) = Some((y & mask) > 0)

    mapping.toMap
  }



  private enum Operation:
    case AND, OR, XOR

  private case class Rule(source1: String, operation: Operation, source2: String, target: String) {
    // Not calling it "apply" because that word is used by Scala already.
    def applyRule(mapping: scala.collection.mutable.Map[String, Option[Boolean]]): Option[Boolean] = {
      var result: Option[Boolean] = None
      val inputValue1 = mapping(source1)
      if inputValue1.isDefined then
        val inputValue2 = mapping(source2)
        if inputValue2.isDefined then
          result = operation match
            case Operation.AND => Some(inputValue1.head & inputValue2.head)
            case Operation.OR => Some(inputValue1.head | inputValue2.head)
            case Operation.XOR => Some(inputValue1.head ^ inputValue2.head)
      result
    }

    override def toString: String = {
      s"$source1 ${operation.toString} $source2 -> $target"
    }
  }

  override def solvePart1(lines: List[String]): String = {
    val mapping = parseInitialAssignment(lines)
    val rules = parseRules(lines, mapping)
    val result = applyRules(rules, mapping.toMap)
    result.toString
  }

  private def parseInitialAssignment(lines: List[String]): scala.collection.mutable.Map[String, Option[Boolean]] = {
    val mapping = scala.collection.mutable.Map[String, Option[Boolean]]()

    for line <- lines do
      if line.contains(":") then
        // Initial wire
        val name = line.takeWhile(ch => ch != ':')
        val value = line.dropWhile(ch => ch != ':').drop(1).trim.toInt
        mapping(name) = Some(value == 1)

    mapping
  }

  private def parseRules(lines: List[String], mapping: scala.collection.mutable.Map[String, Option[Boolean]]): List[Rule] = {
    var rules = List[Rule]()

    for line <- lines do
      if line.contains("->") then
        rules = parseRule(line, mapping) :: rules

    rules
  }

  private def parseRule(line: String, mapping: scala.collection.mutable.Map[String, Option[Boolean]]): Rule = {
    val inputAndOutput = line.split("->")
    val targetGate = inputAndOutput(1).trim
    if !mapping.contains(targetGate) then
      mapping(targetGate) = None
    val sourceGates = inputAndOutput(0).split(" ")
    val sourceGate1 = sourceGates(0)
    val sourceGate2 = sourceGates(2)
    if !mapping.contains(sourceGate1) then
      mapping(sourceGate1) = None
    if !mapping.contains(sourceGate2) then
      mapping(sourceGate2) = None
    val op = sourceGates(1).trim match
      case "AND" => Operation.AND
      case "OR" => Operation.OR
      case "XOR" => Operation.XOR
    Rule(sourceGate1, op, sourceGate2, targetGate)
  }

}
