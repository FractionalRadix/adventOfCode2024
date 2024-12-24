package com.cormontia.adventOfCode2024

class SolverDay24 extends Solver {

  override def solvePart2(lines: List[String]): String = {
    val mapping = parseInitialAssignment(lines)
    val rules = parseRules(lines, mapping)

    // Note that we don't have to CORRECT the device!
    // We only need to find 8 gates that are involved in ERRONEOUS calculations!

    // Let's find the erroneous calculations, then find the gates that they all have in common.

    // First let's see what happens if we add numbers that each only contain 1 bit, in different places.
    val xGates = mapping.keySet.filter( str => str(0) == 'x' )
    val yGates = mapping.keySet.filter( str => str(0) == 'y' )
    val zGates = mapping.keySet.filter( str => str(0) == 'z' )

    for xBitPos <- 0 until xGates.size; yBitPos <- 0 until yGates.size if xBitPos != yBitPos do
        val xBit: Long = 1 << xBitPos
        val yBit: Long = 1 << yBitPos
        //println(s"$xBit, $yBit")
        val map = setXY(xBitPos, yBitPos, mapping.keySet.toSet)


    //TODO!+
    ""
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

    //// All 100 possible gates whose name starts with "z"
    //val allPossibleZGates = for digit0 <- 0 to 9; digit1 <- 0 to 9 yield s"z$digit0$digit1"
    //val zGates = allPossibleZGates.filter(name => mapping.keySet.contains(name))
    val zGates = mapping.keySet.filter( name => name(0) == 'z')
    var result = 0L
    for zGate <- zGates.toList.sorted.reverse do
      val bit = mapping.get(zGate).head.head
      result = 2L * result + (if bit then 1 else 0)

    result
  }

  /** A mapping where bit N of x is set to TRUE, M of y is set to TRUE,
   * every other value of x and y is set to FALSE, and everything else is set to None.
   */
  private def setXY(n: Int, m: Int, wires: Set[String]): scala.collection.mutable.Map[String, Option[Boolean]] = {
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

  private enum Operation:
    case And, Or, Xor

  private case class Rule(source1: String, operation: Operation, source2: String, target: String) {
    // Not calling it "apply" because that word is used by Scala already.
    def applyRule(mapping: scala.collection.mutable.Map[String, Option[Boolean]]): Option[Boolean] = {
      var result: Option[Boolean] = None
      val inputValue1 = mapping(source1)
      if inputValue1.isDefined then
        val inputValue2 = mapping(source2)
        if inputValue2.isDefined then
          result = operation match
            case Operation.And => Some(inputValue1.head & inputValue2.head)
            case Operation.Or => Some(inputValue1.head | inputValue2.head)
            case Operation.Xor => Some(inputValue1.head ^ inputValue2.head)
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
      case "AND" => Operation.And
      case "OR" => Operation.Or
      case "XOR" => Operation.Xor
    Rule(sourceGate1, op, sourceGate2, targetGate)
  }

}
