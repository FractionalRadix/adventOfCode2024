package com.cormontia.adventOfCode2024

class SolverDay24 extends Solver {

  private enum Operation:
    case And, Or, Xor

  private class Rule(source1: String, operation: Operation, source2: String, target: String) {
    // Not calling it "apply" because that word is used by Scala already.
    def applyRule(mapping: Map[String, Option[Boolean]]): Option[Boolean] = {
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

    val mapping = scala.collection.mutable.Map[String, Option[Boolean]]()
    var rules = List[Rule]() // first op second -> third

    for line <- lines do
      if line.contains(":") then
        // Initial gate
        val name = line.takeWhile(ch => ch != ':')
        val value = line.dropWhile(ch => ch != ':').drop(1).trim.toInt
        mapping(name) = Some(value == 1)
      else if line.contains("->") then
        // rule
        rules = parseRule(line, mapping) :: rules

    for (k,v) <- mapping do
      println(s"Gate $k has value $v")
    for rule <- rules do
      println(rule)

    // All 100 possible gates whose name starts with "z"
    val allPossibleZGates = for digit0 <- 0 to 9; digit1 <- 0 to 9 yield s"z$digit0$digit1"
    val zGates = allPossibleZGates.filter(name => mapping.keySet.contains(name))
    println(zGates)


    //TODO!+
    ""
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

  override def solvePart2(lines: List[String]): String = {
    //TODO!+
    ""
  }
}
