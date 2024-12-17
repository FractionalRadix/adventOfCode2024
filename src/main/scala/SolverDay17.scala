package com.cormontia.adventOfCode2024

class SolverDay17 extends Solver {
  override def solvePart1(lines: List[String]): Long =

    val strippedLines = for line <- lines yield
      line.dropWhile( ch => ch != ':').drop(2)
    val regA = strippedLines(0).toLong
    val regB = strippedLines(1).toLong
    val regC = strippedLines(2).toLong
    val instructions = strippedLines(4).split(",").map( str => str.toInt )
    println(s"Registers: $regA $regB $regC")
    println(s"Instructions: [${instructions.mkString(" .. ")}]")


    val output: List[Long] = executeProgram(instructions, regA, regB, regC)

    println(s"OUTPUT: ${output.mkString(",")}")
    println(s"A: $regA, B: $regB, C: $regC")
    val str = output.mkString
    if str.isEmpty then
      println("No output.")
    else
      println(str)

    if str.isEmpty then 0 else str.toLong

  override def solvePart2(lines: List[String]): Long =
    val strippedLines = for line <- lines yield
      line.dropWhile( ch => ch != ':').drop(2)
    var regA = strippedLines(0).toLong
    val regB = strippedLines(1).toLong
    val regC = strippedLines(2).toLong
    val instructions = strippedLines(4).split(",").map( str => str.toInt )
    println(s"Registers: $regA $regB $regC")
    println(s"Instructions: [${instructions.mkString(" .. ")}]")

    var init_regA: Long = 0
    var found = false
    while !found do
      if init_regA % 100000 == 0 then
        print(s"$init_regA ")
      regA = init_regA
      val output = executeProgram2(instructions, regA, regB, regC)
      if output == instructions.toList then
        println("FOUND!")
        found = true
      else
        init_regA = init_regA + 1
    init_regA

  //TODO!~ Add optimizations.
  private def executeProgram2(instructions: Array[Int], param_regA: Long, param_regB: Long, param_regC: Long) = {
    var regA = param_regA
    var regB = param_regB
    var regC = param_regC
    var ip = 0
    var output: List[Long] = Nil
    while ip < instructions.length do
      instructions(ip) match
        case 0 => // ADV
          //println(s"ADV")
          val operand = combo(instructions(ip + 1))
          val numerator = regA
          val denominator = math.pow(2, operand)
          regA = truncate(numerator / denominator)
          ip = ip + 2
        case 1 => // BXL
          //println(s"BXL")
          val operand = instructions(ip + 1)
          regB = regB ^ operand
          ip = ip + 2
        case 2 => // BST
          //println(s"BST")
          val operand = combo(instructions(ip + 1))
          regB = operand % 8
          ip = ip + 2
        case 3 => // JNZ
          //println(s"JNZ")
          if regA != 0 then
            ip = instructions(ip + 1)
          else
            ip = ip + 2
        case 4 => // BXC
          //println(s"BXC")
          regB = regB ^ regC
          ip = ip + 2
        case 5 => // OUT
          //println(s"OUT")

          val operand = combo(instructions(ip + 1)) % 8
          if operand != instructions(output.length) then
            ip = ip + instructions.length // Force break

          output = output :+ operand
          ip = ip + 2
        case 6 => // BDV
          //println(s"BDV")
          val operand = combo(instructions(ip + 1))
          val numerator = regA
          val denominator = math.pow(2, operand)
          regB = truncate(numerator / denominator)
          ip = ip + 2
        case 7 => // CDV
          //println(s"CDV")
          val operand = combo(instructions(ip + 1))
          val numerator = regA
          val denominator = math.pow(2, operand)
          regC = truncate(numerator / denominator)
          ip = ip + 2
        case _ => println(s"ILLEGAL INSTRUCTION! ip=$ip")

      def combo(operand: Int): Long =
        operand match
          case 0 => 0
          case 1 => 1
          case 2 => 2
          case 3 => 3
          case 4 => regA
          case 5 => regB
          case 6 => regC
          case _ => println(s"ILLEGAL COMBO OPERAND! $operand $ip")
            0
    output
  }

  private def executeProgram(instructions: Array[Int], param_regA: Long, param_regB: Long, param_regC: Long) = {
    var regA = param_regA
    var regB = param_regB
    var regC = param_regC
    var ip = 0
    var output: List[Long] = Nil
    while ip < instructions.length do
      instructions(ip) match
        case 0 => // ADV
          //println(s"ADV")
          val operand = combo(instructions(ip + 1))
          val numerator = regA
          val denominator = math.pow(2, operand)
          regA = truncate(numerator / denominator)
          ip = ip + 2
        case 1 => // BXL
          //println(s"BXL")
          val operand = instructions(ip + 1)
          regB = regB ^ operand
          ip = ip + 2
        case 2 => // BST
          //println(s"BST")
          val operand = combo(instructions(ip + 1))
          regB = operand % 8
          ip = ip + 2
        case 3 => // JNZ
          //println(s"JNZ")
          if regA != 0 then
            ip = instructions(ip + 1)
          else
            ip = ip + 2
        case 4 => // BXC
          //println(s"BXC")
          regB = regB ^ regC
          ip = ip + 2
        case 5 => // OUT
          //println(s"OUT")
          val operand = combo(instructions(ip + 1)) % 8
          output = output :+ operand
          ip = ip + 2
        case 6 => // BDV
          //println(s"BDV")
          val operand = combo(instructions(ip + 1))
          val numerator = regA
          val denominator = math.pow(2, operand)
          regB = truncate(numerator / denominator)
          ip = ip + 2
        case 7 => // CDV
          //println(s"CDV")
          val operand = combo(instructions(ip + 1))
          val numerator = regA
          val denominator = math.pow(2, operand)
          regC = truncate(numerator / denominator)
          ip = ip + 2
        case _ => println(s"ILLEGAL INSTRUCTION! ip=$ip")

      def combo(operand: Int): Long =
        operand match
          case 0 => 0
          case 1 => 1
          case 2 => 2
          case 3 => 3
          case 4 => regA
          case 5 => regB
          case 6 => regC
          case _ => println(s"ILLEGAL COMBO OPERAND! $operand $ip")
            0
    output
  }


  private def truncate(x: Double): Long =
    x.toLong
}
