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
    val regA = strippedLines(0).toLong
    val regB = strippedLines(1).toLong
    val regC = strippedLines(2).toLong
    val instructions = strippedLines(4).split(",").map( str => str.toInt )

    val useNaive = true
    val result: Long = if useNaive then
      NAIVE_solvePart2(instructions, regA, regB, regC)
    else
      SPECIFIC_solvePart2(instructions, regA, regB, regC)

    result

  private def SPECIFIC_solvePart2(instructions: Array[Int], regA: Long, regB: Long, regC: Long): Long =
    val desiredResult: Array[Int] = instructions
    val desiredResultXor3: Array[Int] = instructions.map( instr => instr ^ 3 )
    var multiplier: Long = 1
    var result: Long = 0
    for elt <- desiredResultXor3 do
      result = result + elt * multiplier
      multiplier = multiplier * 8

    println(s"Result is $result")

    NAIVE_solvePart2(instructions, regA, regB, regC)


  //def NAIVE_solvePart2(lines: List[String]): Long =
  private def NAIVE_solvePart2(instructions: Array[Int], regA: Long, regB: Long, regC: Long): Long =
    println(s"Registers: $regA $regB $regC")
    println(s"Instructions: [${instructions.mkString(" .. ")}]")
    printProgram(instructions)

    //var init_regA: Long = 22537800000L // Earlier things have already been tried
    //var init_regA: Long = 34740700000L // Earlier things have already been tried
    //var init_regA: Long = 72935900000L // Earlier things have already been tried
    var init_regA = 0L

    var found = false
    while !found do
      if init_regA % 100000 == 0 then
        print(s"$init_regA ")
      val output = executeProgram2(instructions, init_regA, regB, regC)
      if output == instructions.toList then
        println("FOUND!")
        found = true
      else
        init_regA = init_regA + 1
    init_regA

  private def printProgram(instructions: Array[Int]): Unit =
    def comboOperand(operand: Int): String =
      operand match
        case 0 => "0"
        case 1 => "1"
        case 2 => "2"
        case 3 => "3"
        case 4 => "regA"
        case 5 => "regB"
        case 6 => "regC"
        case _ => " ERROR!!"

    var ip = 0
    for instruction <- instructions do
      if ip % 2 == 0 then
        instruction match
          case 0 => println(s"ADV ${comboOperand(instructions(ip+1))}")
          case 1 => println(s"BXL ${instructions(ip+1)}")
          case 2 => println(s"BST ${comboOperand(instructions(ip+1))}")
          case 3 => println(s"JNZ ${instructions(ip+1)}")
          case 4 => println(s"BXC ")
          case 5 => println(s"OUT ${comboOperand(instructions(ip+1))}")
          case 6 => println(s"BDV ${comboOperand(instructions(ip+1))}")
          case 7 => println(s"CDV ${comboOperand(instructions(ip+1))}")
      ip = ip + 1

  //TODO!~ Add optimizations.
  private def executeProgram2(instructions: Array[Int], param_regA: Long, param_regB: Long, param_regC: Long) = {
    var regA = param_regA
    var regB = param_regB
    var regC = param_regC
    var ip = 0
    var output: List[Long] = Nil
    //println("--------------------------------------------------")
    while ip < instructions.length do
      //println(s"$regA $regB $regC")
      instructions(ip) match
        case 0 => // ADV
          //println(s"ADV")
          val operand = combo(instructions(ip + 1))
          regA = regA >> operand
          ip = ip + 2
        case 1 => // BXL
          val operand = instructions(ip + 1)
          regB = regB ^ operand
          ip = ip + 2
        case 2 => // BST
          val operand = combo(instructions(ip + 1))
          regB = operand % 8
          ip = ip + 2
        case 3 => // JNZ
          if regA != 0 then
            ip = instructions(ip + 1)
          else
            ip = ip + 2
        case 4 => // BXC
          regB = regB ^ regC
          ip = ip + 2
        case 5 => // OUT
          val operand = combo(instructions(ip + 1)) % 8
          if operand != instructions(output.length) then
            ip = ip + instructions.length // Force break

          output = output :+ operand
          ip = ip + 2
        case 6 => // BDV
          val operand = combo(instructions(ip + 1))
          regB = regA >> operand
          ip = ip + 2
        case 7 => // CDV
          val operand = combo(instructions(ip + 1))
          regC = regA >> operand
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
          val operand = combo(instructions(ip + 1))
          regA = regA >> operand
          ip = ip + 2
        case 1 => // BXL
          val operand = instructions(ip + 1)
          regB = regB ^ operand
          ip = ip + 2
        case 2 => // BST
          val operand = combo(instructions(ip + 1))
          regB = operand % 8
          ip = ip + 2
        case 3 => // JNZ
          if regA != 0 then
            ip = instructions(ip + 1)
          else
            ip = ip + 2
        case 4 => // BXC
          regB = regB ^ regC
          ip = ip + 2
        case 5 => // OUT
          val operand = combo(instructions(ip + 1)) % 8
          output = output :+ operand
          ip = ip + 2
        case 6 => // BDV
          val operand = combo(instructions(ip + 1))
          regB = regA >> operand
          ip = ip + 2
        case 7 => // CDV
          val operand = combo(instructions(ip + 1))
          regC = regA >> operand
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
