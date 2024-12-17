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

    val useNaive = false
    val result: Long = if useNaive then
      NAIVE_solvePart2(instructions, regA, regB, regC)
    else
      SPECIFIC_solvePart2(instructions, regA, regB, regC)

    result

  private def SPECIFIC_solvePart2(instructions: Array[Int], regA: Long, regB: Long, regC: Long): Long =
    // The requested output is 16 elements long. There is only one "OUT" instruction, so this should be called 16 times.
    // The values in the registers always decrease, unless they are taken (via XOR) from another register.
    // This means the value in any register is capped by the highest input value.

    // Our output ends in 5,5,3,0.
    // Let's find the combinations of 6 octal digits that result in a list ending with (5,5,3,0).
    val resultsIn5530 = findListsThatResultIn5530(instructions, regB, regC)
    println(s"${resultsIn5530.length} combinations.")
    // To be on the safe side, strip the most significant octal digits.
    // Then iterate over 4 more octal digits: 4 "fresh" ones followed by the elements from this List.
    val truncatedResultsIn5530 = for list <- resultsIn5530 yield list.takeRight(4)
    //TODO!+ De-duplicate the truncated list!
    for i0 <- 0L to 7L do
      for i1 <- 0L to 7L do
        for i2 <- 0L to 7L do
          for i3 <- 0L to 7L do
            var octals7to4: Long = 8 * 8 * 8 * i0 +  8 * 8 * i1 +  8 * i2 + i3
            octals7to4 = octals7to4 * (8*8*8*8)
            for octals3to0List <- truncatedResultsIn5530 do
              val octals3to0 = 8*8*8*octals3to0List(0) + 8*8*octals3to0List(1) + 8*octals3to0List(2) + octals3to0List(3)
              val octals7to0 = octals7to4 + octals3to0
              val output = executeProgram2(instructions, octals7to0, regB, regC)
              if output.takeRight(6) == List(4,2,5,5,3,0) then
                println(s"FOUND ONE! $i0 $i1 $i2 $i3 ${octals3to0List(0)} ${octals3to0List(1)} ${octals3to0List(2)} ${octals3to0List(3)} $output")



    println(s"Expected output: ${instructions.mkString(",")}")

    0 //TODO!~

  private def findListsThatResultIn5530(instructions: Array[Int], regB: Long, regC: Long ): List[List[Long]] =
    // Find the octals that will result in a computation ending on (5,5,3,0).
    var result: List[List[Long]] = Nil
    for i0 <- 0L to 7L do
      for i1 <- 0L to 7L do
        for i2 <- 0L to 7L do
          for i3 <- 0L to 7L do
            for i4 <- 0L to 7L do
              for i5 <- 0L to 7L do
                val a = 8 * 8 * 8 * 8 * 8 * i0 + 8 * 8 * 8 * 8 * i1 + 8 * 8 * 8 * i2 + 8 * 8 * i3 + 8 * i4 + i5
                val output = executeProgram2(instructions, a, regB, regC)
                if output.takeRight(4) == List[Long](5, 5, 3, 0) then
                  //println(s"FOUND! $i0 $i1 $i2 $i3 $i4 $i5 $output")
                  result = List(i0,i1,i2,i3,i4,i5) :: result
    result

  private def NAIVE_solvePart2(instructions: Array[Int], regA: Long, regB: Long, regC: Long): Long =
    println(s"Registers: $regA $regB $regC")
    println(s"Instructions: [${instructions.mkString(" .. ")}]")
    printProgram(instructions)

    //var init_regA: Long = 22537800000L // Earlier things have already been tried
    //var init_regA: Long = 34740700000L // Earlier things have already been tried
    var init_regA: Long = 72935900000L // Earlier things have already been tried

    //var init_regA: Long = 67523687193987L // Not the solution...
    //var init_regA: Long = 0

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
    while ip < instructions.length do
      //println(s"ip=$ip A=$regA B=$regB C=$regC")
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
          //TODO?+
          //if operand != instructions(output.length) then
          //  ip = ip + instructions.length // Force break

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
