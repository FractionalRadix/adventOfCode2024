package com.cormontia.adventOfCode2024

class SolverDay17 extends Solver {
  override def solvePart1(lines: List[String]): String =

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

    str

  override def solvePart2(lines: List[String]): String =

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

    result.toString

  private def SPECIFIC_solvePart2(instructions: Array[Int], regA: Long, regB: Long, regC: Long): Long = {
    // The requested output is 16 elements long. There is only one "OUT" instruction, so this should be called 16 times.
    // The values in the registers always decrease, unless they are taken (via XOR) from another register.
    // This means the value in any register is capped by the highest input value.

    // Our output ends in 5,5,3,0.
    // Let's find the combinations of 6 octal digits that result in a list ending with (5,5,3,0).
    val resultsIn5530 = findListsThatResultIn5530(instructions, regB, regC)
    println(s"${resultsIn5530.length} combinations.")
    // To be on the safe side, strip the most significant octal digits.
    // Then iterate over 4 more octal digits: 4 "fresh" ones followed by the elements from this List.
    var truncatedResultsIn5530 = for list <- resultsIn5530 yield list.takeRight(4)
    truncatedResultsIn5530 = truncatedResultsIn5530.distinct

    val resultsIn425530 = findInputsThatResultIn(instructions, truncatedResultsIn5530, regB, regC, List(4, 2, 5, 5, 3, 0))
    // Again, to be on the safe side, we strip the most significant octal digits.
    val truncatedResultsIn425530 = resultsIn425530.map(l => l.takeRight(6)).distinct

    println(s"Found ${truncatedResultsIn425530.size} candidates for 6 digits.")

    val resultsIn034325530 = findInputsThatResultIn(instructions, truncatedResultsIn425530, regB, regC, List(0, 3, 4, 2, 5, 5, 3, 0))
    val truncatedResultsIn03425530 = resultsIn034325530.map(l => l.takeRight(8)).distinct

    println(s"Found ${truncatedResultsIn03425530.size} candidates for 8 digits")

    val resultsIn1603425530 = findInputsThatResultIn(instructions, truncatedResultsIn03425530, regB, regC, List(1, 6, 0, 3, 4, 2, 5, 5, 3, 0))
    val truncatedResultsIn1603425530 = resultsIn1603425530.map(l => l.takeRight(10)).distinct

    println(s"Found ${truncatedResultsIn1603425530.size} candidates for 10 digits")

    val resultsWith12Digits = findInputsThatResultIn(instructions, truncatedResultsIn1603425530, regB, regC, List(7, 5, 1, 6, 0, 3, 4, 2, 5, 5, 3, 0))
    val truncatedResultsWith12Digits = resultsWith12Digits.map(l => l.takeRight(12)).distinct

    println(s"Found ${truncatedResultsWith12Digits.size} candidates for 12 digits")

    val resultsWith14Digits = findInputsThatResultIn(instructions, truncatedResultsWith12Digits, regB, regC, List(1, 5, 7, 5, 1, 6, 0, 3, 4, 2, 5, 5, 3, 0))
    val truncatedResultsWith14Digits = resultsWith14Digits.map(l => l.takeRight(14)).distinct

    println(s"Found ${truncatedResultsWith14Digits.size} candidates for 14 digits.")

    val solutions = findInputsThatResultIn(instructions, truncatedResultsWith14Digits, regB, regC, List(2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 2, 5, 5, 3, 0))
    println(s"Found ${solutions.size} solutions..")
    for solution <- solutions do
      println("Solution: $solution")

    // 2 .. 4 .. 1 .. 5 .. 7 .. 5 .. 1 .. 6 .. 0 .. 3 .. 4 .. 2 .. 5 .. 5 .. 3 .. 0

    println(s"Expected output: ${instructions.mkString(",")}")

    0 //TODO!~
  }

  /**
   * Given a list of octal Digits, return the value it represents.
   * For example, List(2L, 3L) should result in 19 (2*(8 exp 1)+3*(8 exp 0))
   * @param l A list of octal digits; in other words the values should range from 0 to 7 inclusive.
   * @return The value represented by the list.
   */
  private def octalListToLong(l: List[Long]): Long = {
    if l.isEmpty then
      0L
    else
      var acc = 0L
      for octalDigit <- l do
        acc = 8 * acc + octalDigit
      acc
  }


  /**
   * Given a value, return a list that contains the octal digits that build up this value.
   * For example, 83 in octal is 123, so this method would return [1,2,3].
   * @param l The value to convert
   * @return A list of octal digits representing the given value.
   */
  private def longToOctalList(l: Long): List[Long] = {
    if l == 0 then
      List(0L)
    else
      var result: List[Long] = Nil
      var l1 = l
      while l1 > 0 do
        val last = l1 & 7
        result = last :: result
        l1 = l1 >> 3
      result
  }

  /**
   * Given the instructions and a set of possible values for A, try out the prepending this list with the
   * octal values (0000)-(7777).
   * From these numbers, select those for which the program ends in the given list.
   * @param instructions The instructions to the Chrono-spatial Computer.
   * @param listRegA The list of values we try for register A, represented as lists of octal digits.
   * @param regB The value for register B.
   * @param regC The value for register C.
   * @param desiredEnding The desired ending of the output.
   * @return The list of inputs that result in a sequence that ends with the "desired ending", represented as lists
   *         of octal digits.
   */
  private def findInputsThatResultIn(instructions: Array[Int], listRegA: List[List[Long]], regB: Long, regC: Long, desiredEnding: List[Long]): List[List[Long]] = {
    var result: List[List[Long]] = Nil

    for i0 <- 0L to 7L do
      for i1 <- 0L to 7L do
        for i2 <- 0L to 7L do
          println(s"(i0,i1,i2)==($i0,$i1,$i2)")
          for i3 <- 0L to 7L do
            for i4 <- 0L to 7L do
              for i5 <- 0L to 7L do
                for i6 <- 0L to 7L do
                for tailRegA <- listRegA do
                  val aAsList: List[Long] = List(i0, i1, i2, i3, i4, i5, i6) ++ tailRegA
                  val regA = octalListToLong(aAsList)
                  val output = executeProgram2(instructions, regA, regB, regC)
                  if output.takeRight(desiredEnding.length) == desiredEnding then
                    result = aAsList :: result
    result
  }

  private def findListsThatResultIn5530(instructions: Array[Int], regB: Long, regC: Long): List[List[Long]] =
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
