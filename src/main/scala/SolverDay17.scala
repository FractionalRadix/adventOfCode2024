package com.cormontia.adventOfCode2024

class SolverDay17 extends Solver {
  override def solvePart1(lines: List[String]): String =

    val strippedLines = for line <- lines yield
      line.dropWhile( ch => ch != ':').drop(2)
    val regA = strippedLines(0).toLong
    val regB = strippedLines(1).toLong
    val regC = strippedLines(2).toLong
    val instructions = strippedLines(4).split(",").map( str => str.toInt )

    val output: List[Long] = executeProgram(instructions, regA, regB, regC)

    val str = output.mkString
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
      SPECIFIC_solvePart2_take2(instructions, regB, regC)

    result.toString

  private def SPECIFIC_solvePart2_take2(instructions: Array[Int], regB: Long, regC: Long): Long = {
    // The output must be 16 octal digits long.
    // First, let's create inputs that vary the lowest four octal digits, then filter the ones that yield "2,4".
    var aFor24 = List[List[Long]]()
    for i0 <- 0 to 7 do
      for i1 <- 0 to 7 do
        for i2 <- 0 to 7 do
          for i3 <- 0 to 7 do
            val aAsList = List[Long](i0, i1, i2, i3)
            val res = executeProgram2(instructions, octalListToLong(aAsList), regB, regC)
            if res.startsWith(List(2,4)) then
              aFor24 = aAsList :: aFor24

    // Each time, we take the last two octal digits:
    val lastTwoFor24 = aFor24.map( l => l.takeRight(2) ).distinct
    var resultsStartWith2415 = loop6OctalDigitsPlusTails(lastTwoFor24, instructions, regB, regC, List(2,4,1,5))
    resultsStartWith2415 = resultsStartWith2415.map( l => l.takeRight(4) ).distinct
    var resultsStartWith241575 = loop6OctalDigitsPlusTails(resultsStartWith2415, instructions, regB, regC, List(2,4,1,5,7,5))
    resultsStartWith241575 = resultsStartWith241575.map( l => l.takeRight(6) ).distinct
    var resultsStartWith24157516 = loop6OctalDigitsPlusTails(resultsStartWith241575, instructions, regB, regC, List(2,4,1,5,7,5,1,6))
    resultsStartWith24157516 = resultsStartWith24157516.map( l => l.takeRight(8) ).distinct
    var resultsStartWith2415751603 = loop6OctalDigitsPlusTails(resultsStartWith24157516, instructions, regB, regC, List(2,4,1,5,7,5,1,6,0,3))
    resultsStartWith2415751603 = resultsStartWith2415751603.map( l => l.takeRight(10) ).distinct
    var resultsStartWith241575160342 = loop6OctalDigitsPlusTails( resultsStartWith2415751603, instructions, regB, regC, List(2,4,1,5,7,5,1,6,0,3,4,2))
    resultsStartWith241575160342 = resultsStartWith241575160342.map( l => l.takeRight(12) ).distinct
    var resultsStartWith24157516034255 = loop6OctalDigitsPlusTails( resultsStartWith241575160342, instructions, regB,regC, List(2,4,1,5,7,5,1,6,0,3,4,2, 5, 5))
    resultsStartWith24157516034255 = resultsStartWith24157516034255.map( l => l.takeRight(14) ).distinct

    val possibleA = scala.collection.mutable.Set[Long]()
    for i0 <- 0L to 7L do
      for i1 <- 0L to 7L do
        for tail <- resultsStartWith24157516034255 do
          val aAsList = i0 :: i1 :: tail
          val aAsLong = octalListToLong(aAsList)
          val result = executeProgram2(instructions, aAsLong, regB, regC)
          if result == List(2,4,1,5,7,5,1,6,0,3,4,2, 5, 5, 3, 0) then
            possibleA.add(aAsLong)
    possibleA.min
  }

  private def loop6OctalDigitsPlusTails(
       tails: List[List[Long]],
       instructions: Array[Int],
       regB: Long,
       regC: Long,
       shouldStartWith: List[Long]
  ): List[List[Long]] = {
    var res = List[List[Long]]()
    for i0 <- 0L to 7L do
      for i1 <- 0L to 7L do
        for i2 <- 0L to 7L do
          for i3 <- 0L to 7L do
            for i4 <- 0L to 7L do
              for i5 <- 0L to 7L do
                for tail <- tails do
                  val inputList = List(i0, i1, i2, i3, i4, i5) ++ tail
                  val input = octalListToLong( inputList )
                  val output = executeProgram2(instructions, input, regB, regC)
                  if output.startsWith(shouldStartWith) then
                    res = inputList :: res
    res
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
