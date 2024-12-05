package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay05 {
  def parseDay05Input(filename: String): List[String] = {
    val source = Source.fromFile(filename)
    source.getLines.toList
  }

  def solvePart2(input: List[String]): Int = {
    val rules = parseRules(input)
    //rules.foreach( (pair) => println(s"(${pair._1},${pair._2})"))
    val pageLists = parsePageLists(input)
    //pageLists.foreach( (list) => println(s"${list.mkString(",")}"))

    val incorrectPageLists = for pageList <- pageLists if validate(pageList, rules)
      yield pageList

    var sum = 0
    for pageList <- incorrectPageLists do
      println(s"${pageList.mkString(",")}")
      //TODO!~  Not yet working right. Probably needs multiple swaps, until the rules are all obeyed...
      var correctedList = pageList
      for rule <- rules do
        correctedList = applyRule(correctedList, rule)
      println(s"Corrected: ${correctedList.mkString(",")}")
      val arrayLen = correctedList.length
      val idx = (arrayLen - 1) / 2
      val middle = correctedList(idx)
      sum = sum + middle

    sum

  }

  private def applyRule(pageList: Array[Int], rule: (Int, Int)): Array[Int] = {
    val fst = rule._1
    val snd = rule._2
    val indexOfFst = pageList.indexOf(fst)
    val indexOfSnd = pageList.indexOf(snd)
    if indexOfFst > 0 && indexOfSnd > 0 then
      if indexOfSnd > indexOfFst then
        return copyAndSwap(pageList, indexOfFst, indexOfSnd)
    pageList
  }

  private def copyAndSwap(array: Array[Int], fst: Int, snd: Int): Array[Int] = {
    //println("Swapping..")
    val result = Array.ofDim[Int](array.length)
    //println(s"Size allocated: ${result.length}")

    for i <- array.indices do
      result(i) = if i == fst then
        array(snd)
      else if i == snd then
        array(fst)
      else
        array(i)

    result.toArray
  }

  def solvePart1(input: List[String]): Int = {
    val rules = parseRules(input)
    //rules.foreach( (pair) => println(s"(${pair._1},${pair._2})"))
    val pageLists = parsePageLists(input)
    //pageLists.foreach( (list) => println(s"${list.mkString(",")}"))

    var sum = 0
    for pageList <- pageLists do
        val valid = validate(pageList, rules)
        if valid then
          //println(s"${valid}")
          val arrayLen = pageList.length
          val idx = (arrayLen - 1) / 2
          val middle = pageList(idx)
          sum = sum + middle

    sum
  }

  private def validate(pageList: Array[Int], rules: List[(Int, Int)]): Boolean = {
    var valid = true
    for rule <- rules do
      val obeyed = validate(pageList, rule)
      valid = valid && obeyed
      //println(s"Rule $rule is $obeyed.")

    valid
  }

  private def validate(pageList: Array[Int], rule: (Int, Int)): Boolean = {
    val first = rule._1
    val second = rule._2
    val tail = pageList.dropWhile( (pageNr) => pageNr != second )

    if tail.length == 0 then
      true
    else if tail.contains(first) then
      false
    else
      true
  }

  private def parseRules(input: List[String]): List[(Int,Int)] = {
    val rules = input.filter( str => str.contains("|") )
    val pairs = for rule <- rules
      yield {
        val stringPair = rule.split('|')
        val first = stringPair(0).toInt
        val second = stringPair(1).toInt
        (first, second)
      }
    pairs
  }

  private def parsePageLists(input: List[String]): List[Array[Int]] = {
    val pageLists = input.filter( str => str.contains(",") )
    val parsedLists = for pageList <- pageLists
      yield parsePageList(pageList)
    parsedLists
  }

  private def parsePageList(input: String): Array[Int] = {
    val parts = input.split(",")
    val numbers = parts.map( str => str.toInt )
    numbers
  }




}
