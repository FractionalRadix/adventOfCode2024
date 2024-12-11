package com.cormontia.adventOfCode2024

import scala.io.Source

class SolverDay11 {
  def parseDay11Input(filename: String): List[Long] =
    val source = Source.fromFile(filename)
    val lines = source.getLines.toList
    val line = lines.head
    val numbers1 = line.split("\\s")
    val numbers2 = numbers1.map( str => str.toLong )
    numbers2.toList


  def solvePart1(stones: List[Long]): Long =
    var newStones = stones
    for i <- 1 to 25 do
      println(s"Iteration $i")
      newStones = blink1(newStones)
    //printList(newStones)
    newStones.length

  private def printList(stones: List[Long]): Unit =
    for stone <- stones do
      print(s" [$stone]")

  /**
   * Naive solution to part 1: blink once.
   * After we get this working properly we'll add some caching.
   * @param stones The list of numbers (engravings) on the stones.
   * @return The new series of stones after blinking once.
   */
  private def blink1(stones: List[Long]): List[Long] =
    var result: List[Long] = Nil
    for stone <- stones do
      if stone == 0 then
        result = 1 :: result
      else if stone.toString.length % 2 == 0 then
        val str = stone.toString
        val len = str.length
        val n = len / 2
        // We are prepending the results to make sure they are executed in O(1).
        // This means we are processing the list in REVERSE order!
        // Hence, we must put the rightmost half ("str.drop(n)") in front and the leftmost half behind it.
        val newStone1 = str.drop(n).toLong
        val newStone2 = str.take(n).toLong
        result = newStone1 :: newStone2 :: result
      else
        result = (stone * 2024) :: result
    result.reverse


  private val transitions = scala.collection.mutable.Map[Long, (Long, Option[Long])]()

  /**
   * Given the engraving on a stone (the value to process), update the list of transitions.
   * If the value is already present in our map of transitions, do nothing.
   * Otherwise, apply the three rules once, and add the transitions accordingly.
   * Note that this method updates the map "transitions" !
   * @param engraving The value that is engraved on a given stone.
   */
  private def processEngraving(engraving: Long): Unit =
    if (!transitions.contains(engraving))
      if engraving == 0 then
          transitions(engraving) = (1, None)
      else
        val str = engraving.toString
        val len = str.length
        if len % 2 == 0 then
          val n = len / 2
          val target1 = str.take(n).toLong
          val target2 = str.drop(n).toLong
          transitions(engraving) = (target1, Some(target2))
        else
          transitions(engraving) = (2024 * engraving, None)

  /**
   * For every transition in our list, look at its targets.
   * If these targets do not yet have transitions for themselves, add them.
   */
  private def processAllValues(): Unit =
    for transition <- transitions do
      val target1 = transition._2._1
      processEngraving(target1)
      val target2 = transition._2._2
      if target2.isDefined then
        processEngraving(target2.head)

  private def printTransitions(): Unit =
    for transition <- transitions do
      println(s"${transition._1} -> ${transition._2._1} ${transition._2._2}")

  /**
   * Recursively follow "n" transitions of a given stone.
   * Print the result. (Later we'll just count the number of stones that we transition to).
   * @param engraving The engraving on the current stone.
   * @param n The number of transitions to follow.
   */
  private def followTransitions(engraving: Long, n: Int): Unit =
    if n == 0 then
      print(s"$engraving ")
    else
      val targets = transitions(engraving)
      followTransitions(targets._1, n - 1)
      if targets._2.isDefined then
        followTransitions(targets._2.head, n - 1)

  /**
   * Recursively follow "n" transitions of a given stone.
   * Count the resulting number of stones.
   * @param engraving The engraving on the current stone.
   * @param n         The number of transitions to follow.
   */
  private def followTransitionsAndCount(engraving: Long, n: Int): Long =
    if n == 0 then
      1
    else
      val targets = transitions(engraving)
      var count = followTransitionsAndCount(targets._1, n - 1)
      if targets._2.isDefined then
        count = count + followTransitionsAndCount(targets._2.head, n - 1)
      count

  def solvePart2(input: List[Long]): Long =
    // Let's process the list once...
    for engraving <- input do
      processEngraving(engraving)
    for i <- 1 to 75 do
      processAllValues()
    // Now, for every value, we should also cache what value it yields for (up to) 75 iterations....
    println
    var count: Long = 0
    for i <- input do
      count = count + followTransitionsAndCount(i, 75)
    count



}
