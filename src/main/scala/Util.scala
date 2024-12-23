package com.cormontia.adventOfCode2024

import scala.reflect.ClassTag
import scala.util.boundary
import scala.util.boundary.break

class Util {
}

object Util {
  /**
   * Given two lists of Strings, determine all combinations of them.
   * For example, ["hello ","bye "] and ["world", "cat", "dog"] will yield:
   * ["hello world", "bye world", "hello cat", "bye cat", "hello dog", "bye dog"]
   * Note that if either list is empty, the result will also be empty.
   * To get the identity operation, do a cross product with List("").
   *
   * @param l1 A list of Strings.
   * @param l2 A list of Strings.
   * @return A list containing all combinations of the elements of the input lists.
   */
  def crossProduct(l1: List[String], l2: List[String]): List[String] = {
    for e1 <- l1; e2 <- l2 yield e1 + e2
  }

  /**
   * Returns true if and only if the given list is a list of subsequent numbers.
   * For example, (1,2,3,4) yields <code>true</code>; (1,3,4,5) does not as the number 2 is missing.
   *
   * @param list The list to inspect.
   * @return <code>true</code> if and only if the list is a sequence of subsequent numbers.
   */
  def subsequentNumbers(list: List[Long]): Boolean =
    var expected = list.head
    for elt <- list.tail do
      expected = expected + 1
      if elt != expected then
        return false
    true

  /**
   * Determine if the given list contains a contiguous chunk of at least length `n`
   * PREREQUISITES: n > 0, list not empty, n <= length of list.
   * @param list The list to inspect.
   * @param n The minimum size for a series of consecutive values to be considered a chunk.
   * @return <code>True</code> if and only if the list contains a subset of at least `n` consecutive values.
   */
  def containsContiguousChunk(list: List[Long], n: Int): Boolean =
    val sortedList = list.sorted // Sort is ascending by default.
    val windows = sortedList.sliding(n)
    var result = false
    boundary {
      for window <- windows do
        var expected = window.head
        if Util.subsequentNumbers(window) then
          result = true
          break()
    }
    result

  def charGridToSvg(grid: Grid[Char]): String =
    val start = "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n  <meta charset=\"UTF-8\">\n</head>\n<body>\n  <svg width=\"200\" height=\"200\">"
    val end = "  </svg>\n</body>\n</html>"
    val middle = "" //TODO!~
    start ++ middle ++ end

  //TODO!+ Add unit tests! This one has not yet been tested!
  /**
   * Given an array and an "interspersed" value, returns a copy of the array with the "interspersed" elements between
   * the values.
   * For example, intersperse([2,1,4],8) returns [2,8,1,8,4].
   * For example, intersperse(["Do","not","stop","coding"], "!!") returns ["Do", "!!", "not", "!!", "stop", "!!",
   * "coding"]
   * An empty array will result in an empty array: intersperse([], "x") returns [].
   * A singleton array will result in the same singleton array: intersperse(["Hi"],"world") will return ["Hi"].
   * @param arr The array to intersperse.
   * @param elt T The element to put between the array elements.
   * @tparam T The type of the array and the interspersed element.
   * @return A copy of the original array, where the new element is interspersed between the other elements.
   */
  private def intersperse[T: ClassTag](arr: Array[T], elt: T): Array[T] = {
    if arr.isEmpty then
      new Array[T](0) // Array of zero elements.
    else
      val len = 2 * arr.length - 1
      val result = new Array[T](len)
      for i <- 0 to arr.length do
        result(2 * i) = arr(i)
        if 2 * i + 1 < len then
          result(2 * i + 1) = elt
      result
  }

  /**
   * Given a String, return all permutations of the characters in that string.
   * For example, "ABC" will yield ["ABC", "ACB", "BAC", "BCA", "CAB", "CBA"].
   * If characters occur twice, each occurrence will count.
   * For example, "ABB" will yield ["ABB", "BAB", "BBA"].
   * Note that this method has factorial complexity!!
   *
   * @param str A string.
   * @return All permutations of the input string.
   */
  def permutations(str: String): List[String] = {
    if str.isEmpty then
      List("")
    else
      var result = List[String]()
      // For every character in the string, yield: that character plus all permutations of the rest.
      for i <- str.indices do
        val remainingCharacters = str.take(i) + str.drop(i + 1)
        val list0 = permutations(remainingCharacters)
        val list1 = list0.map(x => str(i).toString + x)
        result = result ++ list1
      result.distinct
  }

}
