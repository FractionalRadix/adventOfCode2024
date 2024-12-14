package com.cormontia.adventOfCode2024

class Util {
}

object Util {
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
    for window <- windows do
      var expected = window.head
      if Util.subsequentNumbers(window) then
        return true
    false

}
