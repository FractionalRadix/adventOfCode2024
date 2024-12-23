package com.cormontia.adventOfCode2024

class SolverDay23 extends Solver {

  override def solvePart1(lines: List[String]): String = {
    val pairs = for line <- lines yield
      val c1 = line.take(2)
      val c2 = line.slice(3, 5)
      //println(s"$c1 connected with $c2")
      (c1, c2)

    // All pairs where at least one of the computer names starts with a 't'.
    val firstSetPairs = pairs.filter((c1, c2) => c1(0) == 't' || c2(0) == 't').toSet
    // All computers separately.
    val computers1 = pairs.map((c1,c2) => c1)
    val computers2 = pairs.map((c1,c2) => c2)
    val computers = computers1.concat(computers2).toSet

    println(firstSetPairs)
    println(computers)


    val triplets = scala.collection.mutable.Set[Set[String]]()
    for firstSetPair <- firstSetPairs do
      // Is there a third computer that is connected to both?
      // Find all computers that are connected to BOTH c1 and c2.
      for computer <- computers do
        if isConnected(computer, firstSetPair._1) & isConnected(computer, firstSetPair._2) then
          //println(s"$firstSetPair,$computer")
          val triplet = Set(firstSetPair._1, firstSetPair._2, computer)
          triplets.add(triplet)
    //println(triplets)
      ;


    def isConnected(c1: String, c2: String) = pairs.contains((c1,c2)) || pairs.contains((c2,c1))


    triplets.size.toString
  }

  override def solvePart2(lines: List[String]): String = {
    //TODO!+
    ""
  }
}
