package com.cormontia.adventOfCode2024

class SolverDay23 extends Solver {

  override def solvePart1(lines: List[String]): String = {
    val skipPart1 = true

    if skipPart1 then
      "Part 1 temporarily skipped"
    else
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

      //println(firstSetPairs)
      //println(computers)

      val triplets = scala.collection.mutable.Set[Set[String]]()
      for firstSetPair <- firstSetPairs do
        // Is there a third computer that is connected to both?
        // Find all computers that are connected to BOTH c1 and c2.
        for computer <- computers do
          if isConnected(computer, firstSetPair._1) & isConnected(computer, firstSetPair._2) then
            val triplet = Set(firstSetPair._1, firstSetPair._2, computer)
            triplets.add(triplet)
        ;

      def isConnected(c1: String, c2: String) = pairs.contains((c1,c2)) || pairs.contains((c2,c1))

      triplets.size.toString
  }

  override def solvePart2(lines: List[String]): String = {
    // A cluster is a set of computers.
    val pairs = for line <- lines yield
      val c1 = line.take(2)
      val c2 = line.slice(3, 5)
      (c1, c2)

    // All computers separately.
    val computers1 = pairs.map((c1, c2) => c1)
    val computers2 = pairs.map((c1, c2) => c2)
    val computers = computers1.concat(computers2).toSet

    val candidateClusters1 = pairs.map((c1,c2) => Set(c1,c2))
    var candidateClusters = scala.collection.mutable.Set[Set[String]]()
    candidateClusters.addAll(candidateClusters1)
    println(candidateClusters)

    var found = false
    var largestCluster = scala.collection.mutable.Set[Set[String]]()
    while !found do
      var newCandidateClusters = scala.collection.mutable.Set[Set[String]]()
      for cluster <- candidateClusters do
        // For every computer NOT in the cluster, check if it is connected to EVERY computer in the cluster.
        for computer <- computers if !cluster.contains(computer) do
          if connectedToAll(cluster, computer, pairs) then
            val newCluster = cluster union Set(computer)
            //newCandidateClusters = newCluster :: newCandidateClusters
            newCandidateClusters.add(newCluster)
      if newCandidateClusters.isEmpty then
        found = true
        largestCluster = candidateClusters
      println(newCandidateClusters)
      println(newCandidateClusters.size)
      candidateClusters = newCandidateClusters

    println(largestCluster)
    val computersInLargestCluster = largestCluster.toList.head // Set should be only 1 element.
    val names = computersInLargestCluster.toList.sorted

    names.mkString(",")

  }

  private def isConnected(c1: String, c2: String, pairs: List[(String, String)]): Boolean = {
    pairs.contains(c1,c2) || pairs.contains(c2,c1)
  }

  private def connectedToAll(cluster: Set[String], computer: String, connections: List[(String,String)]): Boolean = {
    // For every member of the cluster, is it connected to this computer?
    cluster.forall( member => isConnected(member, computer, connections))
  }


}
