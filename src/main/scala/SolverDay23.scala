package com.cormontia.adventOfCode2024

class SolverDay23 extends Solver {

  override def solvePart1(lines: List[String]): String = {
    val pairs = for line <- lines yield
      val c1 = line.take(2)
      val c2 = line.slice(3, 5)
      (c1, c2)

    // All pairs where at least one of the computer names starts with a 't'.
    val firstSetPairs = pairs.filter((c1, c2) => c1(0) == 't' || c2(0) == 't').toSet
    // All computers separately.
    val computers1 = pairs.map((c1,c2) => c1)
    val computers2 = pairs.map((c1,c2) => c2)
    val computers = computers1.concat(computers2).toSet

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
      val newCandidateClusters = scala.collection.mutable.Set[Set[String]]()
      println(s"Checking ${candidateClusters.size} candidate clusters.")
      var counter = 0
      for cluster <- candidateClusters do
        if counter % 100 == 0 then print(s"$counter ")
        counter = counter + 1
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

  private def ALT_solvePart2(lines: List[String]): String = {
    val pairs = for line <- lines yield
      val c1 = line.take(2)
      val c2 = line.slice(3, 5)
      (c1, c2)

    val computers = (pairs.map((c,_) =>c) ++ pairs.map((_,c) => c)).toSet

    // Populate the initial set of clusters.
    var clusters = scala.collection.mutable.Set[Set[String]]()
    for pair <- pairs do
      clusters.add(Set(pair._1, pair._2))
    // For every cluster, find equal-sized other clusters that are completely separate from it.
    // If every member of the original cluster is connected to every member of the other cluster,
    // then we have a new cluster that is twice as large.
    // In this way, we should approach the size of the largest cluster: it is between 2 ** N and 2 ** (N+1)
    // We return the latest non-empty set of clusters.
    // (Is this correct, or is it possible that we miss the largest cluster?)
    var newClustersAvailable = true
    while newClustersAvailable do
      val newClusters = scala.collection.mutable.Set[Set[String]]()
      for cluster1 <- clusters; cluster2 <- clusters if cluster1.intersect(cluster2).isEmpty do
        val formsBiggerCluster = cluster1.forall( member => connectedToAll(cluster2, member, pairs))
        if formsBiggerCluster then
          newClusters.add(cluster1.union(cluster2))
      if newClusters.nonEmpty then
        newClustersAvailable = true
        clusters = newClusters

    // At this point "clusters" should contain all clusters of size 2 ** N.





    //TODO!+
    ""
  }

  private def isConnected(c1: String, c2: String, pairs: List[(String, String)]): Boolean = {
    pairs.contains(c1,c2) || pairs.contains(c2,c1)
  }

  private def connectedToAll(cluster: Set[String], computer: String, connections: List[(String,String)]): Boolean = {
    // For every member of the cluster, is it connected to this computer?
    cluster.forall( member => isConnected(member, computer, connections))
  }


}
