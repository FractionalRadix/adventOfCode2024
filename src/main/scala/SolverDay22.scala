package com.cormontia.adventOfCode2024

class SolverDay22 extends Solver {
  override def solvePart1(lines: List[String]): String = {

    //println(mix(42,15)) // Should be 37
    //println(prune(100000000L)) // Should be 16113920

    //var secret = 123L

    val seeds = lines.map( str => str.toLong )
    //println(seeds.mkString("..."))

    // This should be made functional.
    // The procedural approach just made it easier to show intermediate values, to verify if we were on the right track.
    var sum = 0L
    for seed <- seeds do
      var secret = seed
      for i <- 1 to 2000 do
        secret = iterate(secret)
      println(s"$seed: $secret")
      sum = sum + secret
    println(sum)

    sum.toString
  }

  override def solvePart2(lines: List[String]): String = {
    val seeds = lines.map( str => str.toLong )
    //TODO!+ Use these seeds to find the best deals....

    // (For now we're just testing what we've built so far).


    val prices = lastDigits(123L, 10)
    println(prices) // Should be 3,0,6,5,4,4,6,4,4,2
    val changes = differenceList(prices)
    println(changes) // Should be -3,6,-1,-1,0,2,-2,0,-2

    // Let's find the highest price available for each sequence of 4 changes.
    val windowed = changes.sliding(4).toList
    println(windowed)
    // Now let's make a list of the highest value each sequence can give us.
    // DUPLICATED sequences don't work! You must always go for the first occurrence of each sequence.
    // Note that there's 19 ** 4 possible sequences - that's 130321 possible sequences.
    // First, let's restore the link between sequences and values:
    val seqVal = windowed.zip(prices.drop(4))
    println(seqVal)
    val seqVal2 = seqVal.map( (a,b) => (listOfFourToTuple(a), b) )
    println(seqVal2)
    val map = buildMap(seqVal2)
    println(map)


    "" //TODO!+
  }

  private case class Sequence(a0: Long, a1: Long, a2: Long, a3: Long)

  private def listOfFourToTuple(l: List[Long]) = Sequence(l(0),l(1),l(2),l(3))
  /**
   * Given a list of pairs [(a,b,c,d),e], build the map from (a,b,c,d) to e.
   * If the same key occurs multiple times, use only the FIRST occurrence.
   * For example, [((1,2,3,4),8), ((1,2,3,4),8)] will result in a map yielding 8 for key (1,2,3,4).
   * @param sequencesAndValues A list of pairs. The first half of each pair should be a list of 4 Longs. The second half should be a single Long.
   * @return The mapping from the first half of the list to the second.
   */
  private def buildMap(sequencesAndValues: List[(Sequence, Long)]): Map[Sequence, Long] = {
    val map = scala.collection.mutable.Map[Sequence, Long]()
    for elt <- sequencesAndValues do
      val key = elt._1
      if !map.contains(key) then
        map(key) = elt._2
    map.toMap
  }


  private def lastDigits(initialSecret: Long, nrOfIterations: Int): List[Long] = {
    var secret = initialSecret
    // We're actually doing n-1 iterations, the initial secret counts as the first iteration.
    val lastDigits = for i <- 1 until nrOfIterations yield
      secret = iterate(secret)
      secret % 10
    (initialSecret % 10) :: lastDigits.toList
  }

  private def differenceList(l: List[Long]): List[Long] =
    l.zip(l.tail).map((a,b) => b - a)



  private def iterate(input: Long): Long = {
    var secret = input

    // Calculate the result of multiplying the secret number by 64.
    val t1 = secret << 6
    // Mix this result into the secret number.
    secret = mix(secret, t1)
    // Prune the secret number.
    secret = prune(secret)

    // Calculate the result of dividing the secret by 32, rounding down to the nearest integer
    val t2 = secret >> 5
    // Mix this result in to the secret number.
    secret = mix(secret, t2)
    // Prune the secret number.
    secret = prune(secret)

    // Calculate the result of multiplying the secret number by 2048.
    val t3 = secret << 11
    // Mix this result into the secret number
    secret = mix(secret, t3)
    // Prune the secret number.
    secret = prune(secret)

    // Return the result
    secret
  }

  private def mix(secret: Long, value: Long): Long = {
    secret ^ value
  }

  private def prune(secret: Long): Long = {
    secret & 0xFFFFFF
  }
}
