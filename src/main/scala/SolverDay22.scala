package com.cormontia.adventOfCode2024

class SolverDay22 extends Solver {
  override def solvePart1(lines: List[String]): String = {

    //println(mix(42,15)) // Should be 37
    //println(prune(100000000L)) // Should be 16113920

    //var secret = 123L

    val seeds = lines.map( str => str.toLong )
    println(seeds.mkString("..."))

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

  override def solvePart2(lines: List[String]): String = {

    "" //TODO!+
  }
}
