/*
 * Copyright (c) 2013, Martin Zuber
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * - Redistributions in binary form must reproduce the above
 *   copyright notice, this list of conditions and the following
 *   disclaimer in the documentation and/or other materials provided
 *   with the distribution.
 * - Neither the name of the TU Berlin nor the names of its
 *   contributors may be used to endorse or promote products derived
 *   from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

import Fresh._
import BenchmarkDataGenerator._
import scala.compat.Platform.currentTime


/**
  * A benchmark program for comparing the performance of
  * the 'swap' implementation in Fresh Scala and Fresh O'Caml.
  */
object Benchmark {

  /**
    * Directory where the generated Fresh O'Caml benchmark programs will be
    * saved. The benchmark program assumes writing permissions for this directory.
    */
  val mlDirectory = "."


  /**
    * Store the benchmark result, i.e. the measured time, and a corresponding
    * Fresh O'Caml program which performs the same benchmark run.
    */
  case class BenchmarkResult(swapTime: Long, mlProgram: String)


  /**
    * Run the benchmark for a term with the given number of abstraction.
    *
    * This method will generate a random term (of a fixed object language)
    * containing 'n' abstraction values and perform a swapping operation on
    * this term.
    *
    * @return Time needed to perform the swapping operation in milliseconds
    *         for each 'swap' implementation, i.e., the Kiama-based and the
    *         Shapeless-based one. Additionaly, a String containing a Fresh
    *         O'Caml program which performs the same swapping operation on
    *         the same term of the object language will be stored in the
    *         returned benchmark result.
    */
  def benchmarkRun(n: Int): BenchmarkResult = {
    /* Generate some fresh names */
    val x: Name[Var] = fresh()
    val y: Name[Var] = fresh()

    /* Generate a object-language term which contains 'x' at least one time */
    val term = generate(n, x)

    /* Run Kiama-based 'swap' implementation */
    val timeBefore = currentTime
    swap(x, y, term)
    val timeAfter = currentTime

    BenchmarkResult(timeAfter - timeBefore, term.mlCode)
  }


  def main(args: Array[String]) {
    /* Warmup phase */
    println("Warming up ...")
    for (n <- List(10000, 100000, 1000000)) benchmarkRun(n)
    
    /* Number of abstractions used in each benchmark run */
    val termSizes = List(10, 100, 1000, 5000, 10000, 50000, 100000, 500000, 1000000)

    /* Run benchmark for each term size */
    println("n | FreshScala")
    for (n <- termSizes) {
      val result = benchmarkRun(n)
      println(n + " | " + result.swapTime)
    }
  }

}
