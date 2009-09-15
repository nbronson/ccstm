/* CCSTM - (c) 2009 Stanford University - PPL */

// Histogram

package edu.stanford.ppl.ccstm


object Histogram {
  def main(args: Array[String]) {
    val BucketCount = 4
    val WorkerCount = 2
    val SamplesPerWorker = 1000
    val SamplesPerTxn = 2
    val buckets = Array.fromFunction(i => TVar(0))(BucketCount)
    val threads = new Array[Thread](WorkerCount)
    
    for (worker <- 0 until WorkerCount) {
      threads(worker) = new Thread("worker " + worker) {
        override def run {
          val begin = System.currentTimeMillis
          var i = 0
          while (i < SamplesPerWorker) {
            if (SamplesPerTxn == 0) {
              buckets(Math.abs(hash(worker, i) % BucketCount)).nonTxn.transform(_ + 1)
              i += 1
            } else {
              new Atomic { def body {
                var j = 0
                while (j < SamplesPerTxn && i + j < SamplesPerWorker) {
                  val tv = buckets(Math.abs(hash(worker, i + j) % BucketCount))
                  tv := !tv + 1
                  j += 1
                }
              }}.run
              i += SamplesPerTxn
            }
          }
          val elapsed = System.currentTimeMillis - begin
          print("thread " + worker + " took " + elapsed + " millis\n")
        }
      }
      threads(worker).start
    }

    for (worker <- 0 until WorkerCount) threads(worker).join

    val sum = buckets.map(_.nonTxn.elem).reduceLeft(_+_)
    println(sum)
    assert(SamplesPerWorker * WorkerCount == sum)
  }

  def hash(i: Int, j: Int) = {
    var h = i * 37 + j * 101
    h ^= (h >>> 20) ^ (h >>> 12)
    h ^ (h >>> 7) ^ (h >>> 4)
  }
}