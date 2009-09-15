/* CCSTM - (c) 2009 Stanford University - PPL */

// HistogramSuite

package edu.stanford.ppl.ccstm


import java.util.concurrent.CyclicBarrier
import org.scalatest.FunSuite

class HistogramSuite extends FunSuite {

  val OpsPerTest = 10000000

  for (buckets <- List(1, 30, 10000)) {
    for (threads <- List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512) if (threads <= 4*Runtime.getRuntime.availableProcessors)) {
      addTest("non-txn, " + buckets + " buckets, " + threads + " threads") {
        histogram(buckets, threads, OpsPerTest / threads, 100, 1)
      }

      for (accesses <- List(1, 2, 10, 1000)) {
        addTest("txn, " + buckets + " buckets, " + threads + " threads, " + accesses + " incr per txn") {
          histogram(buckets, threads, 10000000 / threads, 0, accesses)
        }
        addTest("mix, " + buckets + " buckets, " + threads + " threads, " + accesses + " incr per txn") {
          histogram(buckets, threads, 10000000 / threads, 50, accesses)
        }
      }
    }
  }

  private def addTest(name: String)(block: => Unit) { test(name)(block) }

  def histogram(bucketCount: Int,
                workerCount: Int,
                samplesPerWorker: Int,
                nonTxnPct: Int,
                samplesPerTxn: Int) {

    val buckets = Array.fromFunction(i => TVar(0))(bucketCount)
    val threads = new Array[Thread](workerCount)
    val barrier = new CyclicBarrier(workerCount, new Runnable {
      var start = 0L
      def run {
        val now = System.currentTimeMillis
        if (start == 0) {
          start = now
        } else {
          val elapsed = now - start
          println("hist(" + bucketCount + "," + workerCount + "," + samplesPerWorker + "," + nonTxnPct +
            "," + samplesPerTxn + ")  total_elapsed=" + elapsed + " msec,  throughput=" +
            (samplesPerWorker * workerCount * 1000L) / elapsed + " ops/sec,  per_thread_latency=" +
            (elapsed * 1000000L) / samplesPerWorker + " nanos/op,  avg_arrival=" +
            (elapsed * 1000000L) / (samplesPerWorker * workerCount) + " nanos/op")
        }
      }
    })
    
    for (worker <- 0 until workerCount) {
      val work = new Runnable {
        def run {
          barrier.await
          var i = 0
          while (i < samplesPerWorker) {
            if (Math.abs(hash(i, worker) % 100) < nonTxnPct) {
              buckets(Math.abs(hash(worker, i) % bucketCount)).nonTxn.transform(_ + 1)
              i += 1
            } else {
              new Atomic { def body {
                var j = 0
                while (j < samplesPerTxn && i + j < samplesPerWorker) {
                  val tv = buckets(Math.abs(hash(worker, i + j) % bucketCount))
                  //tv.transform(_ + 1)
                  tv := !tv + 1
                  //tv := tv.bind.readForWrite + 1
                  j += 1
                }
              }}.run
              i += samplesPerTxn
            }
          }
          barrier.await
        }
      }
      if (worker < workerCount - 1) {
        threads(worker) = new Thread(work, "worker " + worker)
        threads(worker).start
      } else {
        work.run
      }
    }

    for (worker <- 0 until workerCount - 1) threads(worker).join

    val sum = buckets.map(_.nonTxn.elem).reduceLeft(_+_)
    assert(samplesPerWorker * workerCount === sum)
  }

  private def hash(i: Int, j: Int) = {
    var h = i * 37 + j * 101
    h ^= (h >>> 20) ^ (h >>> 12)
    h ^ (h >>> 7) ^ (h >>> 4)
  }
}