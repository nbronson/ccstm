/* CCSTM - (c) 2009 Stanford University - PPL */

// HistogramSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm.collection.TArray
import edu.stanford.ppl.ccstm._
import java.util.concurrent.CyclicBarrier


class HistogramSuite extends STMFunSuite {

  val OpsPerTest = 1000000

  for (buckets <- List(1, 30, 10000)) {
    for (threads <- List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512) if (threads <= 4*Runtime.getRuntime.availableProcessors)) {
      for (useTArray <- List(false, true)) {
        val str = ("" + buckets + " buckets, " + threads + " threads, " +
                (if (useTArray) "TArray[Int]" else "Array[Ref[Int]]"))
        addTest("non-txn, " + str) {
          histogram(buckets, threads, OpsPerTest / threads, useTArray, 100, 1)
        }

        for (accesses <- List(1, 2, 10, 1000)) {
          addTest("txn, " + str + ", " + accesses + " incr per txn") {
            histogram(buckets, threads, OpsPerTest / threads, useTArray, 0, accesses)
          }
          addTest("mix, " + str + ", " + accesses + " incr per txn") {
            histogram(buckets, threads, OpsPerTest / threads, useTArray, 50, accesses)
          }
        }
      }
    }
  }

  private def addTest(name: String)(block: => Unit) { test(name)(block) }

  def histogram(bucketCount: Int,
                workerCount: Int,
                samplesPerWorker: Int,
                useTArray: Boolean,
                nonTxnPct: Int,
                samplesPerTxn: Int) {

    val buckets: Seq[Ref[Int]] = (if (useTArray) {
      new TArray[Int](bucketCount).refs
    } else {
      // Array.fromFunction results in buckets being a BoxedAnyArray, which has
      // synchronization on each access
      //Array.fromFunction(i => Ref(0))(bucketCount)
      val a = new Array[Ref[Int]](bucketCount)
      for (i <- 0 until a.length) a(i) = Ref(0)
      a
    })
    val threads = new Array[Thread](workerCount)
    val barrier = new CyclicBarrier(workerCount, new Runnable {
      var start = 0L
      def run {
        val now = System.currentTimeMillis
        if (start == 0) {
          start = now
        } else {
          val elapsed = now - start
          println("hist(" + bucketCount + "," + workerCount + "," + samplesPerWorker + "," +
            useTArray + "," + nonTxnPct +
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

    val sum = buckets.map(_.nonTxn.get).reduceLeft(_+_)
    assert(samplesPerWorker * workerCount === sum)
  }

  private def hash(i: Int, j: Int) = {
    var h = i * 37 + j * 101
    h ^= (h >>> 20) ^ (h >>> 12)
    h ^ (h >>> 7) ^ (h >>> 4)
  }
}