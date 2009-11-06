/* CCSTM - (c) 2009 Stanford University - PPL */

// HistogramSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm.collection.TArray
import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ExhaustiveTest
import java.util.concurrent.CyclicBarrier
import org.scalatest.Tag


class HistogramSuite extends STMFunSuite {

  for ((opsPerTest, name, groups) <- List((10000, "10K", List[Tag]()),
                                          (1000000, "1M", List[Tag](ExhaustiveTest)))) {
    for (buckets <- List(1, 30, 10000)) {
      for (threads <- List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512) if (threads <= 2*Runtime.getRuntime.availableProcessors)) {
        for (useTArray <- List(false, true)) {
          val str = ("" + buckets + " buckets, " + threads + " threads, " +
                  (if (useTArray) "TArray[Int]" else "Array[Ref[Int]]"))
          addTest("non-txn, " + str + ", " + name, groups:_*) {
            histogram(buckets, threads, opsPerTest / threads, useTArray, 100, 1)
          }

          for (accesses <- List(1, 3, 100)) {
            addTest("txn, " + str + ", " + accesses + " incr per txn, " + name, groups:_*) {
              histogram(buckets, threads, opsPerTest / threads, useTArray, 0, accesses)
            }
            addTest("mix, " + str + ", " + accesses + " incr per txn " + name, groups:_*) {
              histogram(buckets, threads, opsPerTest / threads, useTArray, 50, accesses)
            }
          }
        }
      }
    }
  }

  private def addTest(name: String, tags: Tag*)(block: => Unit) { test(name, tags:_*)(block) }

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
        val now = System.nanoTime
        if (start == 0) {
          start = now
        } else {
          val elapsed = now - start
          println("hist(" + bucketCount + "," + workerCount + "," + samplesPerWorker + "," +
            useTArray + "," + nonTxnPct +
            "," + samplesPerTxn + ")  total_elapsed=" + elapsed + " nanos,  throughput=" +
            (samplesPerWorker * workerCount * 1000000000L) / elapsed + " ops/sec,  per_thread_latency=" +
            elapsed / samplesPerWorker + " nanos/op,  avg_arrival=" +
            elapsed / (samplesPerWorker * workerCount) + " nanos/op")
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
              if ((i % 2) == 0) {
                buckets(Math.abs(hash(worker, i) % bucketCount)).nonTxn.transform(_ + 1)
              } else {
                val nt = buckets(Math.abs(hash(worker, i) % bucketCount)).nonTxn
                var x = !nt
                while (!nt.compareAndSet(x, x + 1)) x = !nt
              }
              i += 1
            } else {
              new Atomic { def body {
                var j = 0
                while (j < samplesPerTxn && i + j < samplesPerWorker) {
                  val tv = buckets(Math.abs(hash(worker, i + j) % bucketCount))
                  //tv.getAndTransform(_ + 1)
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