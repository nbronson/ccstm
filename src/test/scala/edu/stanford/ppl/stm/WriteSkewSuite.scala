/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// WriteSkewSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl._


class WriteSkewSuite extends STMFunSuite {
  val IncrCount = 1000000

  test("write skew test 1K") { runTest(1000) }
  test("write skew test 1M", ExhaustiveTest) { runTest(1000000) }              

  def runTest(incrCount: Int) {
    // Two threads, each of which increments its own Ref if the other Ref is
    // even.  Neither thread should ever observe that both Refs are odd.
    // MVCC STMs will require the addition of something like Clojure's "ensure"
    // or SQL's "select for update" to avoid the write skew. 
    val refs = Array(Ref(0), Ref(0))
    val threads = new Array[Thread](2)

    @volatile var failure: Throwable = null
    for (id <- 0 to 1) {
      threads(id) = new Thread("write skew #" + id) {
        val self = refs(id)
        val other = refs(1 - id)

        override def run {
          try {
            for (i <- 0 until incrCount) {
              if (null != failure) return
              new Atomic { def body {
                if ((other() % 2) != 0) {
                  if ((self() % 2) != 0) {
                    fail("refs=" + refs.map(_.get))
                  }
                  retry
                }
                self() = self() + 1
              }}.run
            }
          } catch {
            case x => if (null == failure) failure = x
          }
        }
      }
    }

    val begin = System.currentTimeMillis
    for (t <- threads) t.start
    for (t <- threads) t.join

    if (null != failure) throw failure
    val elapsed = System.currentTimeMillis - begin
    println("writeSkew(" + (2 * incrCount) + "): " + elapsed + " millis total")
  }
}
