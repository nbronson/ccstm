/* CCSTM - (c) 2009 Stanford University - PPL */

// FlipperSuite

package edu.stanford.ppl.ccstm


import java.util.concurrent.CyclicBarrier
import org.scalatest.FunSuite

class FlipperSuite extends FunSuite {
  val DEFAULT_SYNC_COUNT = 3
  val DEFAULT_TRANS_COUNT = 100
  val DEFAULT_INSTR_COUNT = 100
  val DEFAULT_THREAD_COUNT = 4
  val DEFAULT_WORD_COUNT = 4096
  val DEFAULT_FLIP_PROB = 0.5f

  test("default flipper test") {
    Config(
      DEFAULT_SYNC_COUNT,
      DEFAULT_TRANS_COUNT,
      DEFAULT_INSTR_COUNT,
      DEFAULT_THREAD_COUNT,
      DEFAULT_WORD_COUNT,
      DEFAULT_FLIP_PROB,
      0).runTest
  }

  test("4 random flipper tests") {
    for (i <- 0 until 4) {
      Config(
        DEFAULT_SYNC_COUNT,
        DEFAULT_TRANS_COUNT,
        DEFAULT_INSTR_COUNT,
        DEFAULT_THREAD_COUNT,
        DEFAULT_WORD_COUNT,
        DEFAULT_FLIP_PROB,
        System.currentTimeMillis + System.nanoTime).runTest
    }
  }

  case class Config(val syncCount: Int,
                    val transCount: Int,
                    val instrCount: Int,
                    val threadCount: Int,
                    val wordCount: Int,
                    val flipProb: Float,
                    val randSeed: Long) {

    private val len = syncCount*transCount*instrCount*threadCount
    private val rand = new java.util.Random(randSeed)
    val R = Array.fromFunction(i => rand.nextInt(wordCount))(len)
    val F = Array.fromFunction(i => rand.nextDouble() < flipProb)(len)
     
    def index(id: Int, sync: Int, trans: Int, instr: Int) = {
      ((id*syncCount+sync)*transCount+trans)*instrCount+instr;
    }

    def runTest {
      println(this)

      print("computing sequentially...")
      Console.flush

      val P = Array.fromFunction(i => Ref(false))(len)
      val expected = computeSequential(this, P)

      print("\ncomputing in parallel with transactions...")
      Console.flush()
      
      val actual = computeParallelTxn(this, P)

      println()      
      for (i <- 0 until expected.length) {
        assert(expected(i).nonTxn.elem === actual(i).nonTxn.elem)
      }
    }
  }

  abstract class FlipperTask(val config: Config,
                             val A: Array[Ref[Int]],
                             val P: Array[Ref[Boolean]],
                             val computeP: Boolean,
                             val id: Int,
                             val sync: Int) extends (() => Unit) {
    def doWork(task: => Unit)

    def read[T](ref: Ref[T]): T
    def write[T](ref: Ref[T], v: T)

    def apply() {
      val mask = 1 << id
      for (trans <- 0 until config.transCount) {
        doWork {
          for (instr <- 0 until config.instrCount) {
            val i = config.index(id, sync, trans, instr)
            val target = config.R(i)
            val a = read(A(target))
            val p = (a & mask) != 0
            if (computeP) {
              write(P(i), p)
            }
            else {
              assert(read(P(i)) === p)
            }
            if (config.F(i)) {
              // do some work before storing to A, to increase probability of a conflict
              var h = a
              for (j <- 0 until 10000) {
                h |= 1+((h >>> 1)^(h*13))
              }
              if (h == a) println("?")
              write(A(target), a ^ mask)
            }
          }
        }
        //println("thread " + id + " transaction " + trans + " completed (" + computeP + ")")
      }
    }
  }

  def computeSequential(config: Config, P: Array[Ref[Boolean]]): Array[Ref[Int]] = {
    val A = Array.fromFunction(i => Ref(0))(config.wordCount)
    for (sync <- 0 until config.syncCount) {
      for (thread <- 0 until config.threadCount) {
        (new FlipperTask(config, A, P, true, thread, sync) {
          def read[T](ref: Ref[T]): T = ref.nonTxn.elem
          def write[T](ref: Ref[T], v: T) { ref.nonTxn.elem = v }
          def doWork(task: => Unit) { task }
        })()
      }
    }
    A
  }

  def computeParallelTxn(config: Config, P: Array[Ref[Boolean]]): Array[Ref[Int]] = {
    val A = Array.fromFunction(i => Ref(0))(config.wordCount)
    for (sync <- 0 until config.syncCount) {
      val tasks = (for (thread <- 0 until config.threadCount) yield {
        new FlipperTask(config, A, P, false, thread, sync) {
          implicit var txn: Txn = null

          def read[T](ref: Ref[T]): T = !ref
          def write[T](ref: Ref[T], v: T) { ref := v }
          def doWork(task: => Unit) {
            new Atomic { def body {
              txn = currentTxn
              task
            }}.run
            txn = null
          }
        }
      })
      parallelRun(tasks)
    }
    A
  }

  private def parallelRun(tasks: Collection[() => Unit]) {
    val barrier = new CyclicBarrier(tasks.size)
    var failure: Throwable = null
    val threads = for (task <- tasks.toList) yield new Thread {
      override def run {
        barrier.await
        try {
          task()
        } catch {
          case x => failure = x
        }
      }
    }
    for (t <- threads) t.start
    for (t <- threads) t.join
    if (failure != null) throw failure
  }
}
