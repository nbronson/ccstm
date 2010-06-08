/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// DynSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.Dynamic._

class DynSuite extends STMFunSuite {

  test("empty transaction") {
    atomic {
      () // do nothing
    }
  }

  test("atomic function") {
    val answer = atomic {
      42
    }
    assert(Integer.parseInt(answer.toString, 13) === 6*9)
  }

  test("duplicate binding with old access") {
    val x = Ref(1)
    atomic {
      val b1 = x.bind
      assert(b1.get === 1)
      val b2 = x.bind
      assert(b2.get === 1)
      b1() = 2
      assert(b1.get === 2)
      assert(b2.get === 2)
      b2() = 3
      assert(b1.get === 3)
      assert(b2.get === 3)
    }
    assert(x.single.get === 3)
  }

  class UserException extends Exception

  test("failure atomicity") {
    val x = Ref(1)
    intercept[UserException] {
      atomic {
        x() = 2
        throw new UserException
      }
    }
    assert(x.single.get === 1)
  }

  test("non-local return") {
    val x = Ref(1)
    val y = nonLocalReturnHelper(x)
    assert(x.single.get === 2)
    assert(y === 2)
  }

  def nonLocalReturnHelper(x: Ref[Int]): Int = {
    atomic {
      x() = x() + 1
      return x()
    }
    return -1
  }

  test("Atomic.orElse") {
    intercept[UserException] {
      atomic {
        if ("likely".hashCode != 0)
          retry
      } orAtomic {
        throw new UserException
      }
    }
  }

  test("AtomicFunc.orElse") {
    val x = Ref(1)
    def a() = {
      atomic {
        if (x.get > 1) true else retry
      } orAtomic {
        false
      }
    }
    assert(a() === false)
    x.single() = 2
    assert(a() === true)
  }

  test("simple nesting") {
    val x = Ref(10)
    atomic {
      x += 1
      atomic {
        assert(x.get === 11)
        x += 2
        assert(x.bind.mode eq currentTxn)
      }
      assert(x.get === 13)
      assert(x.bind.mode eq currentTxn)
    }
    assert(x.single.get === 13)
  }

  test("mode Single") {
    val x = Ref(10)
    val xs = x.single
    atomic {
      x += 1
      assert(x() === 11)
      assert(xs() === 11)
      xs += 1
      assert(x() === 12)
      assert(xs() === 12)
      x.single += 1
      assert(x() === 13)
      assert(xs() === 13)
      assert(x.single() === 13)
      x.single() = 14
      assert(x() === 14)
      assert(xs.mode == Single)
    }
  }

  test("mode Escaped") {
    val x = Ref(10)
    atomic {
      x += 1
      assert(x() === 11)
      assert(x.escaped() === 10)
      assert(x.escaped.mode == Escaped)
      val f = x.escaped.trySet(20)
      assert(f === false)
    }
  }

  test("uncontended R+W txn perf") {
    val x = Ref("abc")
    var best = java.lang.Long.MAX_VALUE
    for (pass <- 0 until 100000) {
      val begin = System.nanoTime
      var i = 0
      while (i < 5) {
        i += 1
        atomic {
          assert(x() == "abc")
          x() = "def"
        }
        atomic {
          assert(x() == "def")
          x() = "abc"
        }
      }
      val elapsed = System.nanoTime - begin
      best = best min elapsed
    }
    println("uncontended R+W Dynamic txn: best was " + (best / 10.0) + " nanos/txn")

    // We should be able to get less than 5000 nanos, even on a Niagara.
    // On most platforms we should be able to do much better than this.
    // The exception is StripedIntRef, which has relatively expensive reads.
    assert(best / 10 < 5000)
  }
}
