/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._


class TxnSuite extends STMFunSuite {

  test("empty transaction") {
    atomic { implicit t =>
      () // do nothing
    }
  }

  test("atomic function") {
    val answer = atomic { implicit t =>
      42
    }
    assert(Integer.parseInt(answer.toString, 13) === 6*9)
  }

  test("duplicate binding with old access") {
    val x = Ref(1)
    atomic { implicit t =>
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
      atomic { implicit t =>
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
    atomic { implicit t =>
      x() = x() + 1
      return x()
    }
    return -1
  }

  test("atomicOrElse") {
    val x = Ref(false)
    val y = Ref(false)
    val z = Ref(false)
    for ((ref,name) <- List((x,"x"), (y,"y"), (z,"z"))) {
      new Thread("wakeup") { override def run { Thread.sleep(200) ; ref.single() = true }}.start()

      val result = Ref("")
      var sleeps = 0
      STM.atomicOrElse(
          (txn: Txn) => { result.set("x")(txn) ; if (!x.get(txn)) txn.retry },
          (txn: Txn) => { if (y.get(txn)) result.set("y")(txn) else txn.retry },
          (txn: Txn) => { if (z.get(txn)) result.set("z")(txn) else txn.retry },
          (txn: Txn) => { sleeps += 1; txn.retry }
        )
      ref.single() = false
      assert(result.single.get === name)
      assert(sleeps <= 1)
    }
  }

  test("Atomic.orElse") {
    intercept[UserException] {
      atomic { implicit t =>
        if ("likely".hashCode != 0)
          retry        
      } orAtomic { implicit t =>
        throw new UserException
      }
    }
  }

  test("AtomicFunc.orElse") {
    val x = Ref(1)
    def a() = {
      atomic { implicit t =>
        if (x.get > 1) true else retry
      } orAtomic { implicit t =>
        false
      }
    }
    assert(a() === false)
    x.single() = 2
    assert(a() === true)
  }

  test("STM.transform2") {
    val x = Ref(10)
    val y = Ref.lazyConflict(20)
    val z = STM.transform2(x, y, (a: Int, b: Int) => (a+2*b, b+2*a, a*b))
    assert(x.single.get === 50)
    assert(y.single.get === 40)
    assert(z === 200)
  }

  test("simple nesting") {
    val x = Ref(10)
    atomic { implicit t =>
      x += 1
      atomic { implicit t =>
        assert(x.get === 11)
        x += 2
        assert(x.bind.mode eq t)
      }
      assert(x.get === 13)
      assert(x.bind.mode eq t)
    }
    assert(x.single.get === 13)
  }

  test("mode Single") {
    val x = Ref(10)
    val xs = x.single
    atomic { implicit t =>
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
    atomic { implicit t =>
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
        atomic { implicit t =>
          assert(x() == "abc")
          x() = "def"
        }
        atomic { implicit t =>
          assert(x() == "def")
          x() = "abc"
        }
      }
      val elapsed = System.nanoTime - begin
      best = best min elapsed
    }
    println("uncontended R+W txn: best was " + (best / 10.0) + " nanos/txn")

    // We should be able to get less than 5000 nanos, even on a Niagara.
    // On most platforms we should be able to do much better than this.
    // The exception is StripedIntRef, which has relatively expensive reads.
    assert(best / 10 < 5000)
  }

  def testAfterCommit(name: String, f1: (Txn, (Txn => Unit)) => Unit, f2: (Txn, (Txn => Unit),Int) => Unit) {
    test(name) {
      var ran = false
      atomic { implicit t =>
        f1(t, { t2 =>
          assert(t === t2)
          assert(!ran)
          ran = true
        })
      }
      assert(ran)
    }

    test(name + " with priority") {
      var first = false
      var second = false
      atomic { implicit t =>
        f1(t, { t2 =>
          assert(t === t2)
          assert(first)
          assert(!second)
          second = true
        })
        f2(t, { t2 =>
          assert(t === t2)
          assert(!first)
          assert(!second)
          first = true
        }, -10)
      }
      assert(first)
      assert(second)
    }
  }

  testAfterCommit("afterCompletion", { (t, h) => t.afterCompletion(h) }, { (t, h, p) => t.afterCompletion(h, p) })
  testAfterCommit("afterCommit", { (t, h) => t.afterCommit(h) }, { (t, h, p) => t.afterCommit(h, p) })

  test("afterRollback on commit") {
    atomic { implicit t =>
      t.afterRollback { _ => assert(false) }
    }
  }

  test("afterRollback on rollback") {
    val x = Ref(10)
    var ran = false
    atomic { implicit t =>
      t.afterRollback { _ =>
        assert(!ran)
        ran = true
      }
      if (x() == 10) {
        val adversary = new Thread {
          override def run() {
            x.single.transform(_ + 1)
          }
        }
        adversary.start()
        adversary.join()
        x()
        assert(false) // this relies on the semantics of CCSTM, some STMs could reach here safely
      }
    }
    assert(ran)
  }

  test("afterCommit runs a txn") {
    var ran = false
    val x = Ref(0)
    atomic { implicit t =>
      x() = 1
      t.afterCommit { _ =>
        atomic { implicit t =>
          assert(!ran)
          ran = true
          assert(x() === 1)
          x() = 2
        }
      }
    }
    assert(ran)
    assert(x.single() === 2)
  }
}
