/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._


class TxnSuite extends STMFunSuite {

  test("empty transaction") {
    new Atomic { def body {
      // do nothing
    }}.run
  }

  test("atomic function") {
    val answer = new AtomicFunc[Int] { def body = {
      42
    }}.run
    assert(Integer.parseInt(answer.toString, 13) === 6*9)
  }

  test("duplicate binding with old access") {
    val x = Ref(1)
    new Atomic { def body {
      val b1 = x.bind
      assert(b1.get === 1)
      val b2 = x.bind
      assert(b2.get === 1)
      b1 := 2
      assert(b1.get === 2)
      assert(b2.get === 2)
      b2 := 3
      assert(b1.get === 3)
      assert(b2.get === 3)
    }}.run
    assert(x.nonTxn.get === 3)
  }

  class UserException extends Exception

  test("failure atomicity") {
    val x = Ref(1)
    intercept[UserException] {
      new Atomic { def body {
        x := 2
        throw new UserException
      }}.run
    }
    assert(x.nonTxn.get === 1)
  }

  test("non-local return") {
    val x = Ref(1)
    val y = nonLocalReturnHelper(x)
    assert(x.nonTxn.get === 2)
    assert(y === 2)
  }

  def nonLocalReturnHelper(x: Ref[Int]): Int = {
    STM.atomic((t: Txn) => {
      implicit val txn = t
      x := !x + 1
      return !x
    })
    return -1
  }

  test("atomicOrElse") {
    val x = Ref(false)
    val y = Ref(false)
    val z = Ref(false)
    for ((ref,name) <- List((x,"x"), (y,"y"), (z,"z"))) {
      new Thread("wakeup") { override def run { Thread.sleep(200) ; ref.nonTxn := true }}.start()

      val result = Ref("")
      var sleeps = 0
      STM.atomicOrElse(
          (txn: Txn) => { result.set("x")(txn) ; if (!x.get(txn)) txn.retry },
          (txn: Txn) => { if (y.get(txn)) result.set("y")(txn) else txn.retry },
          (txn: Txn) => { if (z.get(txn)) result.set("z")(txn) else txn.retry },
          (txn: Txn) => { sleeps += 1; txn.retry }
        )
      ref.nonTxn := false
      assert(result.nonTxn.get === name)
      assert(sleeps <= 1)
    }
  }

  test("Atomic.orElse") {
    intercept[UserException] {
      new Atomic { def body {
        retry
      }} orElse new Atomic { def body {
        throw new UserException
      }} run 
    }
  }

  test("AtomicFunc.orElse") {
    val x = Ref(1)
    val a = new AtomicFunc[Boolean] { def body = {
      if (x.get > 1) true else retry
    }} orElse new AtomicFunc[Boolean] { def body = {
      false
    }}
    assert(a.run() === false)
    x.nonTxn := 2
    assert(a.run() === true)
  }
}