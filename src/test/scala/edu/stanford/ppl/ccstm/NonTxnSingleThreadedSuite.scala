/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnSuite

package edu.stanford.ppl.ccstm


import org.scalatest.FunSuite

class NonTxnSingleThreadedSuite extends FunSuite {

  test("Counter increment") {
    val x = TVar(1)
    x.nonTxn.elem = x.nonTxn.elem + 1
    assert(x.nonTxn.elem == 2)
  }

  test("Counter increment should be fast") {
    val x = TVar(1)
    var best = java.lang.Long.MAX_VALUE
    for (pass <- 0 until 100000) {
      val begin = System.nanoTime
      for (i <- 0 until 10) {
        x.nonTxn.elem = x.nonTxn.elem + 1
      }
      val elapsed = System.nanoTime - begin
      best = best min elapsed
    }
    assert(x.nonTxn.elem == 1000001)
    println("best was " + (best / 10.0) + " nanos/call")

    // We should be able to get less than 5000 nanos, even on a Niagara.
    // On most platforms we should be able to do much better than this.
    assert(best / 10 < 5000)
  }

  test("Transforms") {
    val x = TVar(1)
    assert(x.nonTxn.elem == 1)
    assert(x.nonTxn.elemMap(_ * 10) == 10)
    x.nonTxn.transform(_ + 1)
    assert(x.nonTxn.elem == 2)
    val f1 = x.nonTxn.compareAndSet(2, 3)
    assert(f1 && x.nonTxn.elem == 3)
    val f2 = x.nonTxn.compareAndSet(2, 4)
    assert(!f2 && x.nonTxn.elem == 3)
    val pf = new PartialFunction[Int,Int] {
      def isDefinedAt(x: Int): Boolean = x < 8
      def apply(x: Int) = x*x
    }
    val f3 = x.nonTxn.transformIfDefined(pf)
    assert(f3 && x.nonTxn.elem == 9)
    val f4 = x.nonTxn.transformIfDefined(pf)
    assert(!f4 && x.nonTxn.elem == 9)

    while (!x.nonTxn.weakCompareAndSet(9, 10)) {
      assert(x.nonTxn.elem == 9)
    }
    assert(x.nonTxn.elem == 10)

    val ref1 = new java.lang.Integer(5)
    val ref2 = new java.lang.Integer(5)
    val ref3 = new java.lang.Integer(5)
    val ref4 = new java.lang.Integer(6)
    val r = TVar(ref1)
    assert(r.nonTxn.elem eq ref1)
    assert(!(r.nonTxn.elem eq ref2))
    val f5 = r.nonTxn.compareAndSetIdentity(ref1, ref2)
    assert(f5 && (r.nonTxn.elem eq ref2))
    val f6 = r.nonTxn.compareAndSetIdentity(ref3, ref4)
    assert(!f6 && (r.nonTxn.elem eq ref2))
    val f7 = r.nonTxn.compareAndSet(ref4, ref3)
    assert(!f7 && (r.nonTxn.elem eq ref2))
    val f8 = r.nonTxn.compareAndSet(ref3, ref4)
    assert(f8 && (r.nonTxn.elem eq ref4))

    while (!r.nonTxn.weakCompareAndSetIdentity(ref4, ref2)) {}
    assert(r.nonTxn.elem eq ref2)
  }

  test("Unrecorded read") {
    val x = TVar(1)
    val u = x.nonTxn.unrecordedRead
    assert(u.value == 1)
    assert(u.stillValid)
    x.nonTxn.elem = 2
    assert(u.value == 1)
    assert(!u.stillValid)
    x.nonTxn.elem = 1
    assert(!u.stillValid, "ABA problem")
  }

  test("TryWrite") {
    val x = TVar(1)
    assert(x.nonTxn.elem == 1)
    while (!x.nonTxn.tryWrite(2)) {
      assert(x.nonTxn.elem == 1)
    }
    assert(x.nonTxn.elem == 2)    
  }

  // TODO: parameterize tests as uninterfered tests, apply them with a single non-txn bound instance, with fresh non-txn bound instances, and inside a single txn
}