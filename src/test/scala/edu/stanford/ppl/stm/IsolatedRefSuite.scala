/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.collection._
import scala.collection.jcl.IdentityHashMap


/** Performs single-threaded tests of <code>Ref</code>.  Since there is no
 *  outside interference, the tests cover non-transactional and transactional
 *  accesses, with various types and levels of bound view reuse.   
 */
class IsolatedRefSuite extends STMFunSuite {

  /** Binder implementations provide a way of obtaining Ref.Bound instances
   *  from a Ref.  The bound views may be reused, or not, and they may be
   *  non-transactional or transactional.
   */
  sealed trait Binder {
    def apply[T](v: Ref[T]): Ref.Bound[T]
    def reset() {}
  }

  case object FreshNonTxn extends Binder {
    def apply[T](v: Ref[T]) = v.nonTxn
  }

  case object ReuseNonTxn extends Binder {
    private val cache = new IdentityHashMap[Ref[_],Ref.Bound[_]]

    def apply[T](v: Ref[T]) = cache.getOrElseUpdate(v, v.nonTxn).asInstanceOf[Ref.Bound[T]]
  }

  case class FreshTxn(txnLen: Int) extends Binder {
    var txn: Txn = null
    var accesses = 0

    def apply[T](v: Ref[T]) = {
      if (accesses == txnLen) reset()
      accesses += 1
      if (null == txn) txn = new Txn
      v.bind(txn)
    }

    override def reset() {
      if (null != txn) {
        val s = txn.commit()
        assert(s === Txn.Committed)
        txn = null
        accesses = 0
      }
    }
  }

  case class ReuseTxn(txnLen: Int) extends Binder {
    private val cache = new IdentityHashMap[Ref[_],Ref.Bound[_]]
    var txn: Txn = null
    var accesses = 0

    def apply[T](v: Ref[T]) = {
      if (accesses == txnLen) reset()
      accesses += 1
      if (null == txn) txn = new Txn
      cache.getOrElseUpdate(v, v.bind(txn)).asInstanceOf[Ref.Bound[T]]
    }

    override def reset() {
      if (null != txn) {
        val s = txn.commit()
        assert(s === Txn.Committed)
        txn = null
        accesses = 0
      }
      cache.clear
    }
  }


  sealed trait IntRefFactory {
    def apply(initialValue: Int): Ref[Int]
  }

  case object TAnyRefFactory extends IntRefFactory {
    def apply(initialValue: Int): Ref[Int] = new TAnyRef[Int](initialValue)
  }

  case object TIntRefFactory extends IntRefFactory {
    def apply(initialValue: Int): Ref[Int] = new TIntRef(initialValue)
  }

  case object LazyConflictRefFactory extends IntRefFactory {
    def apply(initialValue: Int): Ref[Int] = new LazyConflictRef[Int](initialValue)
  }


  val binders = List(FreshNonTxn, ReuseNonTxn, FreshTxn(1), ReuseTxn(1), FreshTxn(2), ReuseTxn(2), FreshTxn(8), ReuseTxn(8), FreshTxn(1000), ReuseTxn(1000))
  val factories = List(TAnyRefFactory, TIntRefFactory, LazyConflictRefFactory)
  for (f <- factories; b <- binders) createTests(b, f)

  private def createTests(binder: Binder, fact: IntRefFactory) {
    test(fact + ": " + binder + ": counter") {
      val x = fact(1)
      val r = binder(x).get + 1
      binder(x) := r
      assert(binder(x).get === 2)
      binder.reset()
    }

    test(fact + ": " + binder + ": counter increment should be fast", ExhaustiveTest) {
      val x = fact(1)
      var best = java.lang.Long.MAX_VALUE
      for (pass <- 0 until 100000) {
        val begin = System.nanoTime
        for (i <- 0 until 10) {
          val r = binder(x).get + 1
          binder(x) := r
        }
        val elapsed = System.nanoTime - begin
        best = best min elapsed
      }
      assert(binder(x).get === 1000001)
      println("best was " + (best / 10.0) + " nanos/call")
  
      // We should be able to get less than 5000 nanos, even on a Niagara.
      // On most platforms we should be able to do much better than this.
      assert(best / 10 < 5000)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": map") {
      val x = fact(1)
      assert(binder(x).map(_ * 10) === 10)
      binder.reset()
    }

    test(fact + ": " + binder + ": map + write") {
      val x = fact(1)
      binder(x) := 2
      assert(binder(x).map(_ * 10) === 20)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": write + map") {
      val x = fact(1)
      assert(binder(x).map(_ * 10) === 10)
      binder(x) := 2
      binder.reset()
    }

    test(fact + ": " + binder + ": transform") {
      val x = fact(1)
      binder(x).transform(_ + 1)
      assert(binder(x).get === 2)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": successful compareAndSet") {
      val x = fact(1)
      val f = binder(x).compareAndSet(1, 2)
      assert(f)
      assert(binder(x).get === 2)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": failing compareAndSet") {
      val x = fact(1)
      val f = binder(x).compareAndSet(2, 3)
      assert(!f)
      assert(binder(x).get === 1)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": successful compareAndSetIdentity") {
      val ref1 = new java.lang.Integer(3)
      val ref2 = new java.lang.Integer(4)
      val x = Ref(ref1)
      val f = binder(x).compareAndSetIdentity(ref1, ref2)
      assert(f)
      assert(binder(x).get eq ref2)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": failing compareAndSetIdentity") {
      val ref1 = new java.lang.Integer(3)
      val ref2 = new java.lang.Integer(3)
      val ref3 = new java.lang.Integer(4)
      val x = Ref(ref1)
      val f = binder(x).compareAndSetIdentity(ref2, ref3)
      assert(!f)
      assert(binder(x).get eq ref1)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": applicable transformIfDefined") {
      val x = fact(2)
      val pf = new PartialFunction[Int,Int] {
        def isDefinedAt(x: Int): Boolean = x < 8
        def apply(x: Int) = x*x
      }
      val f = binder(x).transformIfDefined(pf)
      assert(f)
      assert(binder(x).get === 4)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": inapplicable transformIfDefined") {
      val x = fact(2)
      val pf = new PartialFunction[Int,Int] {
        def isDefinedAt(x: Int): Boolean = x > 8
        def apply(x: Int) = x*x
      }
      val f = binder(x).transformIfDefined(pf)
      assert(!f)
      assert(binder(x).get === 2)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": successful weakCompareAndSet") {
      val x = fact(1)
      while (!binder(x).weakCompareAndSet(1, 2)) {
        assert(binder(x).get === 1)
      }
      assert(binder(x).get === 2)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": failing weakCompareAndSet") {
      val x = fact(1)
      val f = binder(x).weakCompareAndSet(2, 3)
      assert(!f)
      assert(binder(x).get === 1)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": successful weakCompareAndSetIdentity") {
      val ref1 = new java.lang.Integer(3)
      val ref2 = new java.lang.Integer(4)
      val x = Ref(ref1)
      while (!binder(x).weakCompareAndSetIdentity(ref1, ref2)) {
        assert(binder(x).get eq ref1)
      }
      assert(binder(x).get eq ref2)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": failing weakCompareAndSetIdentity") {
      val ref1 = new java.lang.Integer(3)
      val ref2 = new java.lang.Integer(3)
      val ref3 = new java.lang.Integer(4)
      val x = Ref(ref1)
      val f = binder(x).weakCompareAndSetIdentity(ref2, ref3)
      assert(!f)
      assert(binder(x).get eq ref1)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": unrecordedRead immediate use") {
      val x = fact(1)
      val u = binder(x).unrecordedRead
      assert(u.value === 1)
      assert(u.stillValid)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": unrecordedRead ABA") {
      val x = fact(1)
      val u = binder(x).unrecordedRead
      for (i <- 0 until 1001) binder(x) := 2
      for (i <- 0 until 1001) binder(x) := 1
      assert(u.value === 1)
      assert(binder(x).get === 1)
      assert(!u.stillValid)
      binder.reset()
    }

    test(fact + ": " + binder + ": txn unrecordedRead/write mix") {
      val x = fact(1)
      val b = binder(x)
      if (!b.context.isEmpty) {
        val u1 = b.unrecordedRead
        b := 2
        val u2 = b.unrecordedRead
        b := 3
        val u3 = b.unrecordedRead
        assert(u1.stillValid)
        assert(u1.value === 1)
        assert(u2.stillValid)
        assert(u2.value === 2)
      }
      binder.reset()
    }

    test(fact + ": " + binder + ": simple releasableRead") {
      val x = fact(1)
      val r1 = binder(x).releasableRead
      binder(x) := 2
      val r2 = binder(x).releasableRead
      r1.release()
      r2.release()
      assert(r1.value === 1)
      assert(r2.value === 2)
      r1.release()
      r2.release()
      binder.reset()
    }

    test(fact + ": " + binder + ": tryWrite") {
      val x = fact(1)
      assert(binder(x).get === 1)
      while (!binder(x).tryWrite(2)) {
        assert(binder(x).get === 1)
      }
      assert(binder(x).get === 2)
      binder.reset()
    }

  }
}