/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ExhaustiveTest
import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.collection._
import java.util.IdentityHashMap


/** Performs single-threaded tests of <code>Ref</code>.  Since there is no
 *  outside interference, the tests cover non-transactional and transactional
 *  accesses, with various types and levels of bound view reuse.   
 */
class IsolatedRefSuite extends STMFunSuite {

  /** Binder implementations provide a way of obtaining Ref.Bound instances
   *  from a Ref.  The bound views may be reused, or not, and they may be
   *  non-transactional or transactional.
   */
  class Binder(reuse: Boolean, txnLen: Int, single: Boolean) {
    private val cache = new IdentityHashMap[Ref[_],Ref.Bound[_]]
    private var txn: Txn = null
    private var accesses = 0

    def apply[T](v: Ref[T]): Ref.Bound[T] = {
      if (txnLen > 0) {
        if (accesses == txnLen) reset()
        accesses += 1
        if (null == txn) txn = new Txn
      }
      if (reuse) {
        val z = cache.get(v)
        if (null != z) return z.asInstanceOf[Ref.Bound[T]]
      }
      val z = (if (single) {
        v.single
      } else if (txnLen > 0) {
        v.bind(txn)
      } else {
        v.escaped
      })
      if (reuse) cache.put(v, z)
      z
    }

    def reset() {
      if (null != txn) {
        val s = txn.commit()
        assert(s === Txn.Committed)
        txn = null
        accesses = 0
      }
      if (reuse) cache.clear
    }
  }

  case object FreshEscaped extends Binder(false, 0, false)
  case object FreshSingleNonTxn extends Binder(false, 0, true)
  case class FreshSingleTxn(txnLen: Int) extends Binder(false, txnLen, true)
  case class FreshTxn(txnLen: Int) extends Binder(false, txnLen, false)

  case object ReuseEscaped extends Binder(true, 0, false)
  case object ReuseSingleNonTxn extends Binder(true, 0, true)
  case class ReuseSingleTxn(txnLen: Int) extends Binder(true, txnLen, true)
  case class ReuseTxn(txnLen: Int) extends Binder(true, txnLen, false)

  sealed trait IntRefFactory {
    def apply(initialValue: Int): Ref[Int]
    def incrLimitFactor = 1
  }

  case object TAnyRefFactory extends IntRefFactory {
    def apply(initialValue: Int): Ref[Int] = new TAnyRef[Int](initialValue)
  }

  case object TIntRefFactory extends IntRefFactory {
    def apply(initialValue: Int): Ref[Int] = new TIntRef(initialValue)
  }

  case object LazyConflictIntRefFactory extends IntRefFactory {
    def apply(initialValue: Int): Ref[Int] = new LazyConflictIntRef(initialValue)
  }

  case object StripedIntRefFactory extends IntRefFactory {
    def apply(initialValue: Int): Ref[Int] = new StripedIntRef(initialValue)
    override def incrLimitFactor = 3
  }


  val binders = List(
    FreshEscaped, FreshSingleNonTxn, FreshSingleTxn(1), FreshTxn(1),
    FreshSingleTxn(2), FreshTxn(2), FreshSingleTxn(8), FreshTxn(8),
    FreshSingleTxn(1000), FreshTxn(1000), ReuseEscaped, ReuseSingleNonTxn,
    ReuseSingleTxn(1), ReuseTxn(1), ReuseSingleTxn(2), ReuseTxn(2),
    ReuseSingleTxn(8), ReuseTxn(8), ReuseSingleTxn(1000), ReuseTxn(1000))
  val factories = List(TAnyRefFactory, TIntRefFactory, LazyConflictIntRefFactory, StripedIntRefFactory)
  for (f <- factories; b <- binders) createTests(b, f)

  private def createTests(binder: Binder, fact: IntRefFactory) {
    test(fact + ": " + binder + ": counter") {
      val x = fact(1)
      val r = binder(x).get + 1
      binder(x) := r
      assert(binder(x).get === 2)
      binder.reset()
    }

    runIncrTest(fact + ": " + binder + ": transform(_+1)") { x =>
      var i = 0
      while (i < 10) {
        i += 1
        binder(x).transform(_ + 1)
      }
    }

    runIncrTest(fact + ": " + binder + ": x:=!x+1") { x =>
      var i = 0
      while (i < 10) {
        i += 1
        val v = binder(x)()
        binder(x) := v + 1
      }
    }

    if (fact(1).isInstanceOf[IntRef]) {
      runIncrTest(fact + ": " + binder + ": x+=1") { x =>
        var i = 0
        while (i < 10) {
          i += 1
          val b = binder(x)
          b.asInstanceOf[IntRef.Bound] += 1
        }
      }
    }

    def runIncrTest(name: String)(incrTenTimes: Ref[Int] => Unit) {
      test(name + " should be fast", ExhaustiveTest) {
        val x = fact(1)
        var best = java.lang.Long.MAX_VALUE
        for (pass <- 0 until 10000) {
          val begin = System.nanoTime
          incrTenTimes(x)
          val elapsed = System.nanoTime - begin
          best = best min elapsed
        }
        assert(binder(x).get === 100001)
        println(name + ": best was " + (best / 10.0) + " nanos/call")

        // We should be able to get less than 5000 nanos, even on a Niagara.
        // On most platforms we should be able to do much better than this.
        // The exception is StripedIntRef, which has relatively expensive reads.
        assert(best / 10 < 5000 * fact.incrLimitFactor)
        binder.reset()
      }
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

    class UserException extends Exception
  
    test(fact + ": " + binder + ": excepting transform") {
      val x = fact(1)
      intercept[UserException] {
        binder(x).transform(v => throw new UserException)
      }
      assert(binder(x).get === 1)
      binder(x).transform(_ + 1)
      assert(binder(x).get === 2)
      binder.reset()
    }

    test(fact + ": " + binder + ": tryTransform") {
      val x = fact(1)
      while (!binder(x).tryTransform(_ + 1)) {
        assert(binder(x) === 1)
      }
      assert(binder(x).get === 2)
      binder.reset()
    }

    test(fact + ": " + binder + ": excepting tryTransform") {
      val x = fact(1)
      intercept[UserException] {
        binder(x).tryTransform(v => throw new UserException)
      }
      assert(binder(x).get === 1)
      while (!binder(x).tryTransform(_ + 1)) {
        assert(binder(x).get === 1)
      }
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
  
    test(fact + ": " + binder + ": excepting transformIfDefined") {
      val x = fact(1)
      val pfThrowEarly = new PartialFunction[Int,Int] {
        def isDefinedAt(x: Int): Boolean = throw new UserException
        def apply(x: Int) = x*x
      }
      intercept[UserException] {
        binder(x).transformIfDefined(pfThrowEarly)
      }
      assert(binder(x).get === 1)
      val pfThrowLate = new PartialFunction[Int,Int] {
        def isDefinedAt(x: Int): Boolean = true
        def apply(x: Int) = throw new UserException
      }
      intercept[UserException] {
        binder(x).transformIfDefined(pfThrowLate)
      }
      assert(binder(x).get === 1)
      binder(x) := 2
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
      if (b.mode.isInstanceOf[Txn]) {
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
