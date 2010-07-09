package edu.stanford.ppl.ccstm

import edu.stanford.ppl.ccstm.impl._
import java.util.IdentityHashMap
import edu.stanford.ppl.ExhaustiveTest
import edu.stanford.ppl.stm.STMFunSuite


/** Performs single-threaded tests of `Ref`.  Since there is no
 *  outside interference, the tests cover non-transactional and transactional
 *  accesses, with various types and levels of bound view reuse.   
 */
class IsolatedRefSuite extends STMFunSuite {

  /** Binder implementations provide a way of obtaining Ref.View instances
   *  from a Ref.  The bound views may be reused, or not, and they may be
   *  non-transactional or transactional.
   */
  class Binder(reuse: Boolean, txnLen: Int, single: Boolean) {
    private val cache = new IdentityHashMap[Ref[_],Ref.View[_]]
    private var txn: Txn = null
    private var accesses = 0

    def apply[T](v: Ref[T]): Ref.View[T] = {
      if (txnLen > 0) {
        if (accesses == txnLen) reset()
        accesses += 1
        if (null == txn) txn = new Txn
      }
      if (reuse) {
        val z = cache.get(v)
        if (null != z) return z.asInstanceOf[Ref.View[T]]
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
    def apply(initialValue: Int): Ref[Int] = Ref(initialValue)
  }

  case object LazyConflictRefFactory extends IntRefFactory {
    def apply(initialValue: Int): Ref[Int] = Ref.lazyConflict(initialValue)
  }

  case object StripedIntRefFactory extends IntRefFactory {
    def apply(initialValue: Int): Ref[Int] = Ref.striped(initialValue)
    override def incrLimitFactor = 3
  }


  val binders = List(
    FreshEscaped, FreshSingleNonTxn, FreshSingleTxn(1), FreshTxn(1),
    FreshSingleTxn(2), FreshTxn(2), FreshSingleTxn(8), FreshTxn(8),
    FreshSingleTxn(1000), FreshTxn(1000), ReuseEscaped, ReuseSingleNonTxn,
    ReuseSingleTxn(1), ReuseTxn(1), ReuseSingleTxn(2), ReuseTxn(2),
    ReuseSingleTxn(8), ReuseTxn(8), ReuseSingleTxn(1000), ReuseTxn(1000))
  val factories = List(TAnyRefFactory, TIntRefFactory, LazyConflictRefFactory, StripedIntRefFactory)
  for (f <- factories; b <- binders) createTests(b, f)

  private def createTests(binder: Binder, fact: IntRefFactory) {
    test(fact + ": " + binder + ": counter") {
      val x = fact(1)
      val r = binder(x).get + 1
      binder(x)() = r
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
        binder(x)() = v + 1
      }
    }

    runIncrTest(fact + ": " + binder + ": x+=1") { x =>
      var i = 0
      while (i < 10) {
        i += 1
        val b = binder(x)
        b += 1
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
  
    test(fact + ": " + binder + ": getWith") {
      val x = fact(1)
      assert(binder(x).getWith(_ * 10) === 10)
      binder.reset()
    }

    test(fact + ": " + binder + ": getWith + write") {
      val x = fact(1)
      binder(x)() = 2
      assert(binder(x).getWith(_ * 10) === 20)
      binder.reset()
    }
  
    test(fact + ": " + binder + ": write + getWith") {
      val x = fact(1)
      assert(binder(x).getWith(_ * 10) === 10)
      binder(x)() = 2
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
      binder(x)() = 2
      assert(binder(x).get === 2)
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
      for (i <- 0 until 1001) binder(x)() = 2
      for (i <- 0 until 1001) binder(x)() = 1
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
        b() = 2
        val u2 = b.unrecordedRead
        b() = 3
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
      binder(x)() = 2
      val r2 = binder(x).releasableRead
      r1.release()
      r2.release()
      assert(r1.value === 1)
      assert(r2.value === 2)
      r1.release()
      r2.release()
      binder.reset()
    }

    test(fact + ": " + binder + ": trySet") {
      val x = fact(1)
      assert(binder(x).get === 1)
      while (!binder(x).trySet(2)) {
        assert(binder(x).get === 1)
      }
      assert(binder(x).get === 2)
      binder.reset()
    }

    test(fact + ": " + binder + ": +=") {
      val x = fact(1)
      binder(x) += 2
      assert(binder(x).get === 3)
      binder.reset()
    }

    test(fact + ": " + binder + ": -=") {
      val x = fact(1)
      binder(x) -= 3
      assert(binder(x).get === -2)
      binder.reset()
    }

    test(fact + ": " + binder + ": *=") {
      val x = fact(2)
      binder(x) *= -3
      assert(binder(x).get === -6)
      binder.reset()
    }

    test(fact + ": " + binder + ": /=") {
      val x = fact(11)
      binder(x) /= 2
      assert(binder(x).get === 5)
      binder.reset()
    }
  }

  for (binder <- binders) {
    for (fact <- List({ (v: Double) => new TDoubleRef(v) },
                      { (v: Double) => new TAnyRef[Double](v) })) {
      createDivTest(binder, fact)
    }
  }

  private def createDivTest(binder: Binder, fact: Double => Ref[Double]) {
    test(fact(0.0).getClass.getSimpleName + ": " + binder + ": /=") {
      val x = fact(11.0)
      binder(x) /= 2
      assert(binder(x).get === 5.5)
      binder.reset()
    }
  }
}
