/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// MaybeTxnSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl._

class MaybeTxnSuite extends STMFunSuite {
  test("TxnUnknown is found") {
    assert(context eq TxnUnknown)
  }

  test("Txn is found") {
    STM.atomic { t0 =>
      implicit val t = t0
      assert(context eq t)
    }
  }

  private def context(implicit mt: MaybeTxn) = mt

//  test("Inline nesting") {
//    val x = Ref(10)
//    STM.atomic { implicit t =>
//      assert(x() === 10)
//      x := 11
//      STM.atomic { implicit t =>
//        assert(x() === 11)
//        x := 12
//        STM.atomic { implicit t =>
//          assert(x() == 12)
//          x := 13
//        }
//        assert(x() === 13)
//      }
//      assert(x() === 13)
//    }
//    assert(x.nonTxn() === 13)
//  }

  test("call with txn") {
    val x = Ref(10)
    //STM.atomic { implicit t => // this is syntactically valid, but confuses IDEA
    STM.atomic { t0 =>
      implicit val t = t0
      assert(t === fooWithTxn)
      assert(t === fooWithMaybeTxn)
      assert(goesBothWaysStatic(x) === 10)
      assert(goesBothWaysStaticGeneric(x) === 10)
      assert(goesBothWaysDynamicRecursive(x) === 10)
      assert(goesBothWaysDynamicDelegating(x) === 10)
      assert(goesBothWaysDynamicGeneric(x) === 10)
    }
  }

  test("call without txn") {
    val x = Ref(10)
    // fooWithTxn -- compiler error
    assert(TxnUnknown === fooWithMaybeTxn)
    assert(goesBothWaysStatic(x) === 10)
    assert(goesBothWaysStaticGeneric(x) === 10)
    assert(goesBothWaysDynamicRecursive(x) === 10)
    assert(goesBothWaysDynamicDelegating(x) === 10)
    assert(goesBothWaysDynamicGeneric(x) === 10)
  }

  if (false) test("perf", ExhaustiveTest) {
    val numTxns = 1000
    val opsPerTxn = 10000
    for (p <- 0 until 10) {
      for ((which,name) <- List((0,"static"), (1,"static-generic"), (2,"dyn-recursive"), (3,"dyn-delegating"), (4,"dyn-generic"))) {
        val x = Ref(10)
        val t0 = System.currentTimeMillis
        var txnRemaining = numTxns
        while (txnRemaining > 0) {
          txnRemaining -= 1
          //STM.atomic { implicit bar =>
          STM.atomic { foo =>
            implicit val bar = foo
            x += 1
            var opRemaining = opsPerTxn
            while (opRemaining > 0) {
              opRemaining -= 1
              which match {
                case 0 => goesBothWaysStatic(x)
                case 1 => goesBothWaysStaticGeneric(x)
                case 2 => goesBothWaysDynamicRecursive(x)
                case 3 => goesBothWaysDynamicDelegating(x)
                case 4 => goesBothWaysDynamicGeneric(x)
              }
            }
          }
        }
        val elapsed = System.currentTimeMillis - t0
        println(name + ": " + numTxns + "*" + opsPerTxn + " calls in " + elapsed + " millis")
      }
    }
  }

  def fooWithTxn(implicit txn: Txn) = txn

  def fooWithMaybeTxn(implicit ctx: MaybeTxn) = ctx

  private def work(x: IntRef)(implicit txn: Txn) = {
    x.get
  }

  def goesBothWaysStatic(x: IntRef)(implicit ctx: MaybeTxn): Int = {
    implicit val t: Txn = ctx match {
      case TxnUnknown => Txn.currentOrNull(TxnUnknown)
      case x: Txn => x
    }
    if (null == t) {
      STM.atomic(goesBothWaysStatic(x)(_))
    } else {
      work(x)
    }
  }

  def atomicOrCurrentStatic[Z](ctx: MaybeTxn)(block: Txn => Z): Z = {
    val t: Txn = ctx match {
      case TxnUnknown => Txn.currentOrNull
      case x: Txn => x
    }
    if (null == t) {
      STM.atomic(block)
    } else {
      block(t)
    }
  }

  def goesBothWaysStaticGeneric(x: IntRef)(implicit ctx: MaybeTxn): Int = atomicOrCurrentStatic(ctx) { t =>
    implicit val t2 = t
    work(x)
  }

  def goesBothWaysDynamicRecursive(x: IntRef): Int = {
    implicit val t = Txn.currentOrNull
    if (null == t) {
      STM.atomic(_ => goesBothWaysDynamicRecursive(x))
    } else {
      work(x)
    }
  }

  def goesBothWaysDynamicDelegating(x: IntRef) = {
    implicit val t = Txn.currentOrNull
    if (null == t) {
      STM.atomic(dynDelegatingImpl(x)(_))
    } else {
      dynDelegatingImpl(x)(t)
    }
  }

  private def dynDelegatingImpl(x: IntRef)(implicit t: Txn) = {
    work(x)
  }

  def atomicOrCurrentDynamic[Z](block: Txn => Z): Z = {
    implicit val t = Txn.currentOrNull
    if (null == t) {
      STM.atomic(block)
    } else {
      block(t)
    }
  }

  def goesBothWaysDynamicGeneric(x: IntRef): Int = atomicOrCurrentDynamic { t =>
    implicit val t2 = t
    work(x)
  }
}
