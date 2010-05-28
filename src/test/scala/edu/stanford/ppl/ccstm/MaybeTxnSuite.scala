/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// MaybeTxnSuite

package edu.stanford.ppl.ccstm

import edu.stanford.ppl.stm._
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

  test("Static nesting lookup") {
    val x = Ref(10)
    STM.atomic { implicit t =>
      assert(x() === 10)
      x() = 11
      STM.atomic { implicit t =>
        assert(x() === 11)
        x() = 12
        STM.atomic { implicit t =>
          assert(x() === 12)
          x() = 13
        }
        assert(x() === 13)
      }
      assert(x() === 13)
    }
    assert(x.escaped() === 13)
  }

  test("Dynamic nesting lookup") {
    val x = Ref(10)
    val xs = x.single
    def loop(expected: Int) {
      STM.atomic { implicit t =>
        assert(x() === expected)
        assert(xs() === expected)
        x() = expected + 1
        if (expected < 100) loop(expected + 1)
        assert(x() === 101)
      }
    }
    loop(10)
    assert(xs() === 101)
    assert(x.escaped() === 101)
  }

  test("Static lookup overrides dynamic") {
    implicit var t0: Txn = null
    STM.atomic { t =>
      t0 = t
      assert(Txn.current === Some(t))
      assert(Txn.currentOrNull eq t)
      assert(Txn.dynCurrentOrNull eq t)
    }
    assert(t0.status === Txn.Committed)
    assert(Txn.current === Some(t0))
    assert(Txn.currentOrNull eq t0)
    assert(Txn.dynCurrentOrNull eq null)
    STM.atomic { t =>
      assert(t0 eq t)
      assert(t.status === Txn.Committed)
      assert(Txn.current === Some(t))
      assert(Txn.currentOrNull eq t)
      assert(Txn.dynCurrentOrNull eq null)
    }
  }
}
