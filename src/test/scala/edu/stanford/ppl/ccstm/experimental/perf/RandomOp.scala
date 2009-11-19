package edu.stanford.ppl.ccstm.experimental.perf

/* RandomOp
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm._

object RandomOp {
  val LeadingAdds = System.getProperty("leading-adds", "0").toInt
  val AddPct = System.getProperty("add-pct", "20").toInt
  val RemovePct = System.getProperty("remove-pct", "10").toInt
  val GetPct = 100 - AddPct - RemovePct
  val PowerLawKeys = "1tTyY".indexOf((System.getProperty("power-law", "") + "f").charAt(0)) >= 0
  val TxnSize = System.getProperty("txn-size", "2").toInt
  val TxnOpPct = System.getProperty("txn-op-pct", "0").toInt
  val TxnNoReadOnlyCommit = "1tTyY".indexOf((System.getProperty("txn-no-ro-commit", "") + "f").charAt(0)) >= 0

  println("RandomOp.LeadingAdds = " + LeadingAdds)
  println("RandomOp.AddPct = " + AddPct)
  println("RandomOp.RemovePct = " + RemovePct)
  println("RandomOp.GetPct = " + GetPct)
  println("RandomOp.PowerLawKeys = " + PowerLawKeys)
  println("RandomOp.TxnSize = " + TxnSize)
  println("RandomOp.TxnOpPct = " + TxnOpPct)
  println("RandomOp.TxnNoReadOnlyCommit = " + TxnNoReadOnlyCommit)

  val Values = Array.fromFunction(i => "x"+i)(1024)
}

private object DummyWriteResource extends Txn.WriteResource {
  def prepare(txn: Txn): Boolean = true
  def performCommit(txn: Txn) {}
  def performRollback(txn: Txn) {}
}

class RandomOp extends Perf.Worker {
  import RandomOp._

  { RandomOp.AddPct }

  var rng: scala.util.Random = null

  override def setup(id: Int, size: Int, numThreads: Int, targetType: String, master: Perf.Worker) {
    super.setup(id, size, numThreads, targetType, master)
    rng = new scala.util.Random(id)
  }

  private def nextKey = {
    if (PowerLawKeys) (Math.pow(rng.nextDouble, 7.2126) * size).toInt else rng.nextInt(size)
  }

  def run(warmup: Boolean, pass: Int) {
    var i = 0
    while (i < 1000000) {
      if (TxnOpPct > 0 && (TxnOpPct == 100 || rng.nextInt(100 * TxnSize) < TxnOpPct)) {
        val data = new Array[Int](TxnSize * 3)
        for (j <- 0 until TxnSize) {
          data(3*j+0) = rng.nextInt(100)
          data(3*j+1) = nextKey
          data(3*j+2) = rng.nextInt(Values.length)
        }

        var a = 0
        new Atomic { def body {
          if (TxnNoReadOnlyCommit) currentTxn.addWriteResource(DummyWriteResource)

          a += 1
          if (a == 100) println("livelock")
          var j = 0
          while (j < TxnSize) {
            val r = data(3*j+0)
            val k = data(3*j+1)
            val v = Values(data(3*j+2))
            if (r < AddPct || i < LeadingAdds) {
              doTxnPut(k, v)
            } else if (r < AddPct + RemovePct) {
              doTxnRemove(k)
            } else {
              doTxnGet(k)
            }
            j += 1
          }
        }}.run
        i += TxnSize
      } else {
        val r = rng.nextInt(100)
        val k = nextKey
        val v = Values(rng.nextInt(Values.length))
        if (r < AddPct || i < LeadingAdds) {
          doPut(k, v)
        } else if (r < AddPct + RemovePct) {
          doRemove(k)
        } else {
          doGet(k)
        }
        i += 1
      }
    }
  }
}
