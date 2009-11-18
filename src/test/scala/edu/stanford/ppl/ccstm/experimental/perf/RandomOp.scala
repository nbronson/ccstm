package edu.stanford.ppl.ccstm.experimental.perf

/* RandomOp
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

import scala.collection.mutable.Map
import edu.stanford.ppl.ccstm.experimental.TMap

object RandomOp {
  val LeadingAdds = System.getProperty("leading-adds", "0").toInt
  val AddPct = System.getProperty("add-pct", "20").toInt
  val RemovePct = System.getProperty("remove-pct", "10").toInt
  val GetPct = 100 - AddPct - RemovePct

  println("RandomOp.LeadingAdds = " + LeadingAdds)
  println("RandomOp.AddPct = " + AddPct)
  println("RandomOp.RemovePct = " + RemovePct)
  println("RandomOp.GetPct = " + GetPct)
}

class RandomOp extends Perf.Worker {
  import RandomOp._

  { RandomOp.AddPct }

  var rng: scala.util.Random = null

  override def setup(id: Int, size: Int, numThreads: Int, target: TMap[Int,String], master: Perf.Worker) {
    super.setup(id, size, numThreads, target, master)
    rng = new scala.util.Random(id)
  }

  def run(warmup: Boolean, pass: Int) {
    for (i <- 0 until 1000000) {
      val r = rng.nextInt(100)
      val k = rng.nextInt(size)
      if (r < AddPct || i < LeadingAdds) {
        doPut(k, "value")
      } else if (r < AddPct + RemovePct) {
        doRemove(k)
      } else {
        doGet(k)
      }
    }
  }
}