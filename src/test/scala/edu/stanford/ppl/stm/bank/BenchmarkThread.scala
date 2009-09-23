/* CCSTM - (c) 2009 Stanford University - PPL */

// BenchmarkThread

package edu.stanford.ppl.stm.bank


import ccstm.STM

object BenchmarkThread {
  sealed abstract class Mode
  case object Warmup extends Mode
  case object Timed extends Mode
  case object Shutdown extends Mode
}

class BenchmarkThread(id: Int,
                      threads: Int,
                      accounts: Array[Account],
                      maxAmount: Int,
                      readFreq: Int,
                      writeFreq: Int,
                      readThreads: Int,
                      writeThreads: Int,
                      disjointTransfers: Boolean) extends Thread("BenchmarkThread(" + id + ")") {
  import BenchmarkThread._

  var numTransfers = 0
  var numReads = 0
  var numWrites = 0
  private val rand = new scala.util.Random

  @volatile var mode: Mode = Warmup

  override def run {
    while (mode != Shutdown) {
      step(mode == Timed)
    }
  }

  def step(record: Boolean) {
    val (readCutoff, writeCutoff, pct) = if (id < readThreads) {
      (100, 100, 50)
    } else if (id < writeThreads) {
      (0, 100, 50)
    } else {
      (readFreq, readFreq + writeFreq, rand.nextInt(100))
    }

    if (pct < readCutoff) {
      // Compute total of all accounts (read-all transaction)
      STM.atomic(Account.computeTotal(accounts)(_))
      if (record) numReads += 1
    }
    else if (pct < writeCutoff ) {
      // Add 0% interest (write-all transactions)
      STM.atomic(Account.addInterest(accounts, 0)(_))
      if (record) numWrites += 1
    }
    else {
      val amount = rand.nextInt(maxAmount) + 1
      val (range, mult, base) = if (disjointTransfers) {
        assert(accounts.length >= threads)
        // This distribution mechanism follows the original benchmark.  It
        // performs only self transfers when (accounts.length < threads*2),
        // and (accounts.length % threads) accounts never participate in
        // transfers.
        (accounts.length / threads, threads, id)
      } else {
        (accounts.length, 1, 0)
      }
      val src = rand.nextInt(range) * mult + base
      val dst = rand.nextInt(range) * mult + base

      try {
        STM.atomic(Account.transfer(accounts(src), accounts(dst), amount)(_))
        if (record) numTransfers += 1
      } catch {
        case x: OverdraftException => Console.err.println("Overdraft: " + x.getMessage)
      }
    }
  }

  override def toString = ("T=" + numTransfers + ", R=" + numReads + ", W=" + numWrites)
}