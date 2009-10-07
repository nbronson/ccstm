/* CCSTM - (c) 2009 Stanford University - PPL */

// Benchmark

package edu.stanford.ppl.stm.bank


object Benchmark {

  private val argDefs = List(
      ("startup",       "-s", "2000",     "warmup period duration (millis)"),
      ("elapsed",       "-e", "10000",    "benchmark period duration (millis)"),
      ("threads",       "-n", "8",        "total number of threads"),
      ("accounts",      "-a", "8",        "total number of accounts"),
      ("init",          "-i", "10000",    "starting account balance"),
      ("maxAmount",     "-m", "10",       "maximum transfer amount"),
      ("readFreq",      "-r", "0",        "% prob a non-dedicated thread performs a global read"),
      ("writeFreq",     "-w", "0",        "% prob a non-dedicated thread performs a global update"),
      ("readThreads",   "-R", "0",        "number of threads dedicated to global reads"),
      ("writeThreads",  "-W", "0",        "number of threads dedicated to global writes"),
      ("disjointTrans", "-d", "false",    "guarantee transfers don't conflict"),
      ("yieldALot",     "-y", "false",    "add Thread.yield calls to promote races")
    )

  def main(args: Array[String]) {
    val params = parse(args)
    for (a <- argDefs) {
      printf("%-15s %s\n", a._1 + " =", params(a._1))
    }

    Account.yieldALot = params("yieldALot").toBoolean

    val accounts = Array.fromFunction[Account]((i: Int) => {
      new CheckingAccount("" + i, params("init").toFloat)
    })(params("accounts").toInt)
    
    val threads = Array.fromFunction(i => {
      new BenchmarkThread(
          i,
          params("threads").toInt,
          accounts,
          params("maxAmount").toInt,
          params("readFreq").toInt,
          params("writeFreq").toInt,
          params("readThreads").toInt,
          params("writeThreads").toInt,
          params("disjointTrans").toBoolean
        )
    })(params("threads").toInt)

    val warmup = params("startup").toInt
    val timed = params("elapsed").toInt

    //// warmup

    println("\nwarming up")
    for (t <- threads) t.start()
    Thread.sleep(warmup)

    //// timed

    println("timed execution")
    val start = System.currentTimeMillis
    for (t <- threads) t.mode = BenchmarkThread.Timed
    Thread.sleep(timed)

    //// shutdown

    for (t <- threads) t.mode = BenchmarkThread.Shutdown
    val stop = System.currentTimeMillis
    for (t <- threads) t.join

    //// check

    val finalBalances = accounts.map(_.balance.nonTxn.get)
    val sum = finalBalances.reduceLeft(_+_)
    println("\nTOTAL=" + sum + finalBalances.mkString(" [ ", " ", " ]"))

    //// stats

    println("\n" + (stop - start) + " actual elapsed\n")
    var transfers = 0
    var reads = 0
    var writes = 0
    for (t <- threads) {
      transfers += t.numTransfers
      reads += t.numReads
      writes += t.numWrites
      println(t)
    }
    println("\nTOTAL: T=" + transfers + ", R=" + reads + ", W=" + writes)
  }

  //////////////// Argument parsing

  def parse(args: Array[String]): Map[String,String] = {
    var result = Map.empty ++ (for (a <- argDefs) yield (a._1 -> a._3))

    var i = 0
    while (i < args.length) {
      val arg = args(i)
      argDefs.find(_._2 == arg) match {
        case None => {
          if (arg == "-h" || arg == "--help") exitUsage
          Console.err.println("Unknown argument " + arg + ", try -h for help")
          System.exit(1)
        }
        case Some((name,_,default,_)) => {
          if (default == "false") {
            // boolean args don't have a second part
            result += (name -> "true")
          } else {
            result += (name -> args(i + 1))
            i += 1
          }
        }
      }
      i += 1
    }

    result
  }

  def exitUsage {
    println("Usage:\n" +
            "  scala edu.stanford.ppl.stm.bank.Benchmark [arguments]\n" +
            "\n" +
            "Arguments:")
    for (a <- argDefs) {
      val paramName = if (a._3 == "false") "" else a._1
      printf("  %s %-12s   %s (default %s)\n", a._2, paramName, a._4, a._3)
    }
    System.exit(0)
  }
}