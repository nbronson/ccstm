/* CCSTM - (c) 2009 Stanford University - PPL */

// TSPAnts

package edu.stanford.ppl.stm

import scala.actors.Actor
import scala.actors.Actor._
import java.util.concurrent.atomic.AtomicLong
import edu.stanford.ppl.ccstm._


object TSPAnts {
  import TSPData._

  def distance(coords: City => Pt, edge: Set[City]) = {
    val a = coords(edge.toSeq(0))
    val b = coords(edge.toSeq(1))
    val dx = a.x - b.x
    val dy = a.y - b.y
    val d2 = dx*dx + dy*dy
    Math.sqrt(d2.toDouble).ceil.toInt
  }

  val Q = 100.0
  val InitP = 0.5
  val PFactor = 1.0
  val DFactor = 5.0
  val EFactor = 0.5

  def tourLength(tour: List[City]) = {
    tour.zip(tour.tail).map(p => distance(coords,Set(p._1,p._2))).reduceLeft(_+_)
  }

  val optimalDistance = tourLength(optimalTour)

  val nodes = Set.empty[Int] ++ coords.keys
  val edges = Set.empty[Set[Int]] ++ (for (a <- nodes.toList; b <- nodes.toList; if a != b) yield Set(a, b))
  val distances = Map.empty[Set[Int],Int] ++ (for (e <- edges.toList) yield (e -> distance(coords, e)))

  val pheromones = Map.empty[Set[Int],Ref[Double]] ++ (for (e <- edges.toList) yield (e -> Ref(InitP)))

  def prob(edge: Set[City])(implicit txn: Txn) = {
    Math.pow(!pheromones(edge), PFactor) * Math.pow(1.0 / distances(edge), DFactor)
  }

  val probs = Map.empty[Set[Int],Ref[Double]] ++ (for (e <- edges.toList) yield (e -> Ref(STM.atomic(prob(e)(_)))))

  val bestLength = Ref(Math.MAX_INT)
  val bestTour = Ref(List(Math.MAX_INT))
  val tourCount = new AtomicLong

  val ants = Ref(Array[Thread]())
  @volatile var running = true

  val evaporator = new Actor {
    val state = Ref(0)
    start()
    def act {
      while (running) receive { case f => state.nonTxn.transform(f.asInstanceOf[(Int=>Int)]) }
    }
  }

  def tickAction(cnt: Int) = {
    val newCnt = cnt + 1
    if ((newCnt % ants.nonTxn.get.length) == 0) {
      for (p <- pheromones.values) {
        p.nonTxn.transform(_ * EFactor)
      }
    }
    newCnt
  }

  val rand = new scala.util.Random

  /** Given an array of slice sizes, returns the index of a slice given a
   *  random spin of a roulette wheel with compartments proportional to slices.
   */
  def wrand(slices: Array[Double]) = {
    val total = slices.reduceLeft(_+_)
    val r = rand.nextDouble * total
    def loop(i: Int, sum: Double): Int = {
      val newsum = slices(i) + sum
      if (r < newsum) i else loop(i + 1, newsum)
    }
    loop(0, 0.0)
  }

  def getProb(from: City, to: City) = probs(Set(from, to)).nonTxn.get

  def nextStop(node: City, togo: Set[City]) = {
    val vec = togo.toArray
    vec(wrand(for (t <- vec) yield getProb(node, t)))
  }

  def tour = {
    val home = rand.nextInt(nodes.size)

    def loop(node: City, path: List[City], togo: Set[City]): List[City] = {
      if (togo.isEmpty) {
        node :: path
      } else {
        val next = nextStop(node, togo)
        loop(next, node :: path, (togo - next).asInstanceOf[Set[City]])
      }
    }
    loop(home, Nil, (nodes - home).asInstanceOf[Set[City]])
  }

  def brag(len: Int, tour: List[City]) {
    println("new best, distance:" + len)
    println(tour)
  }

  def tourLoop {
    while (running) {
      val t = tour
      val len = tourLength(t)
      // drop pheromones, recalc edge probs
      for (p <- t.zip(t.tail)) {
        val edge = Set(p._1, p._2)
        new Atomic { def body {
          pheromones(edge).transform(_ + Q / len)
          probs(edge) := prob(edge)
        }}.run
      }
      // are we the new best?
      if (len < bestLength.nonTxn.get) {
        new Atomic { def body {
          if (len < !bestLength) {
            bestLength := len
            bestTour := t
          }
        }}.run
        brag(len, t)
      }
      // counters, evap
      tourCount.incrementAndGet
      evaporator ! tickAction(_: Int)
    }
  }

  def run(nants: Int) = {
    ants.nonTxn := Array.fromFunction(i => new Thread {
      override def run { tourLoop }
    })(nants)
    for (ant <- ants.nonTxn.get) ant.start
    'running
  }

  def runLoop(nants: Int) {
    run(nants)
    println("Running...")
    val start = System.currentTimeMillis
    while (running) {
      Thread.sleep(4000)
      val secs = (System.currentTimeMillis - start) / 1000.0
      printf("Running %d nodes, %d ants, %d tours, %4.3f seconds, %3.2f per second, best-so-far: %d optimal: %d\n",
             nodes.size, nants, tourCount.get, secs, tourCount.get / secs, bestLength.nonTxn.get, optimalDistance)
      // (dorun (map deref @ants))
    }
  }

  def main(args: Array[String]) {
    runLoop(if (args.isEmpty) 10 else args(0).toInt)
  }
}