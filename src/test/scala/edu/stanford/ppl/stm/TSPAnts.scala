/* CCSTM - (c) 2009 Stanford University - PPL */

// TSPAnts

package edu.stanford.ppl.stm

import scala.actors.Actor
import scala.actors.Actor._
import java.util.concurrent.atomic.AtomicLong
import edu.stanford.ppl.ccstm._


// Notes: nbronson 22 Sep 2009
//
// This is a direct port of the Clojure traveling salesman problem
// ant system.  It does not turn out to stress the STM enough to be
// useful as a CCSTM benchmark, rather it is an intensive test of the
// persistent set and persistent map implementations.  The original
// Clojure implementation and the Scala implementation differ by only
// a small factor in their performance (a non-rigorous 20% speedup for
// Scala) when using Clojure's PersistentHash{Set,Map} (via a wrapper
// in Scala).  Switching to immutable.Tree{Set,Map} is good for about
// another 30% improvement.
//
// The largest gain, by far, however, comes from representing edges using
// something other than a Set.  Switching to a Tuple2 was more than 6
// times faster than immutable.Set, and constructing a custom Edge class
// (that doesn't need to box its elements and doesn't need to perform
// implicit conversions during ordering) yields results that are more
// than 20 times faster than the baseline Scala implementation.
//
// I did not benchmark the default Scala persistent set and map
// implementations (immutable.HashSet and immutable.HashMap for
// collections with more than 4 elements) due to bugs in their
// implementation.


object TSPAnts {
  import TSPData._

  // this impl gives ~ 2.5 tours/sec on my laptop (Scala TreeMap, 2 ants)
//  val Edge = Set // Factory
//  type Edge = Set[City]

  // this impl gives ~ 16 tours/sec on my laptop  (Scala TreeMap, 2 ants)
//  def Edge(a: City, b: City) = (a min b, a max b)
//  type Edge = (City,City)
//  implicit def edgeToList(e: Edge) = List(e._1, e._2)

  // this impl gives > 50 tours/sec on my laptop  (Scala TreeMap, 2 ants)
  def Edge(a: City, b: City) = new Edge(a min b, a max b)
  class Edge(val from: City, val to: City) extends Ordered[Edge] {
    override def equals(rhs: Any) = rhs match {
      case x: Edge => compare(x) == 0
      case _ => false
    }
    override def hashCode = from * 1001 + to
    def compare(rhs: Edge) = if (from != rhs.from) from - rhs.from else to - rhs.to
    def toSeq = List(from, to)
  }

  def distance(coords: City => Pt, edge: Edge) = {
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
    tour.zip(tour.tail).map(p => distance(coords, Edge(p._1,p._2))).reduceLeft(_+_)
  }

  val optimalDistance = tourLength(optimalTour)

  val nodes = Set.empty[City] ++ coords.keys
  val edges = Set.empty[Edge] ++ (for (a <- nodes.toList; b <- nodes.toList; if a != b) yield Edge(a, b))
  val distances = Map.empty[Edge,Int] ++ (for (e <- edges.toList) yield (e -> distance(coords, e)))

  val pheromones = Map.empty[Edge,Ref[Double]] ++ (for (e <- edges.toList) yield (e -> Ref(InitP)))

  def prob(edge: Edge)(implicit txn: Txn) = {
    Math.pow(!pheromones(edge), PFactor) * Math.pow(1.0 / distances(edge), DFactor)
  }

  val probs = Map.empty[Edge,Ref[Double]] ++ (for (e <- edges.toList) yield (e -> Ref(STM.atomic(prob(e)(_)))))

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

  def getProb(from: City, to: City) = probs(Edge(from, to)).nonTxn.get

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
        loop(next, node :: path, (togo - next).asInstanceOf[Set[City]]) // missing - override in TreeSet
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
        val edge = Edge(p._1, p._2)
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
