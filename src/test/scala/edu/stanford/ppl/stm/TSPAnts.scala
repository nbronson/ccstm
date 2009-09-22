/* CCSTM - (c) 2009 Stanford University - PPL */

// TSPAnts

package edu.stanford.ppl.stm


import edu.stanford.ppl.ccstm._

class TSPAnts {
  import TSPData._

  def distance(coords: City => Pt, edge: (City,City)) = {
    val a = coords(edge._1)
    val b = coords(edge._2)
    val dx = a.x - b.y
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
    tour.zip(tour.tail).map(distance(coords,_)).reduceLeft(_+_)
  }

  val optimalDistance = tourLength(optimalTour)

  val nodes = coords.keySet
  val edges = Set(for (a <- nodes; b <- nodes - a) yield (a,b))
  val distances = Map(for (e <- edges) yield (e -> distance(coords, e)))

  // TODO: is this right?
  val pheromones = Map(for (e <- edges) yield (e -> Ref(InitP)))

  def prob(edge: (City,City))(implicit txn: Txn) = {
    // TODO: prob doesn't seem to be called from a non-txn context
    Math.pow(pheromones(edge).get, PFactor) * Math.pow(1.0 / distances(edge), DFactor)
  }
}