/* CCSTM - (c) 2009 Stanford University - PPL */

// TMap.scala

package edu.stanford.ppl.ccstm.collection


object TMap {
  trait Bound[A,B] extends scala.collection.mutable.Map[A,B] {
    //def unrecordedGet(k: A): UnrecordedRead[Option[B]]
    //def await(k: A)
    //def await(k: A, pred: (B => Boolean))
    def unbind: TMap[A,B]
    def context: Option[Txn]

    def refs: (A => Ref.Bound[B])
  }

  trait Refs[A,B] {
    def apply(k: A): Ref.Bound[Option[B]]
    def size: Ref.Bound[Int]
  }
}

trait TMap[A,B] {
  def size(implicit txn: Txn): Int = bind.size
  def get(k: A)(implicit txn: Txn): Option[B] = bind.get(k)
  def put(k: A, v: B)(implicit txn: Txn): Option[B] = bind.put(k, v)
  def removeKey(k: A)(implicit txn: Txn): Option[B] = bind.removeKey(k)
  def transform(f: (A,B) => B)(implicit txn: Txn) = bind.transform(f)

  def refs(implicit txn: Txn): (A => Ref[B])

  def bind(implicit txn: Txn): TMap.Bound[A,B]
  def nonTxn: TMap.Bound[A,B]
}

//class TMap {
//
// The general implementation strategy for TMap is to start with a
// concurrent map holding Ref[Option[V]], where each Ref is created
// with an initial value of None.  Map entries holding Ref-s that are
// completely unlocked and that contain None may (must?) be removed.
// Functions such as get, first, and succ are modified to skip None
// entries.  To transactionally put an element in the map, insert a new
// Ref(None) if no mapping is present, then transactionally set the
// Ref to Some(v).  To remove, transactionally set the Ref to None,
// then after commit the entry may actually be removed.
//
// Size is implemented with striped counters.  Note that the actual
// insertion and removal operations on the map do not change its actual
// size, all size changes are the result of Ref updates.  Updates to
// the stripes are handled using transform() to increase commit rates.
// Some sort of automatic splitting/joining of the stripes is desirable
// to adapt to the actual contention levels.  Reads may have to fall
// back to pessimistic conflict management to succeed.
//
// Clone is tricky, because it may result in copies of the Ref-s,
// and it may result in changes to the state of the tree independent of
// its structure.  Cloning within a txn and then using that clone later
// in the txn is especially tricky.  Hmm...
//
//}
