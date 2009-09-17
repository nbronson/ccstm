/* CCSTM - (c) 2009 Stanford University - PPL */

// TMap.scala

package edu.stanford.ppl.ccstm


class TMap {

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

}
