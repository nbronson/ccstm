/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// UnrecordedRead.scala

package edu.stanford.ppl.ccstm


/** Holds the result of a releasable read, which may be discarded from a
 *  transaction's read set prior to the transaction's commit.
 *  <code>UnrecordedRead</code>s provide a related functionality.
 *  @see edu.stanford.ppl.ccstm.Source.View#releasableRead
 *  @see edu.stanford.ppl.ccstm.Source.View#unrecordedRead
 *
 *  @author Nathan Bronson
 */
trait ReleasableRead[+T] {

  /** Returns `Some(txn)` if this releasable read was made from a transactional
   *  context, or `None` if this releasable read was made as part of a
   *  single-operation transaction that was not nested in an explicit atomic
   *  block.
   */
  def context: Option[Txn]

  /** Returns the value observed by this `ReleasableRead`.  For any particular
   *  `ReleasableRead` instance this method will always return the same result.
   *  @return the value observed by this `ReleasableRead`, regardless of
   *      whether it is still valid.
   */
  def value: T

  /** If `context` is `Some(txn)`, removes this read from `txn`'s read set.
   *  If no other (unreleased or unrecorded) reads of this location were
   *  performed by the transaction, then the transaction might commit despite a
   *  conflicting change to this location.  This method removes only this read
   *  from the read set, not other reads to the read location.  Does nothing if
   *  this method was already called on this instance.
   */
  def release()
}
