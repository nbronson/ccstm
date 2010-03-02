/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// UnrecordedRead.scala

package edu.stanford.ppl.ccstm


/** Holds the result of a releasable read, which may be discarded from a
 *  transaction's read set prior to the transaction's commit.
 *  <code>UnrecordedRead</code>s provide a related functionality.
 *  @see edu.stanford.ppl.ccstm.Source.Bound#releasableRead
 *  @see edu.stanford.ppl.ccstm.Source.Bound#unrecordedRead
 *
 *  @author Nathan Bronson
 */
trait ReleasableRead[+T] {

  /** Returns <code>Some(txn)</code> if this releasable read was made from a
   *  transactional context, or <code>None</code> if this releasable read was
   *  made from a non-transactional context.
   */
  def context: Option[Txn]

  /** Returns the value observed by this <code>ReleasableRead</code>.  For any
   *  particular <code>ReleasableRead</code> instance this method will always
   *  return the same result.
   *  @return the value observed by this <code>ReleasableRead</code>,
   *      regardless of whether or not it is still valid.
   */
  def value: T

  /** If <code>context</code> is <code>Some(txn)</code>, removes this read from
   *  <code>txn</code>'s read set.  If no other (unreleased or unrecorded)
   *  reads of this location were performed by the transaction, then the
   *  transaction might commit despite a conflicting change to this location.
   *  This method removes only this read from the read set, not all reads to
   *  the read location.  Does nothing if this releasable read was performed in
   *  a non-transactional context, or if this method was already called on this
   *  instance. 
   */
  def release()
}
