/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// UnrecordedRead.scala

package edu.stanford.ppl.ccstm


/** Holds the result of an unrecorded read, which may be used to avoid
 *  transaction conflicts, or to detect ABA changes when performing
 *  single-operation transactions.  `ReleasableRead`s provide a
 *  related functionality.
 *
 *  When an unrecorded read is performed in a transaction, the caller is
 *  responsible for guaranteeing that the transaction's behavior is correct,
 *  even if the read becomes invalid prior to commit.  Unrecorded reads may be
 *  useful for heuristic decisions that can tolerate inconsistent or stale
 *  data, for methods that register transaction handlers to perform
 *  validation at a semantic level, or for optimistically traversing linked
 *  data structures while tolerating mutations to earlier links.  When used in
 *  combination with transaction resource callbacks, it is important to
 *  consider the case that the unrecorded read is already invalid before it is
 *  returned to the requester.
 *
 *  Writes by the same transaction that performed the unrecorded read are
 *  '''not''' considered to invalidate the read.  
 *
 *  When called from a non-transactional context the returned instance can be
 *  used to determine if a value has remained unchanged for a particular
 *  interval, which may be useful to detect ABA situations.
 *
 *  @author Nathan Bronson
 */
trait UnrecordedRead[+T] {

  /** Returns `Some(txn)` if this unrecorded read was made from a
   *  transactional context, or `None` if this unrecorded read was
   *  made from a non-transactional context.
   */
  def context: Option[Txn]

  /** Returns the value observed by this `UnrecordedRead`.  For any
   *  particular `UnrecordedRead` instance this method will always
   *  return the same result.
   *  @return the value observed by this `UnrecordedRead`,
   *      regardless of whether or not it is still valid.
   */
  def value: T

  /** Returns true if definitely no changes have occurred to the data whose
   *  unrecorded read is encapsulated in this `UnrecordedRead`
   *  instance by any transaction other than `context.get` or by any
   *  non-transactional context.  This method may be useful for detecting ABA
   *  problems.  Some STM implementations may occasionally spuriously return
   *  false.  If this unrecorded read occurred in a transactional context and
   *  that transaction has since performed writes to the location, then it is
   *  possible that `value` differs from the current data value
   *  while the unrecorded read remains valid.
   *  @return true if the unrecorded read has definitely remained valid from
   *      its original creation until this method invocation, false otherwise.  
   */
  def stillValid: Boolean

  /** Returns true if the unrecorded read was performed in a transaction, and
   *  the source of this read is known to be in the transaction's read or write
   *  set, in which case `nonTxnStillValid` will definitely be true at the
   *  transaction's commit (linearization) point if the transaction commits.
   *  Returns false for non-transactional unrecorded reads.  This method is for
   *  optimization purposes only, a false result does not guarantee that the
   *  read is not in the transaction's read or write set.  If this method
   *  returns true for an `UnrecordedRead` instance it will always
   *  return true.
   *  @return true if the caller may assume that `nonTxnStillValid` will
   *      be true for this `UnrecordedRead` if the bound transaction
   *      is successfully committed.
   */
  def recorded: Boolean
}

