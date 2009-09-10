/* UnrecordedRead
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package edu.stanford.ppl.ccstm

/** Holds the result of an unrecorded read, which may be used to avoid
 *  transaction conflicts, or to detect ABA changes when performing
 *  non-transactional accesses.
 *  <p>
 *  When an unrecorded read is performed in a transaction, the caller is
 *  responsible for guaranteeing that the transaction's behavior is correct,
 *  even if the read becomes invalid prior to commit.  Unrecorded reads may be
 *  useful for heuristic decisions that can tolerate inconsistent or stale
 *  data, or for methods that register transaction handlers to perform
 *  validation at a semantic level.  When used in combination with transaction
 *  resource callbacks, it is important to consider the case that the
 *  unrecorded read is already invalid before it is returned to the requester.
 *  <p>
 *  When called from a non-transactional context the returned instance can be
 *  used to determine if a value has remained unchanged for a particular
 *  interval, which may be useful to detect ABA situations.
 *  <p>
 *  Some STM implementations may spuriously indicate that an unrecorded read
 *  has become invalid, despite no change actually occurring to the original
 *  value.
 *  @see edu.stanford.ppl.ccstm.TVar.BoundSource#unrecordedRead
 */
trait UnrecordedRead[+T] {
  /** Returns <code>Some(txn)</code> if this unrecorded read was made from a
   *  transactional context, or <code>None</code> if this unrecorded read was
   *  made from a non-transactional context.
   */
  def context: Option[Txn]

  /** Returns the value observed by this <code>UnrecordedRead</code>.  For any
   *  particular <code>UnrecordedRead</code> instance this method will always
   *  return the same result.
   *  @return the value observed by this <code>UnrecordedRead</code>,
   *      regardless of whether or not it is still valid.
   */
  def value: T

  /** Returns true if definitely no changes have occurred to the data whose
   *  unrecorded read is encapsulated in this <code>UnrecordedRead</code>
   *  instance by any transaction other than <code>context.get</code> or by any
   *  non-transactional context.  This method may be useful for detecting ABA
   *  problems.  Some STM implementations may occasionally spuriously return
   *  false.  If this unrecorded read occurred in a transactional context and
   *  that transaction has since performed writes to the location, then it is
   *  possible that <code>value</code> differs from the current data value
   *  while the unrecorded read remains valid.
   *  @return true if the unrecorded read has definitely remained valid from
   *      its original creation until this method invocation, false otherwise.  
   */
  def stillValid: Boolean

  /** Returns true if the unrecorded read was performed in a transaction, and
   *  the source of this read is known to be in the transaction's read or write
   *  set, in which case <code>stillValid</code> will definitely be true at the
   *  transaction's commit (linearization) point if the transaction commits.
   *  Returns false for non-transactional unrecorded reads.  This method is for
   *  optimization purposes only, a false result does not guarantee that the
   *  read is not in the transaction's read or write set.  If this method
   *  returns true for an <code>UnrecordedRead</code> instance it will always
   *  return true.
   *  @return true if the caller may assume that <code>stillValid</code> will
   *      be true for this <code>UnrecordedRead</code> if the bound transaction
   *      is successfully committed.
   */
  def recorded: Boolean
}

