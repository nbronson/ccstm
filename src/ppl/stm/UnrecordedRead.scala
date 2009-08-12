/* UnrecordedRead
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm

/** Holds the result of an unrecorded read, which may be used to avoid
 *  transaction conflicts, or to detect ABA changes when performing
 *  non-transactional accesses.
 *  <p>
 *  When an unrecorded read is performed in a transaction (see for example
 *  {@link ppl.stm.TVar.BoundSource#unrecordedRead
 *  TVar.BoundSource.unrecordedRead}) the caller is responsible for
 *  guaranteeing that the transaction's behavior is correct, even if the read
 *  becomes invalid prior to commit.  Unrecorded reads may be useful for
 *  heuristic decisions that can tolerate inconsistent or stale data, or for
 *  methods that register transaction handlers to perform validation at a
 *  semantic level.  When used in combination with transaction resource
 *  callbacks, it is important to consider the case that the unrecorded read is
 *  already invalid before it is returned to the requester.
 *  <p>
 *  When called from a non-transactional context the returned instance can be
 *  used to determine if a value has remained unchanged for a particular
 *  interval, which may be useful to detect ABA situations.
 *  <p>
 *  Some STM implementations may spuriously indicate that an unrecorded read
 *  has become invalid, despite a lack of changes to the original value.
 */
trait UnrecordedRead[+T] {
  /** Returns the value observed by this {@code UnrecordedRead}.  For any
   *  particular {@code UnrecordedRead} instance this method will always return
   *  the same result.
   *  @returns the value observed by this {@code UnrecordedRead}, regardless
   *      of whether or not it is still valid.
   */
  def value: T

  /** Returns true if repeating this unrecorded read in the same context as
   *  it was originally performed would definitely produce the same value, and
   *  would have produced the same value at every point between the creation of
   *  this instance and the call to {@code stillValid}, false if repeating the
   *  read would produce a different value or if an ABA change may have
   *  occurred.  Some STM implementations may occasionally return false
   *  spuriously.
   *  @returns true if the unrecorded read has definitely remained valid from
   *      its original creation until this method invocation, false otherwise.  
   */
  def stillValid: Boolean

  /** Returns true if the unrecorded read was performed in a transaction, and
   *  the source of this read is known to be in the transaction's read or write
   *  set, in which case {@link ppl.stm.UnrecordedRead#stillValid stillValid}
   *  will definitely be true at the transaction's commit (linearization) point
   *  if the transaction commits.  Returns false for non-transactional
   *  unrecorded reads.  This method is for optimization purposes only, the
   *  implementation may return false spuriously.
   *  @returns true if the caller may assume that {@code stillValid} will be
   *      true for this {@code UnrecordedRead} if the bound transaction is
   *      successfully committed.
   */
  def recorded: Boolean
}

