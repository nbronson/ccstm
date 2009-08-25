/* $Id$
 *
 * Copyright 2009 Nathan Bronson and Stanford University.
 */

package ppl.stm

/** Callbacks at various points in a <code>Txn</code>'s lifecycle.  The most
 *  straightforward way to use callbacks is to implement one of the callback
 *  traits and call <code>txn.addCallback</code> on the transaction of
 *  interest.  For classes  
 *
 * Callbacks
 *  can be registered one at a time for a transaction, or
 *  @see ppl.stm.Txn#addCallback
 */
object Callbacks {
  
  trait ReadSet {
    /** Called during the <code>Active</code> and/or <code>Prepared</code>
     *  states.  Validation during the <code>Preparing</code> state may be
     *  skipped if no other transactions have committed since the last
     *  validation, or if no <code>WriteSetCallback</code>s have been
     *  registered.  Implementations should call
     *  <code>txn.requireRollback</code> if <code>txn</code> is no longer
     *  valid.
     */
    def validate(txn: Txn)
  }

  trait WriteSet {
    /** Called during the <code>Preparing</code> state.  All locks or other
     *  resources required to complete the commit must be acquired during this
     *  callback, or else <code>txn.requireRollback</code> must be called.
     */
    def prepare(txn: Txn)

    /** Called during the <code>Committing</code> state. */
    def performCommit(txn: Txn)

    /** Called during the <code>RollingBack</code> state. */
    def performRollback(txn: Txn)
  }

  trait PreCompletion {
    /** Called during the <code>Active</code> or <code>MarkedRollback</code>
     *  states, after completion has been requested.
     */
    def beforeCompletion(txn: Txn)
  }

  trait PostCommit {
    /** Called during the <code>Committed</code> state. */
    def afterCommit(txn: Txn)
  }

  trait PostRollback {
    /** Called during the <code>Rolledback</code> state. */
    def afterRollback(txn: Txn)
  }


  //////////////// Slots for callbacks reused on a per-txn basis

  /** A key that provides access to the shared callback instance of type
   *  <i>CB</i> for a transaction.  Shared callbacks are used when it is more
   *  efficient to
   */
  class SharedCallbackKey[CB] private[Txn] (slot0: Int, factory0: Txn => CB) {
    private[Txn] val slot = slot0
    private[Txn] val factory = factory0
  }

  def registerIndexedReadSetCallback[CB <: ReadSetCallback](factory: Txn => CB) = registerCallback(ReadSet, factory)
  def registerIndexedWriteSetCallback[CB <: WriteSetCallback](factory: Txn => CB) = registerCallback(WriteSet, factory)
  def registerIndexedPreCompletionCallback[CB <: PreCompletionCallback](factory: Txn => CB) = registerCallback(PreCompletion, factory)
  def registerIndexedPostCommitCallback[CB <: PostCommitCallback](factory: Txn => CB) = registerCallback(PostCommit, factory)
  def registerIndexedPostRollbackCallback[CB <: PostRollbackCallback](factory: Txn => CB) = registerCallback(PostRollback, factory)

  private[Txn] def registerCallback[CB](index: Int, factory: Txn => CB): SharedCallbackKey[CB] = {
    callbackSlots.synchronized {
      val slot = nextSlot
      callbackSlots(index) = callbackSlots(index) ++ Array(slot)
      nextSlot = slot + 1
      new SharedCallbackKey(slot, factory)
    }
  }

  private[Txn] val ReadSet = 0
  private[Txn] val WriteSet = 1
  private[Txn] val PreCompletion = 2
  private[Txn] val PostCommit = 3
  private[Txn] val PostRollback = 4

  @volatile private[Txn] var nextSlot = 0
  private[Txn] val callbackSlots = Array.make(5, Array[Int]())

  //////////////// Generic callback holders

  private[Txn] abstract class GenericCallback[CB] {
    var callbacks: List[CB] = Nil
    def +=(cb: CB) { callbacks = cb :: callbacks }
  }

  private[Txn] val genericReadSetCallbacks = registerIndexedReadSetCallback(txn =>
          new GenericCallback[ReadSetCallback] with ReadSetCallback {
            def validate(txn: Txn) { for (cb <- callbacks) cb.validate(txn) }
          })
  private[Txn] val genericWriteSetCallbacks = registerIndexedWriteSetCallback(txn =>
          new GenericCallback[WriteSetCallback] with WriteSetCallback {
            def prepare(txn: Txn) { for (cb <- callbacks) cb.prepare(txn) }
            def performCommit(txn: Txn) { for (cb <- callbacks) cb.performCommit(txn) }
            def performRollback(txn: Txn) { for (cb <- callbacks) cb.performRollback(txn) }
          })
  private[Txn] val genericPreCompletionCallbacks = registerIndexedPreCompletionCallback(txn =>
          new GenericCallback[PreCompletionCallback] with PreCompletionCallback {
            def beforeCompletion(txn: Txn) { for (cb <- callbacks) cb.beforeCompletion(txn) }
          })
  private[Txn] val genericPostCommitCallbacks = registerIndexedPostCommitCallback(txn =>
          new GenericCallback[PostCommitCallback] with PostCommitCallback {
            def afterCommit(txn: Txn) { for (cb <- callbacks) cb.afterCommit(txn) }
          })
  private[Txn] val genericPostRollbackCallbacks = registerIndexedPostRollbackCallback(txn =>
          new GenericCallback[PostRollbackCallback] with PostRollbackCallback {
            def afterRollback(txn: Txn) { for (cb <- callbacks) cb.afterRollback(txn) }
          })

}