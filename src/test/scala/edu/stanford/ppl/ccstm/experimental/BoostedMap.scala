/* CCSTM - (c) 2009 Stanford University - PPL */

// BoostedMap

package edu.stanford.ppl.ccstm.experimental

import edu.stanford.ppl.ccstm._


object BoostedMap {
  private[BoostedMap] val NULL = new AnyRef

  trait Bound[A,B] {
    def containsKey(key: A): Boolean
    def get(key: A): Option[B]
    def put(key: A, value: B): Option[B]
    def remove(key: A): Option[B]
  }
}

// TODO: store undo log inline in the same TxnLocal as the lock set

/** A boosted <code>ConcurrentHashMap</code>, sans iteration or size.  This is
 *  an implementation of the transactional data structure methodology proposed
 *  in M. Herlify and E. Koskinen, <em>Transactional Boosting: A Methodology
 *  for Highly-Concurrent Transactional Objects</em>, PPoPP 2008.  The primary
 *  change is that weak references are used in the mapping between keys and
 *  locks so that keys are not retained forever.
 *  <p>
 *  The current implementation uses ReentrantLock-s internally, so it does not
 *  protect against concurrent access by multiple Txn (and nonTxn) that happen
 *  to share a thread. 
 */
class BoostedMap[A,B] {
  import BoostedMap._

  private val underlying = new java.util.concurrent.ConcurrentHashMap[A,AnyRef]
  private val booster = new MapBooster[A]

  val nonTxn = new Bound[A,B] {
    def containsKey(key: A): Boolean = booster.nonTxnOp(key) {
      underlying.containsKey(key)
    }

    def get(key: A): Option[B] = unpackOption(booster.nonTxnOp(key) {
      underlying.get(key)
    })

    def put(key: A, value: B): Option[B] = unpackOption(booster.nonTxnOp(key) {
      underlying.put(key, pack(value))
    })

    def remove(key: A): Option[B] = unpackOption(booster.nonTxnOp(key) {
      underlying.remove(key)
    })
  }

  def containsKey(key: A)(implicit txn: Txn): Boolean = {
    val ctx = booster.context
    ctx.lock(key)
    underlying.containsKey(key)
  }

  def get(key: A)(implicit txn: Txn): Option[B] = {
    val ctx = booster.context
    ctx.lock(key)
    unpackOption(underlying.get(key))
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    val ctx = booster.context
    ctx.lock(key)
    val prev = underlying.put(key, pack(value))
    ctx.undo(underlying, key, (if (null == prev) None else Some(prev)))
    unpackOption(prev)
  }

  def remove(key: A)(implicit txn: Txn): Option[B] = {
    val ctx = booster.context
    ctx.lock(key)
    val prev = underlying.remove(key)
    ctx.undo(underlying, key, (if (null == prev) None else Some(prev)))
    unpackOption(prev)
  }

  private def pack(v: B): AnyRef = {
    if (null == v) NULL else v.asInstanceOf[AnyRef]
  }

  private def unpack(v: AnyRef): B = {
    (if (v eq NULL) null else v).asInstanceOf[B]
  }

  private def unpackOption(v: AnyRef): Option[B] = {
    if (null == v) None else Some(unpack(v))
  }
}
