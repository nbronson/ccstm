/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedHashMap_LazyGC

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import java.util.concurrent.ConcurrentHashMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import edu.stanford.ppl.ccstm.collection.TPairRef
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

private object PredicatedHashMap_LazyGC {

  private trait TokenRef[A,B] {
    def get: Token[A,B]
    def isWeak: Boolean
  }

  private class WeakTokenRef[A,B](map: PredicatedHashMap_LazyGC[A,B],
                                  key: A,
                                  token: Token[A,B]) extends CleanableRef[Token[A,B]](token) with TokenRef[A,B] {
    var pred: Predicate[A,B] = null

    def cleanup(): Unit = map.predicates.remove(key, pred)
    def isWeak = true
  }

  // We use the Token as its own strong reference to itself.  A more
  // straightforward embedding into the type system would be to have
  // predicate.tokenRef: Either[Token,WeakRef[Token]], but then we would have
  // an extra Left or Right instance for each ref.
  private class Token[A,B] extends TokenRef[A,B] {
    def get = this
    def isWeak = false
  }

  private val predicateTokenRefUpdater = new Predicate[Int,Int](null).newUpdater()

  // we extend from TPairRef opportunistically
  private class Predicate[A,B](tokenRef0: TokenRef[A,B]) extends TPairRef[Token[A,B],B]((null, null.asInstanceOf[B])) {
    @volatile private var _tokenRef: TokenRef[A,B] = tokenRef0
    def newUpdater() = AtomicReferenceFieldUpdater.newUpdater(classOf[Predicate[_,_]], classOf[TokenRef[_,_]], "_tokenRef")

    def tokenRef = _tokenRef
    def tokenRefCAS(before: TokenRef[A,B], after: TokenRef[A,B]) = {
      predicateTokenRefUpdater.compareAndSet(this, before, after)
    }
  }
}

class PredicatedHashMap_LazyGC[A,B] extends TMap[A,B] {
  import PredicatedHashMap_LazyGC._

  private val predicates = new ConcurrentHashMap[A,Predicate[A,B]]

  def nonTxn: Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,PredicatedHashMap_LazyGC[A,B]](this) {

    def get(key: A): Option[B] = {
      val p = existingPred(key)
      if (p == null) None else decodePair(p.nonTxn.get)
    }

    override def put(key: A, value: B): Option[B] = {
      putImpl(key, value, existingPred(key))
    }

    private def putImpl(key: A, value: B, p: Predicate[A,B]): Option[B] = {

      // put is harder than removeKey because removeKey can blindly write None
      // to a stale entry without causing problems

      if (null != p) {
        val tokenRef = p.tokenRef
        if (null != tokenRef) {
          val token = tokenRef.get
          if (null != token) {
            // the predicate is still active (or was two lines ago)
            if (tokenRef.isWeak) {
              // this predicate is already in its most general state, and it is
              // active, so we will always succeed
              return decodePair(p.nonTxn.getAndSet((token, value)))
            } else {
              // the predicate is strong, but we can still perform a Some -> Some
              // transition
              val prevPair = p.nonTxn.get
              if (null != prevPair._1 && p.nonTxn.compareAndSetIdentity(prevPair, (token, value))) {
                // success
                return Some(prevPair._2)
              } else if (null != ensureWeak(key, p)) {
                return decodePair(p.nonTxn.getAndSet((token, value)))
              }
              // else p is stale and must be replaced
            }
          }
          // else p is stale and must be replaced
        }
        // else p is stale and must be replaced
      }
      // else there is no p, so we must create one

      val freshToken = new Token[A,B]
      val freshPred = new Predicate(freshToken)

      if (null == p) {
        // no previous predicate
        val race = predicates.putIfAbsent(key, freshPred)
        if (null != race) {
          // CAS failed, try again using the predicate that beat us
          return putImpl(key, value, race)
        }
      } else {
        // existing stale predicate
        if (!predicates.replace(key, p, freshPred)) {
          // someone else already removed the predicate, can we still use the
          // fresh one we just created?
          var race = predicates.get(key)
          if (null == race) {
            // second try, afterward race will be null on success
            race = predicates.putIfAbsent(key, freshPred)
          }
          if (null != race) {
            return putImpl(key, value, race)
          }
          // else second-try success
        }
      }
      
      decodePair(freshPred.nonTxn.getAndSet(freshToken, value))
    }

    override def removeKey(key: A): Option[B] = {
      val p = existingPred(key)
      if (null == p) {
        // no predicate means no entry, as for get()
        return None
      }

      // We don't need to weaken to observe absence or presence here.  If we
      // see a strong pred then every thread that has already observed the
      // predicate will conflict with the -> None transition, and threads that
      // have not yet observed the pred must serialize after us anyway.  The
      // only wrinkle is that we can't clean up the strong ref if we observe
      // absence, because another txn may have created the predicate but not
      // yet done the store to populate it.
      val prevPair = p.nonTxn.getAndSet((null, null.asInstanceOf[B]))
      if (null == prevPair._1) {
        // Not previously present, somebody else's cleanup problem.  The
        // predicate may have been stale, and already removed.
        return None
      } else {
        // the committed state was Some(v), so it falls to us to clean up if
        // the predicate was strong
        immediateCleanup(key, p)
        return Some(prevPair._2)
      }
    }

    override def transform(key: A, f: (Option[B]) => Option[B]) {
      STM.atomic(unbind.transform(key, f)(_))
    }

    override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean = {
      STM.atomic(unbind.transformIfDefined(key, pf)(_))
    }

    protected def transformIfDefined(key: A,
                                     pfOrNull: PartialFunction[Option[B],Option[B]],
                                     f: Option[B] => Option[B]): Boolean = {
      throw new Error
    }

    def elements: Iterator[(A,B)] = new Iterator[(A,B)] {
      val iter = predicates.keySet().iterator
      var avail: (A,B) = null
      advance()

      private def advance() {
        while (iter.hasNext) {
          val k = iter.next()
          get(k) match {
            case Some(v) => {
              avail = (k,v)
              return
            }
            case None => // keep looking
          }
        }
        avail = null
      }

      def hasNext: Boolean = null != avail
      def next(): (A,B) = {
        val z = avail
        advance()
        z
      }
    }
  }

  def bind(implicit txn0: Txn): Bound[A, B] = new TMap.AbstractTxnBound[A,B,PredicatedHashMap_LazyGC[A,B]](txn0, this) {
    def elements: Iterator[(A,B)] = throw new UnsupportedOperationException
  }

  def isEmpty(implicit txn: Txn): Boolean = throw new UnsupportedOperationException

  def size(implicit txn: Txn): Int = throw new UnsupportedOperationException

  def get(key: A)(implicit txn: Txn): Option[B] = {
    getImpl(key, predicates.get(key))
  }

  private def getImpl(key: A, pred: Predicate[A,B])(implicit txn: Txn): Option[B] = {
    if (null != pred) {
      val txnState = pred.get
      if (null != txnState._1) {
        // we are observing presence, predicate is definitely active and either
        // strong or weak is okay  
        assert (pred.tokenRef.get eq txnState._1)
        return Some(txnState._2)
      }

      // we are observing absence, so we must both make sure that the token is
      // weak, and record a strong reference to it
      val token = ensureWeak(key, pred)
      if (null != token) {
        txn.addReference(token)
        return None
      }
      // else predicate is stale and must be replaced
    }
    // else there is no predicate and we must make one

    val freshToken = new Token[A,B]
    val tokenRef = new WeakTokenRef(this, key, freshToken)
    val freshPred = new Predicate(tokenRef)
    tokenRef.pred = freshPred

    txn.addReference(freshToken)

    if (null == pred || !predicates.replace(key, pred, freshPred)) {
      // No previous predicate, or predicate we think is previous is no longer
      // there (and most likely removed by the thread that made it stale).
      val race = predicates.putIfAbsent(key, freshPred)
      if (null != race) {
        // a new predicate is available, retry using it
        return getImpl(key, race)
      }
    }

    // We have to perform a txn read from our new predicate, because another
    // txn may have already updated it.  We know, however, that it is active.
    return decodePair(freshPred.get)
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    putImpl(key, value, predicates.get(key))
  }

  private def putImpl(key: A, value: B, pred: Predicate[A,B])(implicit txn: Txn): Option[B] = {
    if (null != pred) {
      val txnState = pred.get
      if (null != txnState._1) {
        // we are observing presence, no weak ref necessary
        assert (pred.tokenRef.get eq txnState._1)
        pred.set((txnState._1, value))
        return Some(txnState._2)
      }

      // we are observing absence, so we must both make sure that the token is
      // weak, and record a strong reference to it
      val token = ensureWeak(key, pred)
      if (null != token) {
        txn.addReference(token)
        pred.set((token, value))
        return None
      }
    }

    // There is either no predicate or the predicate is stale.  Make a new one.
    val freshToken = new Token[A,B]
    val freshPred = new Predicate(freshToken)

    // If this txn rolls back and nobody has done the strong -> weak
    // conversion, then we must either do that or remove the predicate.  It
    // is likely that the next txn attempt will reinsert the key, but for now
    // we remove and let it reinsert as strong.
    txn.afterRollback(deferredCleanup(key, freshPred))

    if (null == pred || !predicates.replace(key, pred, freshPred)) {
      // No previous predicate, or predicate we think is previous is no longer
      // there (and most likely removed by the thread that made it stale).
      val race = predicates.putIfAbsent(key, freshPred)
      if (null != race) {
        // CAS failed, access the predicate that beat us
        return putImpl(key, value, race)
      }
    }

    // We have to perform a txn read from the new predicate, because another
    // txn may have already updated it.  We don't, however, have to do all of
    // the normal work, because there is no way that the predicate could have
    // become stale.
    decodePair(freshPred.getAndSet((freshToken, value)))
  }

  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    val (tok, pred, prev) = access(key, false)
    if (!prev.isEmpty) {
      pred.set((null, null.asInstanceOf[B]))
      if (!pred.tokenRef.isWeak) {
        txn.afterCommit(deferredCleanup(key, pred))
      }
    }
    prev
  }

  private def deferredCleanup(key: A, pred: Predicate[A,B]) = (t: Txn) => immediateCleanup(key, pred)
  
  private def immediateCleanup(key: A, pred: Predicate[A,B]) {
    val r = pred.tokenRef
    if (!r.isWeak && pred.tokenRefCAS(r, null)) {
      // successfully made it stale
      predicates.remove(key, pred)
      // no need to retry on remove failure, somebody else did it for us
    }
  }

  protected def transformIfDefined(key: A,
                                   pfOrNull: PartialFunction[Option[B],Option[B]],
                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
    val (tok, pred, prev) = access(key, true)
    val defined = null == pfOrNull || pfOrNull.isDefinedAt(prev)
    val after = if (!defined) prev else f(prev)
    after match {
      case Some(v) => {
        // this is like put when defined
        if (defined) pred.set((tok, v))
      }
      case None => {
        if (prev.isEmpty) {
          // No write needed, but we have now observed absent, so we must
          // perform the strong -> weak transition.
          val tokenRef = pred.tokenRef
          if (!tokenRef.isWeak) {
            val weakRef = new WeakTokenRef(this, key, tok)
            weakRef.pred = pred
            if (!pred.tokenRefCAS(tokenRef, weakRef)) {
              // racing strong -> weak transition.  Can't be strong -> stale
              // because we were the one that created the predicate
              assert (pred.tokenRef.get eq tok)
            }
            txn.addReference(tok)
          }
        } else {
          // This is like removeKey.
          pred.set((null, null.asInstanceOf[B]))
          if (!pred.tokenRef.isWeak) {
            txn.afterCommit(deferredCleanup(key, pred))
          }
        }
      }
    }
    defined
  }

  //////////////// encoding and decoding into the pair

//  private def encodePair(token: Token[A,B], vOpt: Option[B]): (Token[A,B],B) = {
//    vOpt match {
//      case Some(v) => (token, v)
//      case None => (null, null.asInstanceOf[B])
//    }
//  }

  private def decodePair(pair: (Token[A,B],B)): Option[B] = {
    if (null == pair._1) None else Some(pair._2)
  }

//  private def liftF(token: Token[A,B], f: Option[B] => Option[B]) = (pair: (Token[A,B],B)) => encodePair(token, f(decodePair(pair)))
//
//  private def liftPF(token: Token[A,B], pf: PartialFunction[Option[B],Option[B]]) = new PartialFunction[(Token[A,B],B),(Token[A,B],B)] {
//    def isDefinedAt(pair: (Token[A,B],B)) = pf.isDefinedAt(decodePair(pair))
//    def apply(pair: (Token[A,B],B)) = encodePair(token, pf(decodePair(pair)))
//  }

  //////////////// predicate management

  private def existingPred(key: A) = predicates.get(key)

  // Our invariant for strong vs. weak ref is that any txn that observes 
  // absence (in the transactional state), that didn't create the predicate,
  // and that sees that the strong ref is still in use, must perform a
  // strong->weak transition.

  private def access(key: A, createAsStrong: Boolean)(implicit txn: Txn): (Token[A,B], Predicate[A,B], Option[B]) = {
    access(key, createAsStrong, predicates.get(key))
  }

  private def access(key: A, createAsStrong: Boolean, pred: Predicate[A,B])(implicit txn: Txn): (Token[A,B], Predicate[A,B], Option[B]) = {
    if (null != pred) {
      val txnState = pred.get
      if (null != txnState._1) {
        // we are observing presence, no weak ref necessary
        assert (pred.tokenRef.get eq txnState._1)
        return (txnState._1, pred, Some(txnState._2))
      }

      // we are observing absence, so we must both make sure that the token is
      // weak, and record a strong reference to it
      val token = ensureWeak(key, pred)
      if (null != token) {
        txn.addReference(token)
        return (token, pred, None)
      }
    }

    // There is either no predicate or the predicate is stale.  Make a new one.
    val freshToken = new Token[A,B]
    val freshPred = (if (createAsStrong) {
      val p = new Predicate(freshToken)

      // If this txn rolls back and nobody has done the strong -> weak
      // conversion, then we must either do that or remove the predicate.  It
      // is likely that the next txn attempt will reinsert the key, but for now
      // we remove and let it reinsert as strong.
      txn.afterRollback(deferredCleanup(key, p))
      p
    } else {
      txn.addReference(freshToken)
      val tokenRef = new WeakTokenRef(this, key, freshToken)
      tokenRef.pred = new Predicate(tokenRef)
      tokenRef.pred
    })

    if (null == pred || !predicates.replace(key, pred, freshPred)) {
      // No previous predicate, or predicate we think is previous is no longer
      // there (and most likely removed by the thread that made it stale).
      val race = predicates.putIfAbsent(key, freshPred)
      if (null != race) {
        // CAS failed, access the predicate that beat us
        return access(key, createAsStrong, race)
      }
    }

    // We have to perform a txn read from the new predicate, because another
    // txn may have already updated it.  We don't, however, have to do all of
    // the normal work, because there is no way that the predicate could have
    // become stale.
    return (freshToken, freshPred, decodePair(freshPred.get))
  }

  // returns null if unsuccessful
  private def ensureWeak(key: A, pred: Predicate[A,B]): Token[A,B] = {
    val tokenRef = pred.tokenRef
    if (null == tokenRef) {
      // this predicate is stale and must be replaced
      null
    } else if (tokenRef.isWeak) {
      // possible success, but not of our doing
      tokenRef.get
    } else {
      // try the strong->weak transition
      val token = tokenRef.get
      val weakRef = new WeakTokenRef(this, key, token)
      weakRef.pred = pred
      if (pred.tokenRefCAS(tokenRef, weakRef)) {
        // success!
        token
      } else {
        // another thread either did our work for us (strong -> weak) or
        // prevented us from succeeding (strong -> stale)
        if (null != pred.tokenRef) token else null
      }
    }
  }
}