/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedHashMap_LazyGC

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import java.util.concurrent.ConcurrentHashMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import edu.stanford.ppl.ccstm.collection.TPairRef
import java.util.concurrent.atomic.{AtomicReferenceFieldUpdater, AtomicReference}

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
      STM.atomic(unbind.get(key)(_))
    }

    override def put(key: A, value: B): Option[B] = {
      STM.atomic(unbind.put(key, value)(_))
    }

    override def removeKey(key: A): Option[B] = {
      STM.atomic(unbind.removeKey(key)(_))
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
    val (tok, pred, prev) = access(key, false)

    // We could equivalently call access(key, true), and then perform the
    // weakening ourself if we observed prev == None (as is done inside
    // transformIfDefined).  The way we do it here allows us to create the
    // Predicate in a pre-weakened state, which saves a CAS.

    prev
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    val (tok, pred, prev) = access(key, true)
    pred.set((tok, value))
    prev
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

  private def deferredCleanup(key: A, pred: Predicate[A,B]) = (t: Txn) => {
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
        // this is like put
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

  private def encodePair(token: Token[A,B], vOpt: Option[B]): (Token[A,B],B) = {
    vOpt match {
      case Some(v) => (token, v)
      case None => (null, null.asInstanceOf[B])
    }
  }

  private def decodePair(pair: (Token[A,B],B)): Option[B] = {
    if (null == pair._1) None else Some(pair._2)
  }

  private def decodePairAndPin(token: Token[A,B], pair: (Token[A,B],B))(implicit txn: Txn): Option[B] = {
    if (null == pair._1) {
      // We need to make sure that this TPairRef survives until the end of the
      // transaction.
      txn.addReference(token)
      None
    } else {
      // The token will survive on its own until the commit of a removeKey,
      // because it is has a strong ref via the transactional state.  If the
      // removal does happen it will invalidate this txn correctly.
      Some(pair._2)
    }
  }

  private def liftF(token: Token[A,B], f: Option[B] => Option[B]) = (pair: (Token[A,B],B)) => encodePair(token, f(decodePair(pair)))

  private def liftPF(token: Token[A,B], pf: PartialFunction[Option[B],Option[B]]) = new PartialFunction[(Token[A,B],B),(Token[A,B],B)] {
    def isDefinedAt(pair: (Token[A,B],B)) = pf.isDefinedAt(decodePair(pair))
    def apply(pair: (Token[A,B],B)) = encodePair(token, pf(decodePair(pair)))
  }

  //////////////// predicate management

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
      var predKnownStale = false
      while (!predKnownStale) {
        val tokenRef = pred.tokenRef
        if (null == tokenRef) {
          // this predicate is stale and must be replaced
          predKnownStale = true
          // <-- FALLTHROUGH
        } else if (tokenRef.isWeak) {
          val token = tokenRef.get
          if (null == token) {
            // this predicate is stale and must be replaced
            predKnownStale = true
            // <-- FALLTHROUGH
          } else {
            // make sure this weakly-referenced token outlives this txn
            txn.addReference(token)
            return (token, pred, None)
          }
        } else {
          // we must perform a strong->weak transition before we can observe
          // absence
          val weakRef = new WeakTokenRef(this, key, tokenRef.get)
          weakRef.pred = pred
          if (pred.tokenRefCAS(tokenRef, weakRef)) {
            // success!
            txn.addReference(tokenRef.get)
            return (tokenRef.get, pred, None)
          }
          // there was a racing strong->weak or a racing strong->stale, try
          // again with the same pred
          // <-- CONTINUE
        }
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

    if (null == pred) {
      // no previous predicate
      val race = predicates.putIfAbsent(key, freshPred)
      if (null != race) {
        // CAS failed, access the predicate that beat us
        return access(key, createAsStrong, race)
      }
    } else {
      // existing stale predicate
      if (!predicates.replace(key, pred, freshPred)) {
        // CAS failed, try again 
        return access(key, createAsStrong)
      }
    }

    // We have to perform a txn read from the new predicate, because another
    // txn may have already updated it.  We don't, however, have to do all of
    // the normal work, because there is no way that the predicate could have
    // become stale.
    return (freshToken, freshPred, decodePair(freshPred.get))
  }
}