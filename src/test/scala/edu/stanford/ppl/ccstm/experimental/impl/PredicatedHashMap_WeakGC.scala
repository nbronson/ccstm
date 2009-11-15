/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedHashMap_LazyGC

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import java.util.concurrent.ConcurrentHashMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import edu.stanford.ppl.ccstm.collection.TPairRef
import java.lang.ref.WeakReference
import java.util.concurrent.atomic.AtomicReference


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
    var pred: Predicate[A,B] = null

    def get = this
    def isWeak = false
  }

  // we extend from AtomicReference opportunistically
  private class Predicate[A,B](tokenRef: TokenRef[A,B]) extends AtomicReference[TokenRef[A,B]](tokenRef) {
    val txnRef = new TPairRef[Token[A,B],B]((null, null.asInstanceOf[B]))
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
    prev
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    val (tok, pred, prev) = access(key, true)
    pred.txnRef.set((tok, value))
    prev
  }

  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    val (tok, pred, prev) = access(key, false)
    if (!prev.isEmpty) {
      pred.txnRef.set((null, null.asInstanceOf[B]))
      if (!pred.get.isWeak) {
        txn.afterCommit(t => {
          val r = pred.get
          if (!r.isWeak && pred.compareAndSet(r, null)) {
            // successfully made it stale
            predicates.remove(key, pred)
          }
        })
      }
    }
    prev
  }

//  override def transform(key: A, f: (Option[B]) => Option[B])(implicit txn: Txn) {
//    val tok = activeToken(key)
//    // in some cases this is overkill, but it is always correct
//    txn.addReference(tok)
//    tok.pred.transform(liftF(tok, f))
//  }
//
//  override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]])(implicit txn: Txn): Boolean = {
//    val tok = activeToken(key)
//    // in some cases this is overkill, but it is always correct
//    txn.addReference(tok)
//    tok.pred.transformIfDefined(liftPF(tok, pf))
//  }

  protected def transformIfDefined(key: A,
                                   pfOrNull: PartialFunction[Option[B],Option[B]],
                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
    throw new Error
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
      val txnState = pred.txnRef.get
      if (null != txnState._1) {
        // we are observing presence, no weak ref necessary
        assert (pred.get.get eq txnState._1)
        return (txnState._1, pred, Some(txnState._2))
      }

      // we are observing absence, so we must both make sure that the token is
      // weak, and record a strong reference to it
      var predKnownStale = false
      while (!predKnownStale) {
        val tokenRef = pred.get
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
          if (pred.compareAndSet(tokenRef, weakRef)) {
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
    if (createAsStrong) {
      freshToken.pred = new Predicate(freshToken)
    } else {
      val tokenRef = new WeakTokenRef(this, key, freshToken)
      val pred = new Predicate(tokenRef)
      tokenRef.pred = pred
      freshToken.pred = pred
      txn.addReference(freshToken)
    }

    if (null == pred) {
      // no previous predicate
      val race = predicates.putIfAbsent(key, freshToken.pred)
      if (null == race) {
        // We have to perform a txn read from the new predicate, because
        // another txn may have already updated it.  We don't, however, have to
        // do all of the normal work, because there is no way that the
        // predicate could have become stale.
        return (freshToken, freshToken.pred, decodePair(freshToken.pred.txnRef.get))
      } else {
        // try to read from the racing predicate
        return access(key, createAsStrong, race)
      }
    } else {
      // existing stale predicate
      if (predicates.replace(key, pred, freshToken.pred)) {
        return (freshToken, freshToken.pred, decodePair(freshToken.pred.txnRef.get))
      } else {
        // there was a race, but we don't have the new predicate's value
        return access(key, createAsStrong)
      }
    }
  }
//
//  // must
//
//  // When we get a token, we can get it from either a strong or weak ref in the
//  // predicate.
//  //
//  //   Any txn that observes absence must guarantee that the token
//  // survives until the end of the txn.  If it observes absence during put,
//  // then the write buffer handles this.  If it observes absence during get or
//  // removeKey, then this requires calling txn.addReference().  If no call to
//  // txn.addReference() has ever been made, then the transactions that may have
//  // a reference to the Token are exactly those that have actually changed the
//  // state.  A WriteResource, during the commit of a removeKey, while the locks
//  // are held, can prevent the strong->weak transition (with a CAS), then
//  // remove the predicate.  Any other txn that may have a reference to the
//  // token must also be a mutating reference, and hence those txns are doomed.
//
//  private def existingPred(key: A): Predicate[A,B] = predicates.get(key)
//
//  private def activeToken(key: A): Token[A,B] = activeToken(key, predicates.get(key))
//
//  private def activeToken(key: A, pred: Predicate[A,B]): Token[A,B] = {
//    val token = if (null == pred) null else pred.weakRef.get
//    if (null != token) token else createToken(key, pred)
//  }
//
//  private def createToken(key: A, existing: Predicate[A,B]): Token[A,B] = {
//    val freshToken = new Token[A,B]
//    val tokenRef = new TokenRef(this, key, freshToken)
//    val pred = new Predicate(tokenRef)
//    tokenRef.pred = pred
//    freshToken.pred = pred
//    freshToken
//
//    if (null == existing) {
//      val racingPred = predicates.putIfAbsent(key, freshToken.pred)
//      if (null == racingPred) {
//        // successful
//        freshToken
//      } else {
//        // we've got the predicate that beat us to it, try with that one
//        activeToken(key, racingPred)
//      }
//    } else {
//      if (predicates.replace(key, existing, freshToken.pred)) {
//        // successful
//        freshToken
//      } else {
//        // failure, but replace doesn't give us the old one
//        activeToken(key)
//      }
//    }
//  }
}