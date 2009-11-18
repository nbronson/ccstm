/* CCSTM - (c) 2009 Stanford University - PPL */

// PredicatedHashMap_GC_Enum

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.experimental.TMap.Bound
import edu.stanford.ppl.ccstm.{STM, Txn}
import java.lang.ref.WeakReference
import java.util.concurrent.{ConcurrentMap, ConcurrentHashMap}
import edu.stanford.ppl.ccstm.collection.{StripedIntRef, TAnyRef, TPairRef}

private object PredicatedHashMap_GC_Enum {
  private class TokenRef[A,B](map: PredicatedHashMap_GC_Enum[A,B], key: A, token: Token[A,B]) extends CleanableRef[Token[A,B]](token) {
    var pred: Predicate[A,B] = null
    def cleanup(): Unit = map.predicates.remove(key, pred)
  }

  private class Token[A,B] {
    var pred: Predicate[A,B] = null
  }

  // we extend from TPairRef opportunistically
  private class Predicate[A,B](val weakRef: WeakReference[Token[A,B]]
          ) extends TPairRef[Token[A,B],B](null, null.asInstanceOf[B]) {
  }
}

class PredicatedHashMap_GC_Enum[A,B] extends TMap[A,B] {
  import PredicatedHashMap_GC_Enum._

  private val sizeRef = new StripedIntRef(0)
  private val predicates = new ConcurrentHashMap[A,Predicate[A,B]]

  def nonTxn: Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,PredicatedHashMap_GC_Enum[A,B]](this) {

    override def size(): Int = {
      sizeRef.nonTxn.get
    }

    def get(key: A): Option[B] = {
      // p may be missing (null), no longer active (weakRef.get == null), or
      // active (weakRef.get != null).  We don't need to distinguish between
      // the last two cases, since they will both have a null txn Token ref and
      // both correspond to None
      val p = existingPred(key)
      if (null == p) None else decodePair(p.nonTxn.get)
    }

    override def put(key: A, value: B): Option[B] = {
      val tok = activeToken(key)
      val p = tok.pred
      val pn = p.nonTxn
      val before = pn.get
      if (null != before._1) {
        // try to update (no size change)
        if (pn.compareAndSet(before, (tok, value))) {
          // success
          return Some(before._2)
        }
        // failure goes to the transform2 implementation
      }
      val prev = STM.transform2(p, sizeRef.aStripe, (vp: (Token[A,B],B), s: Int) => {
        ((tok, value), (if (null == vp._1) s + 1 else s), vp)
      })
      decodePair(prev)
    }

    override def removeKey(key: A): Option[B] = {
      val p = existingPred(key)
      if (null == p || null == p.nonTxn.get._1) {
        // no need to create a predicate, let's linearize here
        return None
      }

      // if the pred is stale, then getAndSet(None) is a no-op and doesn't harm
      // anything
      val prev = STM.transform2(p, sizeRef.aStripe, (vp: (Token[A,B],B), s: Int) => {
        ((null, null.asInstanceOf[B]), (if (null == vp._1) s else s - 1), vp)
      })
      decodePair(prev)
    }

//    override def transform(key: A, f: (Option[B]) => Option[B]) {
//      val tok = activeToken(key)
//      tok.pred.nonTxn.transform(liftF(tok, f))
//    }
//
//    override def transformIfDefined(key: A, pf: PartialFunction[Option[B],Option[B]]): Boolean = {
//      val tok = activeToken(key)
//      tok.pred.nonTxn.transformIfDefined(liftPF(tok, pf))
//    }
//
//    protected def transformIfDefined(key: A,
//                                     pfOrNull: PartialFunction[Option[B],Option[B]],
//                                     f: Option[B] => Option[B]): Boolean = {
//      throw new Error
//    }

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

  def bind(implicit txn0: Txn): Bound[A, B] = new TMap.AbstractTxnBound[A,B,PredicatedHashMap_GC_Enum[A,B]](txn0, this) {
    def elements: Iterator[(A,B)] = new Iterator[(A,B)] {
      private var apparentSize = 0
      private val iter = predicates.entrySet().iterator
      private var avail: (A,B) = null

      advance()
      if (txn.barging) {
        // auto-readForWrite will prevent the size from changing in another txn
        size
      }

      private def advance() {
        while (iter.hasNext) {
          val e = iter.next()
          val pair = e.getValue.get
          if (null != pair._1) {
            apparentSize += 1
            avail = (e.getKey, pair._2)
            return
          }
        }

        // end of iteration
        if (apparentSize != size) {
          txn.forceRollback(Txn.InvalidReadCause(unbind, "PredicatedHashMap_GC_Enum.Iterator missed elements"))
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

  def isEmpty(implicit txn: Txn): Boolean = {
    sizeRef > 0
  }

  def size(implicit txn: Txn): Int = {
    sizeRef.get
  }

  def get(key: A)(implicit txn: Txn): Option[B] = {
    val tok = activeToken(key)
    decodePairAndPin(tok, tok.pred.get)
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    val tok = activeToken(key)
    // write buffer will pin tok
    val prev = tok.pred.getAndSet((tok, value))
    if (null == prev._1) sizeRef += 1
    decodePair(prev)
  }

  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
    val tok = activeToken(key)
    val prev = tok.pred.getAndSet((null, null.asInstanceOf[B]))
    if (null != prev._1) sizeRef -= 1
    decodePairAndPin(tok, prev)
  }

//  protected def transformIfDefined(key: A,
//                                   pfOrNull: PartialFunction[Option[B],Option[B]],
//                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
//    // TODO: implement
//    throw new UnsupportedOperationException
//  }

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

  private def existingPred(key: A): Predicate[A,B] = predicates.get(key)

  private def activeToken(key: A): Token[A,B] = activeToken(key, predicates.get(key))

  private def activeToken(key: A, pred: Predicate[A,B]): Token[A,B] = {
    val token = if (null == pred) null else pred.weakRef.get
    if (null != token) token else createToken(key, pred)
  }

  private def createToken(key: A, existing: Predicate[A,B]): Token[A,B] = {
    val freshToken = new Token[A,B]
    val tokenRef = new TokenRef(this, key, freshToken)
    val pred = new Predicate(tokenRef)
    tokenRef.pred = pred
    freshToken.pred = pred
    freshToken

    if (null == existing) {
      val racingPred = predicates.putIfAbsent(key, freshToken.pred)
      if (null == racingPred) {
        // successful
        freshToken
      } else {
        // we've got the predicate that beat us to it, try with that one
        activeToken(key, racingPred)
      }
    } else {
      if (predicates.replace(key, existing, freshToken.pred)) {
        // successful
        freshToken
      } else {
        // failure, but replace doesn't give us the old one
        activeToken(key)
      }
    }
  }
}
