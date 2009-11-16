///* CCSTM - (c) 2009 Stanford University - PPL */
//
//// PredicatedHashMap_GC_Enum
//
//package edu.stanford.ppl.ccstm.experimental.impl
//
//import edu.stanford.ppl.ccstm.experimental.TMap
//import java.util.concurrent.ConcurrentHashMap
//import edu.stanford.ppl.ccstm.experimental.TMap.Bound
//import edu.stanford.ppl.ccstm.{STM, Txn}
//import edu.stanford.ppl.ccstm.collection.{TPairRef, StripedIntRef}
//
//object PredicatedHashMap_GC_Enum {
//
//  class Token[A,B](map: PredicatedHashMap_GC_Enum[A,B], key: A) {
//    val pred = new Predicate(map, key, this)
//  }
//
//  // TODO: drop TPairRef and use TxnFieldUpdater
//  class Predicate[A,B](map: PredicatedHashMap_GC_Enum[A,B],
//                       val key: A,
//                       token: Token) extends TPairRef[B,Token[A,B]]((null.asInstanceOf[B], null)) {
//    val weak = new CleanableRef[Token[A,B]](token) {
//      def cleanup() { map.cleanupPred(Predicate.this) }
//    }
//  }
//}
//
//class PredicatedHashMap_GC_Enum[A,B] extends TMap[A,B] {
//  import PredicatedHashMap_GC_Enum._
//
//  private val sizeRef = new StripedIntRef(0)
//  private val predicates = new ConcurrentHashMap[A,Predicate[A,B]]
//
//  def nonTxn: Bound[A,B] = new TMap.AbstractNonTxnBound[A,B,PredicatedHashMap_GC_Enum[A,B]](this) {
//
//    override def size(): Int = {
//      sizeRef.nonTxn.get
//    }
//
//    def get(key: A): Option[B] = {
//      pred(key).nonTxn.get
//    }
//
//    override def put(key: A, value: B): Option[B] = {
//      val p = pred(key)
//      val pn = p.nonTxn
//      val before = pn.get
//      if (!before.isEmpty) {
//        // try to update (no size change)
//        if (pn.compareAndSet(before, Some(value))) {
//          // success
//          return before
//        }
//        // failure goes to the transform2 implementation
//      }
//      return STM.transform2(p, sizeRef.aStripe, (vo: Option[B], s: Int) => {
//        (Some(value), (if (vo.isEmpty) s + 1 else s), vo)
//      })
//    }
//
//    override def removeKey(key: A): Option[B] = {
//      val p = pred(key)
//      val before = p.nonTxn.get
//      if (before.isEmpty) {
//        // let's linearize right here!
//        return None
//      }
//      return STM.transform2(p, sizeRef.aStripe, (vo: Option[B], s: Int) => {
//        (None, (if (vo.isEmpty) s else s - 1), vo)
//      })
//    }
//
//    protected def transformIfDefined(key: A,
//                                     pfOrNull: PartialFunction[Option[B],Option[B]],
//                                     f: Option[B] => Option[B]): Boolean = {
//      val p = pred(key)
//      val pn = p.nonTxn
//      val before = pn.get
//      if (null != pfOrNull && !pfOrNull.isDefinedAt(before)) {
//        // no change
//        return false
//      }
//      // make a CAS attempt
//      val after = f(before)
//      if (!before.isEmpty) {
//        if (!after.isEmpty && pn.compareAndSet(before, after)) {
//          // CAS success, and no size change
//          return true
//        }
//        // failure goes to the transform2 implementation
//      }
//      return STM.transform2(p, sizeRef.aStripe, (vo: Option[B], s: Int) => {
//        if (null != pfOrNull && (vo ne before) && !pfOrNull.isDefinedAt(vo)) {
//          // do nothing and return false
//          (vo, s, false)
//        } else {
//          // can we use the precomputed after?
//          val a = if (vo eq before) after else f(vo)
//          (a, s + (if (a.isEmpty) 0 else 1) - (if (vo.isEmpty) 0 else 1), true)
//        }
//      })
//    }
//
//    def elements: Iterator[(A,B)] = new Iterator[(A,B)] {
//      val iter = predicates.keySet().iterator
//      var avail: (A,B) = null
//      advance()
//
//      private def advance() {
//        while (iter.hasNext) {
//          val k = iter.next()
//          get(k) match {
//            case Some(v) => {
//              avail = (k,v)
//              return
//            }
//            case None => // keep looking
//          }
//        }
//        avail = null
//      }
//
//      def hasNext: Boolean = null != avail
//      def next(): (A,B) = {
//        val z = avail
//        advance()
//        z
//      }
//    }
//  }
//
//  def bind(implicit txn0: Txn): Bound[A, B] = new TMap.AbstractTxnBound[A,B,PredicatedHashMap_GC_Enum[A,B]](txn0, this) {
//    def elements: Iterator[(A,B)] = new Iterator[(A,B)] {
//      private var apparentSize = 0
//      private val iter = predicates.entrySet().iterator
//      private var avail: (A,B) = null
//
//      advance()
//      if (txn.barging) {
//        // auto-readForWrite will prevent the size from changing in another txn
//        size
//      }
//
//      private def advance() {
//        while (iter.hasNext) {
//          val e = iter.next()
//          e.getValue.get match {
//            case Some(v) => {
//              apparentSize += 1
//              avail = (e.getKey, v)
//              return
//            }
//            case None => // keep looking
//          }
//        }
//
//        // end of iteration
//        if (apparentSize != size) {
//          txn.forceRollback(Txn.InvalidReadCause(unbind, "PredicatedHashMap_Basic.Iterator missed elements"))
//        }
//        avail = null
//      }
//
//      def hasNext: Boolean = null != avail
//
//      def next(): (A,B) = {
//        val z = avail
//        advance()
//        z
//      }
//    }
//  }
//
//  def isEmpty(implicit txn: Txn): Boolean = {
//    sizeRef > 0
//  }
//
//  def size(implicit txn: Txn): Int = {
//    sizeRef.get
//  }
//
//  def get(key: A)(implicit txn: Txn): Option[B] = {
//    pred(key).get
//  }
//
//  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
//    val prev = pred(key).getAndSet(Some(value))
//    if (prev.isEmpty) sizeRef += 1
//    prev
//  }
//
//  def removeKey(key: A)(implicit txn: Txn): Option[B] = {
//    val prev = pred(key).getAndSet(None)
//    if (!prev.isEmpty) sizeRef -= 1
//    prev
//  }
//
//  protected def transformIfDefined(key: A,
//                                   pfOrNull: PartialFunction[Option[B],Option[B]],
//                                   f: Option[B] => Option[B])(implicit txn: Txn): Boolean = {
//    val p = pred(key)
//    val prev = p.get
//    if (null != pfOrNull && !pfOrNull.isDefinedAt(prev)) {
//      false
//    } else {
//      val after = f(prev)
//      p.set(after)
//      sizeRef += (if (after.isEmpty) 0 else 1) - (if (prev.isEmpty) 0 else 1)
//      true
//    }
//  }
//
//  private def pred(key: A): TOptionRef[B] = {
//    val pred = predicates.get(key)
//    if (null != pred) pred else createPred(key)
//  }
//
//  private def createPred(key: A): TOptionRef[B] = {
//    val fresh = new TOptionRef[B](None)
//    val race = predicates.putIfAbsent(key, fresh)
//    if (null != race) race else fresh
//  }
//}