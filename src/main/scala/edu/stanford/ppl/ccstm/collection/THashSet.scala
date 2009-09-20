/* CCSTM - (c) 2009 Stanford University - PPL */

// THashSet

package edu.stanford.ppl.ccstm.collection


import java.lang.ref.{Reference, WeakReference, ReferenceQueue}
import java.util.concurrent.ConcurrentHashMap

class THashSet[A] {

  // problem case:
  //   txn A is a read-only txn, iterates over set
  //   non-txn B changes an entry, then removes an entry from set
  //   A may observe the pre-change value, but see the removal 

  def add(key: A)(implicit txn: Txn): Boolean = {
    val r = getTxnRef(key, txn)
    val prev = r.get
    if (!prev) r.set(true)
    !prev
  }

  def remove(key: A)(implicit txn: Txn): Boolean = {
    val r = getTxnRef(key, txn)
    val prev = r.get
    if (prev) r.set(false)
    prev
  }

  def contains(key: A)(implicit txn: Txn): Boolean = getTxnRef(key, txn).get

  def refs: (A => Ref[Boolean]) = getRef(_)

  //////////////// Internal implementation

  /** Maps to either a STMImpl.Data[Boolean] or a WeakReference[STMImpl.Data[Boolean]]. */
  private val _data = new ConcurrentHashMap[A,AnyRef]

  private val _stale = new ReferenceQueue[STMImpl.Data[Boolean]]

  private def getRef(key: A): Ref[Boolean] = new Ref[Boolean] {
    def bind(implicit txn: Txn): Ref.Bound[Boolean] = getTxnRef(key, txn)
    def nonTxn: Ref.Bound[Boolean] = getNonTxnRef(key)
    override def toString = {
      "THashSet@" + Integer.toHexString(THashSet.this.hashCode) + "(" + key + ")"
    }
  }

  private def getTxnRef(key0: A, txn0: Txn): Ref.Bound[Boolean] = new STMImpl.TxnAccessor[Boolean] with Accessor[A] {
    def txn = txn0
    def instance = THashSet.this
    def key = key0
  }

  private def getNonTxnRef(key0: A): Ref.Bound[Boolean] = new STMImpl.NonTxnAccessor[Boolean] with Accessor[A] {
    def instance = THashSet.this
    def key = key0
  }

  private trait Accessor[A] {
    def instance: THashSet[A]
    def key: A
    def unbind: Ref[Boolean] = instance.getRef(key)

    def data: STMImpl.Data[Boolean] = {
      instance._data.get(key) match {
        case null => STMImpl.initialData(false)
        case ref: THashSetWeakRefAndKey[_] => {
          val z = ref.get
          if (z == null) STMImpl.initialData(false) else z
        }
        case x => x.asInstanceOf[STMImpl.Data[Boolean]]
      }
    }
    def data_=(v: STMImpl.Data[Boolean]) {
      instance._data.put(key, v)
    }
    def dataCAS(before: STMImpl.Data[Boolean], after: STMImpl.Data[Boolean]) = {
      instance._data.replace(key, before, after)
    }
  }

}

private class THashSetWeakRefAndKey[A](data: STMImpl.Data[Boolean],
                                       queue: ReferenceQueue[STMImpl.Data[Boolean]],
                                       val key: A) extends WeakReference[STMImpl.Data[Boolean]](data, queue)