/* CCSTM - (c) 2009 Stanford University - PPL */

// THashSet

package edu.stanford.ppl.ccstm.collection


import java.lang.ref.{Reference, WeakReference, ReferenceQueue}
import java.util.concurrent.ConcurrentHashMap

class THashSet[A] {

  /** Maps to either a STMImpl.Data[Boolean] or a WeakReference[STMImpl.Data[Boolean]]. */
  private val _data = new ConcurrentHashMap[A,AnyRef]

  private val _stale = new ReferenceQueue[STMImpl.Data[Boolean]]

  def refs: (A => Ref[Boolean]) = getRef(_)

  private def getRef(key: A): Ref[Boolean] = null

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