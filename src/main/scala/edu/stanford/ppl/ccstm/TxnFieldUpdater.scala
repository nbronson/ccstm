/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnFieldUpdater

package edu.stanford.ppl.ccstm

import impl.{MetaHolder, Handle}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentHashMap


private object TxnFieldUpdater {
  private val offsets = new ConcurrentHashMap[(Class[_],String),Int]
  private val nextOffset = new AtomicInteger(1)

  def getOffset(tClazz: Class[_], fieldName: String): Int = {
    val key = (tClazz, fieldName)
    val existing = offsets.get(key)
    if (existing != 0) {
      existing
    } else {
      val fresh = nextOffset.getAndIncrement
      val race = offsets.putIfAbsent(key, fresh)
      if (race == 0) fresh else race
    }
  }
}

/** Provides transactional access to <em>volatile</em> fields of any class that
 *  extends <code>MetaHolder</code>.  This class can be used to store multiple
 *  transactional fields in a single object, to remove the layer of storage
 *  indirection created by <code>Ref</code> instances.  Two
 *  <code>TxnFieldUpdater</code>s are considered equivalent if they have the
 *  same <code>tClazz</code> and <code>fieldName</code>.
 *  <p>
 *  For example, the following two node implementations will both provide
 *  transactional linked list behavior, but the second will involve fewer
 *  non-transient objects:<pre>
 *
 *    class IndirectNode(initialValue: Int) {
 *      val value = Ref(initialValue)
 *      val next = Ref[IndirectNode](null)
 *    }
 *
 *    ///////
 *
 *    object DirectNode {
 *      val Value = new TxnFieldUpdater[DirectNode,Int](classOf[DirectNode], "value") {
 *        protected def getField(instance: DirectNode) = instance._value
 *        protected def setField(instance: DirectNode, v: Int) { instance._value = v }
 *      }
 *
 *      val Next = new TxnFieldUpdater[DirectNode,DirectNode](classOf[DirectNode], "next") {
 *        protected def getField(instance: DirectNode) = instance._next
 *        protected def setField(instance: DirectNode, v: DirectNode) { instance._next = v }
 *      }
 *    }
 *
 *    class DirectNode(initialValue: Int) extends MetaHolder {
 *      &#064;volatile private var _value = initialValue
 *      &#064;volatile private var _next: DirectNode = null
 *
 *      def value = DirectNode.Value(this)
 *      def next = DirectNode.Next(this)
 *
 *      // alternate property-like style
 *      def value(implicit txn: Txn): Int = DirectNode.Value.get(this)
 *      def value_=(v: Int)(implicit txn: Txn) { DirectNode.Value.set(this, v) }
 *      def valueRef = DirectNode.Value(this) 
 *    }
 *  </pre>
 */
abstract class TxnFieldUpdater[T <: MetaHolder,V](tClazz: Class[T], fieldName: String) {
  protected def getField(instance: T): V
  protected def setField(instance: T, v: V)

  private val offset = TxnFieldUpdater.getOffset(tClazz, fieldName)

  def apply(instance: T): Ref[V] = new Ref[V] with Handle[V] {
    protected def handle: Handle[V] = this

    private[ccstm] def meta: Long = instance.meta
    private[ccstm] def meta_=(v: Long) { instance.meta = v }
    private[ccstm] def metaCAS(before: Long, after: Long): Boolean = instance.metaCAS(before, after)

    private[ccstm] def ref: AnyRef = instance
    private[ccstm] def offset: Int = TxnFieldUpdater.this.offset

    private[ccstm] def data: V = getField(instance)
    private[ccstm] def data_=(v: V) { setField(instance, v) }

    override def toString: String = {
      ("TxnFieldUpdater(" + instance.getClass.getSimpleName + "@" +
              (System.identityHashCode(instance).toHexString) + "." + fieldName + ")")
    }
  }

  def get(instance: T)(implicit txn: Txn): V = this(instance).get
  def set(instance: T, v: V)(implicit txn: Txn) { this(instance).set(v) }

  override def toString: String = {
    "TxnFieldUpdater(" + fieldName + ")"
  }
}