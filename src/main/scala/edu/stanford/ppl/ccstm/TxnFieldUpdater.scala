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
 *  <strong>The transactional field must be declared <code>volatile</code>, and
 *  should not be accessed directly by any code except that used to implement
 *  <code>getField</code> and <code>setField</code>.</strong>
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
 *      def valueRef = DirectNode.Value(this)
 *      def value(implicit txn: Txn): Int = !valueRef
 *      def value_=(v: Int)(implicit txn: Txn) { valueRef := v }
 *    }
 *  </pre>
 */
abstract class TxnFieldUpdater[T <: MetaHolder,V](tClazz: Class[T], fieldName: String) {

  /** Overriden for each <code>TxnFieldUpdater</code> instance to return the
   *  current value of the managed field in <code>instance</code>.  This method
   *  should perform a volatile read. 
   */
  protected def getField(instance: T): V

  /** Overriden for each <code>TxnFieldUpdater</code> instance to update the
   *  value of the managed field in <code>instance</code>.  This method should
   *  perform a volatile write.
   */
  protected def setField(instance: T, v: V)

  private val offset = TxnFieldUpdater.getOffset(tClazz, fieldName)

  /** Returns a <code>Ref</code> that will provide transactional access to the
   *  field encapsulated by this updater.  Reads and writes of the returned ref
   *  that are performed as part of a <code>Txn</code> will be linearizable
   *  with all other transactional reads and writes.  Reads and writes of the
   *  returned ref performed via <code>Ref.nonTxn</code> will be atomic and
   *  isolated, and strongly ordered with any transactions that access the same
   *  field of <code>instance</code>.
   *  @returns a <code>Ref</code> that provides transactional access to a field
   *      in <code>instance</code>.
   */
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

  override def hashCode: Int = offset

  override def equals(rhs: Any): Boolean = {
    rhs match {
      case x: TxnFieldUpdater[_,_] => offset == x.offset
      case _ => false
    }
  }

  override def toString: String = {
    "TxnFieldUpdater(" + fieldName + ")"
  }
}
