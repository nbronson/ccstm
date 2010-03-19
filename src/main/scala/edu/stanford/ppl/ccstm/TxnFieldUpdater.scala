/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnFieldUpdater

package edu.stanford.ppl.ccstm

import impl.{MetaHolder, Handle}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentHashMap


object TxnFieldUpdater {
  private val offsets = new ConcurrentHashMap[(Class[_],String),Int]
  private val nextOffset = new AtomicInteger(1)

  private[ccstm] def getOffset(tClazz: Class[_], fieldName: String): Int = {
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

  abstract class Impl[T <: MetaHolder](fieldName: String)(implicit m: ClassManifest[T]) {
    private[ccstm] type InstanceImpl[X,Y,Z] <: T
    private[ccstm] type ValueImpl[X,Y,Z]

    private[ccstm] def getFieldImpl[X,Y,Z](instance: InstanceImpl[X,Y,Z]): ValueImpl[X,Y,Z]
    private[ccstm] def setFieldImpl[X,Y,Z](instance: InstanceImpl[X,Y,Z], v: ValueImpl[X,Y,Z])

    private val offset = TxnFieldUpdater.getOffset(m.erasure, fieldName)

    private[ccstm] def applyImpl[X,Y,Z](instance: InstanceImpl[X,Y,Z]): Ref[ValueImpl[X,Y,Z]] =
        new Handle[ValueImpl[X,Y,Z]] with Ref[ValueImpl[X,Y,Z]] {
      protected def handle: Handle[ValueImpl[X,Y,Z]] = this

      private[ccstm] def meta: Long = instance.meta
      private[ccstm] def meta_=(v: Long) { instance.meta = v }
      private[ccstm] def metaCAS(before: Long, after: Long): Boolean = instance.metaCAS(before, after)

      private[ccstm] def ref: AnyRef = instance
      private[ccstm] def offset: Int = Impl.this.offset
      private[ccstm] def metaOffset: Int = 0

      private[ccstm] def data: ValueImpl[X,Y,Z] = getFieldImpl(instance)
      private[ccstm] def data_=(v: ValueImpl[X,Y,Z]) { setFieldImpl(instance, v) }

      override def toString: String = {
        ("TxnFieldUpdater.Ref(" + instance.getClass.getSimpleName + "@" +
                (System.identityHashCode(instance).toHexString) + "." + fieldName + ")")
      }
    }

    override def hashCode: Int = offset

    override def equals(rhs: Any): Boolean = {
      rhs match {
        case x: Impl[_] => offset == x.offset
        case _ => false
      }
    }

    override def toString: String = {
      "TxnFieldUpdater(" + fieldName + ")"
    }
  }

  /** An implementation of [[TxnFieldUpdater]] that allows the instance and
   *  value types to be constructed using the abstract type constructors
   *  `Instance[X]` and `Value[X]`, each of which takes a single argument.
   */
  abstract class Generic[T <: MetaHolder](fieldName: String)(implicit m: ClassManifest[T]) extends Impl[T](fieldName) {
    type Instance[X] <: T
    type Value[X]

    private[ccstm] type InstanceImpl[X,Y,Z] = Instance[X]
    private[ccstm] type ValueImpl[X,Y,Z] = Value[X]

    /** Overriden for each <code>TxnFieldUpdater</code> instance to return the
     *  current value of the managed field in <code>instance</code>.  This method
     *  should perform a volatile read.
     */
    protected def getField[X](instance: Instance[X]): Value[X]

    /** Overriden for each <code>TxnFieldUpdater</code> instance to update the
     *  value of the managed field in <code>instance</code>.  This method should
     *  perform a volatile write.
     */
    protected def setField[X](instance: Instance[X], v: Value[X])

    /** Returns a <code>Ref</code> that will provide transactional access to the
     *  field encapsulated by this updater.  Reads and writes of the returned ref
     *  that are performed as part of a <code>Txn</code> will be linearizable
     *  with all other transactional reads and writes.  Reads and writes of the
     *  returned ref performed via <code>Ref.nonTxn</code> will be atomic and
     *  isolated, and strongly ordered with any transactions that access the same
     *  field of <code>instance</code>.
     *  @return a <code>Ref</code> that provides transactional access to a field
     *      in <code>instance</code>.
     */
    def apply[X](instance: Instance[X]): Ref[Value[X]] = applyImpl(instance)

    private[ccstm] def getFieldImpl[X,Y,Z](instance: Instance[X]): Value[X] = getField(instance)
    private[ccstm] def setFieldImpl[X,Y,Z](instance: Instance[X], v: Value[X]) = setField(instance, v)
  }

  /** An implementation of [[TxnFieldUpdater]] that allows the instance and
   *  value types to be constructed using the abstract type constructors
   *  `Instance[X,Y]` and `Value[X,Y]`, each of which takes two type arguments. 
   */
  abstract class Generic2[T <: MetaHolder](fieldName: String)(implicit m: ClassManifest[T]) extends Impl[T](fieldName) {
    type Instance[X,Y] <: T
    type Value[X,Y]

    private[ccstm] type InstanceImpl[X,Y,Z] = Instance[X,Y]
    private[ccstm] type ValueImpl[X,Y,Z] = Value[X,Y]

    /** Overriden for each <code>TxnFieldUpdater</code> instance to return the
     *  current value of the managed field in <code>instance</code>.  This method
     *  should perform a volatile read.
     */
    protected def getField[X,Y](instance: Instance[X,Y]): Value[X,Y]

    /** Overriden for each <code>TxnFieldUpdater</code> instance to update the
     *  value of the managed field in <code>instance</code>.  This method should
     *  perform a volatile write.
     */
    protected def setField[X,Y](instance: Instance[X,Y], v: Value[X,Y])

    /** Returns a <code>Ref</code> that will provide transactional access to the
     *  field encapsulated by this updater.  Reads and writes of the returned ref
     *  that are performed as part of a <code>Txn</code> will be linearizable
     *  with all other transactional reads and writes.  Reads and writes of the
     *  returned ref performed via <code>Ref.nonTxn</code> will be atomic and
     *  isolated, and strongly ordered with any transactions that access the same
     *  field of <code>instance</code>.
     *  @return a <code>Ref</code> that provides transactional access to a field
     *      in <code>instance</code>.
     */
    def apply[X,Y](instance: Instance[X,Y]): Ref[Value[X,Y]] = applyImpl(instance)

    private[ccstm] def getFieldImpl[X,Y,Z](instance: Instance[X,Y]): Value[X,Y] = getField(instance)
    private[ccstm] def setFieldImpl[X,Y,Z](instance: Instance[X,Y], v: Value[X,Y]) = setField(instance, v)
  }

  /** An implementation of [[TxnFieldUpdater]] that allows the instance and
   *  value types to be constructed using the abstract type constructors
   *  `Instance[X,Y,Z]` and `Value[X,Y,Z]`, each of which takes three type
   *  arguments.
   */
  abstract class Generic3[T <: MetaHolder](fieldName: String)(implicit m: ClassManifest[T]) extends Impl[T](fieldName) {
    type Instance[X,Y,Z] <: T
    type Value[X,Y,Z]

    private[ccstm] type InstanceImpl[X,Y,Z] = Instance[X,Y,Z]
    private[ccstm] type ValueImpl[X,Y,Z] = Value[X,Y,Z]

    /** Overriden for each <code>TxnFieldUpdater</code> instance to return the
     *  current value of the managed field in <code>instance</code>.  This method
     *  should perform a volatile read.
     */
    protected def getField[X,Y,Z](instance: Instance[X,Y,Z]): Value[X,Y,Z]

    /** Overriden for each <code>TxnFieldUpdater</code> instance to update the
     *  value of the managed field in <code>instance</code>.  This method should
     *  perform a volatile write.
     */
    protected def setField[X,Y,Z](instance: Instance[X,Y,Z], v: Value[X,Y,Z])

    /** Returns a <code>Ref</code> that will provide transactional access to the
     *  field encapsulated by this updater.  Reads and writes of the returned ref
     *  that are performed as part of a <code>Txn</code> will be linearizable
     *  with all other transactional reads and writes.  Reads and writes of the
     *  returned ref performed via <code>Ref.nonTxn</code> will be atomic and
     *  isolated, and strongly ordered with any transactions that access the same
     *  field of <code>instance</code>.
     *  @return a <code>Ref</code> that provides transactional access to a field
     *      in <code>instance</code>.
     */
    def apply[X,Y,Z](instance: Instance[X,Y,Z]): Ref[Value[X,Y,Z]] = applyImpl(instance)

    private[ccstm] def getFieldImpl[X,Y,Z](instance: Instance[X,Y,Z]): Value[X,Y,Z] = getField(instance)
    private[ccstm] def setFieldImpl[X,Y,Z](instance: Instance[X,Y,Z], v: Value[X,Y,Z]) = setField(instance, v)
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
 *  [[TxnFieldUpdater]] should be used when the concrete type of the field is
 *  known.  To parameterize the type of the field over a single type argument,
 *  use [[TxnFieldUpdater.Generic]] (note that this refers to the number of
 *  parameters required to determine the type of the field, not the type of the
 *  instance).  When using the generic form, the abstract type constructors
 *  `Instance[X]` and `Value[X]` must be defined.  Two and three argument
 *  forms are also provided.
 *  <p>
 *  For example, the following two node implementations might both serve as the
 *  building block for a red-black tree, but the second will involve fewer
 *  non-transient objects:<pre>
 *
 *    class IndirectNode[K,V](key0: K, value0: V) {
 *      val color = Ref(false)
 *      val key = Ref(key0)
 *      val value = Ref(value0)
 *      val left = Ref[IndirectNode[K,V]](null)
 *      val right = Ref[IndirectNode[K,V]](null)
 *    }
 *
 *    ///////
 *
 *    object DirectNode {
 *      val Color = new TxnFieldUpdater[DirectNode[_,_],Boolean]("color") {
 *        protected def getField(instance: DirectNode[_,_]) = instance._color
 *        protected def setField(instance: DirectNode[_,_], v: Boolean) { instance._color = v }
 *      }
 *
 *      val Key = new TxnFieldUpdater.Generic[DirectNode[_,_]]("key") {
 *        type Instance[X] = DirectNode[X,_]
 *        type Value[X] = X
 *        protected def getField[K](instance: DirectNode[K,_]) = instance._key
 *        protected def setField[K](instance: DirectNode[K,_], v: K) { instance._key = v }
 *      }
 *
 *      val Value = new TxnFieldUpdater.Generic[DirectNode[_,_]]("value") {
 *        type Instance[X] = DirectNode[_,X]
 *        type Value[X] = X
 *        protected def getField[V](instance: DirectNode[_,V]) = instance._value
 *        protected def setField[V](instance: DirectNode[_,V], v: V) { instance._value = v }
 *      }
 *
 *      val Left = new TxnFieldUpdater.Generic2[DirectNode[_,_]]("left") {
 *        type Instance[X,Y] = DirectNode[X,Y]
 *        type Value[X,Y] = DirectNode[X,Y]
 *        protected def getField[K,V](instance: DirectNode[K,V]) = instance._left
 *        protected def setField[K,V](instance: DirectNode[K,V], v: DirectNode[K,V]) { instance._left = v }
 *      }
 *
 *      val Right = new TxnFieldUpdater.Generic2[DirectNode[_,_]]("right") {
 *        type Instance[X,Y] = DirectNode[X,Y]
 *        type Value[X,Y] = DirectNode[X,Y]
 *        protected def getField[K,V](instance: DirectNode[K,V]) = instance._right
 *        protected def setField[K,V](instance: DirectNode[K,V], v: DirectNode[K,V]) { instance._right = v }
 *      }
 *    }
 *
 *    class DirectNode[K,V](key0: K, value0: V) extends MetaHolder {
 *      &#064;volatile private var _color = false
 *      &#064;volatile private var _key = key0
 *      &#064;volatile private var _value0 = value0
 *      &#064;volatile private var _left: DirectNode[K,V] = null
 *      &#064;volatile private var _right: DirectNode[K,V] = null
 *
 *      def color = DirectNode.Color(this)
 *      def key = DirectNode.Key(this)
 *      def value = DirectNode.Value(this)
 *      def left = DirectNode.Left(this)
 *      def right = DirectNode.Right(this)
 *
 *      // alternate property-like style
 *      def valueRef = DirectNode.Value(this)
 *      def value(implicit txn: Txn) = valueRef.get()
 *      def value_=(v: V)(implicit txn: Txn) = valueRef.set(v)
 *    }
 *  </pre>
 */
abstract class TxnFieldUpdater[T <: MetaHolder,V](fieldName: String)(implicit m: ClassManifest[T]
        ) extends TxnFieldUpdater.Impl[T](fieldName) {

  private[ccstm] type InstanceImpl[X,Y,Z] = T
  private[ccstm] type ValueImpl[X,Y,Z] = V

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

  /** Returns a <code>Ref</code> that will provide transactional access to the
   *  field encapsulated by this updater.  Reads and writes of the returned ref
   *  that are performed as part of a <code>Txn</code> will be linearizable
   *  with all other transactional reads and writes.  Reads and writes of the
   *  returned ref performed via <code>Ref.nonTxn</code> will be atomic and
   *  isolated, and strongly ordered with any transactions that access the same
   *  field of <code>instance</code>.
   *  @return a <code>Ref</code> that provides transactional access to a field
   *      in <code>instance</code>.
   */
  def apply(instance: T): Ref[V] = applyImpl(instance)

  private[ccstm] def getFieldImpl[X,Y,Z](instance: T): V = getField(instance)
  private[ccstm] def setFieldImpl[X,Y,Z](instance: T, v: V) = setField(instance, v)
}
