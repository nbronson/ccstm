/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnFieldUpdater

package edu.stanford.ppl.ccstm

import impl.{RefOps, Handle}
import java.util.concurrent.ConcurrentHashMap
import edu.stanford.ppl.ccstm.TxnFieldUpdater._
import java.util.concurrent.atomic.{AtomicLongFieldUpdater, AtomicInteger}


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

  private val metadataUpdater = (new Base {}).newMetaUpdater

  // It would be convenient if this was a trait, but then the actual field would
  // change on a per-class basis.  That would require a separate Updater per
  // concrete class, with some sort of Class -> AtomicLongFieldUpdater mapping.

  /** Classes that extend `Base` may use `TxnFieldUpdater` to provide
   *  transactional access to their fields.
   */
  abstract class Base {

    @volatile private[TxnFieldUpdater] var meta: Long = 0L

    private[TxnFieldUpdater] def metaCAS(before: Long, after: Long) = {
      metadataUpdater.compareAndSet(this, before, after)
    }

    private[TxnFieldUpdater] def newMetaUpdater = {
      AtomicLongFieldUpdater.newUpdater(classOf[Base], "meta")
    }
  }  

  abstract class Impl[T <: Base](fieldName: String)(implicit m: ClassManifest[T]) {
    private[ccstm] type InstanceImpl[X,Y,Z] <: T
    private[ccstm] type ValueImpl[X,Y,Z]

    private[ccstm] def getFieldImpl[X,Y,Z](instance: InstanceImpl[X,Y,Z]): ValueImpl[X,Y,Z]
    private[ccstm] def setFieldImpl[X,Y,Z](instance: InstanceImpl[X,Y,Z], v: ValueImpl[X,Y,Z])

    private val offset = TxnFieldUpdater.getOffset(m.erasure, fieldName)

    private[TxnFieldUpdater] def applyImpl[X,Y,Z](instance: InstanceImpl[X,Y,Z]): Ref[ValueImpl[X,Y,Z]] =
        new Handle[ValueImpl[X,Y,Z]] with RefOps[ValueImpl[X,Y,Z]] {
      private[ccstm] def handle: Handle[ValueImpl[X,Y,Z]] = this

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

  /** An implementation of `TxnFieldUpdater` that allows the instance and
   *  value types to be constructed using the abstract type constructors
   *  `Instance[X]` and `Value[X]`, each of which takes a single argument.
   */
  abstract class Generic[T <: Base](fieldName: String)(implicit m: ClassManifest[T]) extends Impl[T](fieldName) {
    type Instance[X] <: T
    type Value[X]

    private[ccstm] type InstanceImpl[X,Y,Z] = Instance[X]
    private[ccstm] type ValueImpl[X,Y,Z] = Value[X]

    /** Overriden for each `TxnFieldUpdater` instance to return the
     *  current value of the managed field in `instance`.  This method
     *  should perform a volatile read.
     */
    protected def getField[X](instance: Instance[X]): Value[X]

    /** Overriden for each `TxnFieldUpdater` instance to update the
     *  value of the managed field in `instance`.  This method should
     *  perform a volatile write.
     */
    protected def setField[X](instance: Instance[X], v: Value[X])

    /** Returns a `Ref` that will provide transactional access to the
     *  field encapsulated by this updater.  To perform accesses to the field
     *  outside a transaction, use `Ref.single`, don't access the field
     *  directly.
     *  @return a `Ref` that provides transactional access to a property of
     *      `instance`.
     */
    def apply[X](instance: Instance[X]): Ref[Value[X]] = applyImpl(instance)

    private[ccstm] def getFieldImpl[X,Y,Z](instance: Instance[X]): Value[X] = getField(instance)
    private[ccstm] def setFieldImpl[X,Y,Z](instance: Instance[X], v: Value[X]) = setField(instance, v)
  }

  /** An implementation of `TxnFieldUpdater` that allows the instance and
   *  value types to be constructed using the abstract type constructors
   *  `Instance[X,Y]` and `Value[X,Y]`, each of which takes two type arguments. 
   */
  abstract class Generic2[T <: Base](fieldName: String)(implicit m: ClassManifest[T]) extends Impl[T](fieldName) {
    type Instance[X,Y] <: T
    type Value[X,Y]

    private[ccstm] type InstanceImpl[X,Y,Z] = Instance[X,Y]
    private[ccstm] type ValueImpl[X,Y,Z] = Value[X,Y]

    /** Overriden for each `TxnFieldUpdater` instance to return the
     *  current value of the managed field in `instance`.  This method
     *  should perform a volatile read.
     */
    protected def getField[X,Y](instance: Instance[X,Y]): Value[X,Y]

    /** Overriden for each `TxnFieldUpdater` instance to update the
     *  value of the managed field in `instance`.  This method should
     *  perform a volatile write.
     */
    protected def setField[X,Y](instance: Instance[X,Y], v: Value[X,Y])

    /** Returns a `Ref` that will provide transactional access to the
     *  field encapsulated by this updater.  To perform accesses to the field
     *  outside a transaction, use `Ref.single`, don't access the field
     *  directly.
     *  @return a `Ref` that provides transactional access to a property of
     *      `instance`.
     */
    def apply[X,Y](instance: Instance[X,Y]): Ref[Value[X,Y]] = applyImpl(instance)

    private[ccstm] def getFieldImpl[X,Y,Z](instance: Instance[X,Y]): Value[X,Y] = getField(instance)
    private[ccstm] def setFieldImpl[X,Y,Z](instance: Instance[X,Y], v: Value[X,Y]) = setField(instance, v)
  }

  /** An implementation of `TxnFieldUpdater` that allows the instance and
   *  value types to be constructed using the abstract type constructors
   *  `Instance[X,Y,Z]` and `Value[X,Y,Z]`, each of which takes three type
   *  arguments.
   */
  abstract class Generic3[T <: Base](fieldName: String)(implicit m: ClassManifest[T]) extends Impl[T](fieldName) {
    type Instance[X,Y,Z] <: T
    type Value[X,Y,Z]

    private[ccstm] type InstanceImpl[X,Y,Z] = Instance[X,Y,Z]
    private[ccstm] type ValueImpl[X,Y,Z] = Value[X,Y,Z]

    /** Overriden for each `TxnFieldUpdater` instance to return the
     *  current value of the managed field in `instance`.  This method
     *  should perform a volatile read.
     */
    protected def getField[X,Y,Z](instance: Instance[X,Y,Z]): Value[X,Y,Z]

    /** Overriden for each `TxnFieldUpdater` instance to update the
     *  value of the managed field in `instance`.  This method should
     *  perform a volatile write.
     */
    protected def setField[X,Y,Z](instance: Instance[X,Y,Z], v: Value[X,Y,Z])

    /** Returns a `Ref` that will provide transactional access to the
     *  field encapsulated by this updater.  To perform accesses to the field
     *  outside a transaction, use `Ref.single`, don't access the field
     *  directly.
     *  @return a `Ref` that provides transactional access to a property of
     *      `instance`.
     */
    def apply[X,Y,Z](instance: Instance[X,Y,Z]): Ref[Value[X,Y,Z]] = applyImpl(instance)

    private[ccstm] def getFieldImpl[X,Y,Z](instance: Instance[X,Y,Z]): Value[X,Y,Z] = getField(instance)
    private[ccstm] def setFieldImpl[X,Y,Z](instance: Instance[X,Y,Z], v: Value[X,Y,Z]) = setField(instance, v)
  }
}

/** Provides transactional access to volatile properties of any class that
 *  extends `TxnFieldUpdater.Base`.  This class can be used to store multiple
 *  transactional fields in a single object, removing the layer of storage
 *  indirection created by `Ref` instances.  Two `TxnFieldUpdater`s are 
 *  considered equal if they have the same `tClazz` and `fieldName`.
 * 
 *  '''The transactional field must be declared `volatile`, and should not be 
 *  accessed directly by any code except that used to implement `getField` and
 *  `setField`.'''  For individual accesses use `Ref.single` on the returned
 *  reference instances.
 * 
 *  `TxnFieldUpdater` should be used when the concrete type of the field is
 *  known.  To parameterize the type of the field over a single type argument,
 *  use `TxnFieldUpdater.Generic` (note that this refers to the number of
 *  parameters required to determine the type of the field, not the type of the
 *  instance).  When using the generic form, the abstract type constructors
 *  `Instance[X]` and `Value[X]` must be defined.  Two and three argument
 *  forms are also provided.
 *
 *  For example, the following two node implementations might both serve as the
 *  building block for a red-black tree, but the second will involve fewer
 *  non-transient objects:
 *
 *  {{{
 *  class IndirectNode[K,V](key0: K, value0: V) {
 *    val color = Ref(false)
 *    val key = Ref(key0)
 *    val value = Ref(value0)
 *    val left = Ref[IndirectNode[K,V]](null)
 *    val right = Ref[IndirectNode[K,V]](null)
 *  }
 *
 *  ///////
 *
 *  object DirectNode {
 *    val Color = new TxnFieldUpdater[DirectNode[_,_],Boolean]("color") {
 *      protected def getField(instance: DirectNode[_,_]) = instance._color
 *      protected def setField(instance: DirectNode[_,_], v: Boolean) { instance._color = v }
 *    }
 *
 *    val Key = new TxnFieldUpdater.Generic[DirectNode[_,_]]("key") {
 *      type Instance[X] = DirectNode[X,_]
 *      type Value[X] = X
 *      protected def getField[K](instance: DirectNode[K,_]) = instance._key
 *      protected def setField[K](instance: DirectNode[K,_], v: K) { instance._key = v }
 *    }
 *
 *    val Value = new TxnFieldUpdater.Generic[DirectNode[_,_]]("value") {
 *      type Instance[X] = DirectNode[_,X]
 *      type Value[X] = X
 *      protected def getField[V](instance: DirectNode[_,V]) = instance._value
 *      protected def setField[V](instance: DirectNode[_,V], v: V) { instance._value = v }
 *    }
 *
 *    val Left = new TxnFieldUpdater.Generic2[DirectNode[_,_]]("left") {
 *      type Instance[X,Y] = DirectNode[X,Y]
 *      type Value[X,Y] = DirectNode[X,Y]
 *      protected def getField[K,V](instance: DirectNode[K,V]) = instance._left
 *      protected def setField[K,V](instance: DirectNode[K,V], v: DirectNode[K,V]) { instance._left = v }
 *    }
 *
 *    val Right = new TxnFieldUpdater.Generic2[DirectNode[_,_]]("right") {
 *      type Instance[X,Y] = DirectNode[X,Y]
 *      type Value[X,Y] = DirectNode[X,Y]
 *      protected def getField[K,V](instance: DirectNode[K,V]) = instance._right
 *      protected def setField[K,V](instance: DirectNode[K,V], v: DirectNode[K,V]) { instance._right = v }
 *    }
 *  }
 *
 *  class DirectNode[K,V](key0: K, value0: V) extends TxnFieldUpdater.Base {
 *    @volatile private var _color = false
 *    @volatile private var _key = key0
 *    @volatile private var _value0 = value0
 *    @volatile private var _left: DirectNode[K,V] = null
 *    @volatile private var _right: DirectNode[K,V] = null
 *
 *    def color = DirectNode.Color(this)
 *    def key = DirectNode.Key(this)
 *    def value = DirectNode.Value(this)
 *    def left = DirectNode.Left(this)
 *    def right = DirectNode.Right(this)
 *
 *    // alternate property-like style
 *    //def valueRef = DirectNode.Value(this)
 *    //def value(implicit txn: Txn) = valueRef.get()
 *    //def value_=(v: V)(implicit txn: Txn) = valueRef.set(v)
 *  }
 *  }}}
 */
abstract class TxnFieldUpdater[T <: Base,V](fieldName: String)(implicit m: ClassManifest[T]
        ) extends TxnFieldUpdater.Impl[T](fieldName) {

  private[ccstm] type InstanceImpl[X,Y,Z] = T
  private[ccstm] type ValueImpl[X,Y,Z] = V

  /** Overriden for each `TxnFieldUpdater` instance to return the
   *  current value of the managed field in `instance`.  This method
   *  should perform a volatile read.
   */
  protected def getField(instance: T): V

  /** Overriden for each `TxnFieldUpdater` instance to update the
   *  value of the managed field in `instance`.  This method should
   *  perform a volatile write.
   */
  protected def setField(instance: T, v: V)

  /** Returns a `Ref` that provides transactional access to the field
   *  encapsulated by this updater.  All accesses to the field should be
   *  performed via the `Ref`.  Non-transactional reads and writes should be
   *  performed using single-operation transactions.
   *  @return a `Ref` that provides transactional access to a field
   *      in `instance`.
   */
  def apply(instance: T): Ref[V] = applyImpl(instance)

  private[ccstm] def getFieldImpl[X,Y,Z](instance: T): V = getField(instance)
  private[ccstm] def setFieldImpl[X,Y,Z](instance: T, v: V) = setField(instance, v)
}
