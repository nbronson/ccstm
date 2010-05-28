/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TMap

package edu.stanford.ppl.ccstm.experimental.akka

import edu.stanford.ppl.ccstm._
import java.util.concurrent.atomic._


private object TransactionalRef {
  val metaUpdater = (new TransactionalRef).newMetaUpdater
}

/** A transactional reference whose interface models the references used in
 *  Akka.  Most convenient when used with the `atomic` syntax provided by
 *  `edu.stanford.ppl.ccstm.Dynamic`.  Transaction lookup is dynamic, an
 *  exception will be thrown if an attempt is made to access a
 *  `TransactionalRef` outside of an atomic block.
 *
 *  @author Nathan Bronson
 */
class TransactionalRef[T] extends edu.stanford.ppl.ccstm.impl.Handle[T] {

  // in-place storage of the metadata and the data

  @volatile private[ccstm] var meta: Long = 0L
  private[ccstm] def metaCAS(before: Long, after: Long) = {
    TransactionalRef.metaUpdater.compareAndSet(this, before, after)
  }
  private[TransactionalRef] def newMetaUpdater = {
    AtomicLongFieldUpdater.newUpdater(classOf[TransactionalRef[_]], "meta")
  }

  private[ccstm] def ref: AnyRef = this
  private[ccstm] def offset: Int = 0
  private[ccstm] def metaOffset: Int = 0
  @volatile private[ccstm] var data: T = null.asInstanceOf[T]

  // only dynamic scoping, no static or hybrid scoping using implicit Txn

  private def txn: Txn = {
    val t = Txn.dynCurrentOrNull
    if (t == null) throw new IllegalStateException("TransactionalRef accessed outside an atomic block")
    t
  }

  // public interface

  def swap(elem: T): T = txn.swap(this, elem)

  def get: Option[T] = {
    val z = txn.get(this)
    if (null == z) None else Some(z)
  }

  def getOrWait: T = {
    val z = txn.get(this)
    if (null == z) txn.retry
    z
  }

  def getOrElse(default: => T): T = {
    val z = txn.get(this)
    if (null == z) default else z
  }

  def isDefined = !isEmpty

  def isEmpty: Boolean = { null == txn.get(this) }

  def map[Z](f: T => Z): Option[Z] = {
    val z = txn.get(this)
    if (null == z) None else Some(f(z))
  }

  def flatMap[Z](f: T => Option[Z]): Option[Z] = {
    val z = txn.get(this)
    if (null == z) None else f(z)
  }

  // This models Akka's implementation, but it seems strange that isEmpty => Some(null)
  def filter(p: T => Boolean): Option[T] = {
    val z = txn.get(this)
    if (null == z || p(z)) Some(z) else None
  }

  def foreach(v: T => Unit) {
    val z = txn.get(this)
    if (null != z) v(z)
  }

  def elements: Iterator[T] = {
    val z = txn.get(this)
    if (null == z) Iterator.empty else Iterator.single(z)
  }

  def toList: List[T] = {
    val z = txn.get(this)
    if (null == z) List() else List(z)
  }

  def toRight[X](left: => X): Either[X,T] = {
    val z = txn.get(this)
    if (null == z) Left(left) else Right(z)
  }

  def toLeft[X](right: => X): Either[T,X] = {
    val z = txn.get(this)
    if (null == z) Right(right) else Left(z)
  }
}
