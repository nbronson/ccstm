/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// StripedIntRef

package edu.stanford.ppl.ccstm.collection

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.impl.{SingleProxyView, Handle}

class StripedIntRef(initialValue: Int) extends IntRef {
  private def NumStripes = 16

  private val stripes: Array[TIntRef] = {
    val a = new Array[TIntRef](NumStripes)
    a(0) = new TIntRef(initialValue)
    var i = 1
    while (i < NumStripes) {
      a(i) = new TIntRef(0)
      i += 1
    }
    a
  }

  private[ccstm] def handle: Handle[Int] = throw new UnsupportedOperationException

  override def single: IntRef.View = new SingleProxyView(this) with IntRef.View {
    override def unbind: IntRef = StripedIntRef.this

    override def += (delta: Int) {
      if (delta != 0) {
        Txn.dynCurrentOrNull match {
          case null => aStripe.escaped += delta
          case txn: Txn => aStripe.+=(delta)(txn)
        }
      }
    }
  }

  override def escaped: IntRef.View = new IntRef.View {
    def unbind = StripedIntRef.this
    def mode: BindingMode = Escaped

    def get: Int = STM.atomic(unbind.get(_))

    def getWith[Z](f: (Int) => Z): Z = f(get)

    def await(pred: Int => Boolean) {
      new Atomic { def body {
        if (!pred(unbind.get)) retry
      }}.run()
    }

    def unrecordedRead: UnrecordedRead[Int] = STM.atomic(unbind.bind(_).unrecordedRead)

    def releasableRead: ReleasableRead[Int] = new ReleasableRead[Int] {
      def context: Option[Txn] = None
      val value: Int = get
      def release() {}
    }

    def set(v: Int) { STM.atomic(unbind.set(v)(_)) }

    def trySet(v: Int): Boolean = STM.atomic(unbind.bind(_).trySet(v))

    def readForWrite: Int = get

    def swap(v: Int): Int = STM.atomic(unbind.swap(v)(_))

    def compareAndSet(before: Int, after: Int): Boolean = STM.atomic(unbind.bind(_).compareAndSet(before, after))

    def compareAndSetIdentity[A <: Int with AnyRef](before: A, after: Int): Boolean = {
      throw new UnsupportedOperationException
    }

    def transform(f: Int => Int) { STM.atomic(unbind.transform(f)(_)) }

    def getAndTransform(f: Int => Int): Int = STM.atomic(unbind.bind(_).getAndTransform(f))

    def tryTransform(f: Int => Int): Boolean = STM.atomic(unbind.bind(_).tryTransform(f))

    def transformIfDefined(pf: PartialFunction[Int,Int]): Boolean = STM.atomic(unbind.transformIfDefined(pf)(_))

    //////// IntRef-specific stuff

    override def += (delta: Int) {
      if (delta != 0) {
        aStripe.escaped += delta
      }
    }
  }

  override def bind(implicit txn: Txn): IntRef.View = new IntRef.View {
    def unbind = StripedIntRef.this
    def mode: BindingMode = txn

    def get: Int = {
      unbind.get
    }

    def getWith[Z](f: (Int) => Z): Z = {
      unbind.getWith(f)
    }

    def await(pred: (Int) => Boolean) {
      if (!pred(get)) {
        txn.retry 
      }
    }

    def unrecordedRead: UnrecordedRead[Int] = new UnrecordedRead[Int] {
      val reads = {
        val a = new Array[UnrecordedRead[Int]](NumStripes)
        var i = 0
        while (i < NumStripes) {
          a(i) = stripes(i).bind.unrecordedRead
          i += 1
        }
        a
      }
      val value = {
        val r = releasableRead
        val z = r.value
        r.release()
        z
      }

      def context: Option[Txn] = Some(txn)

      def recorded: Boolean = false

      def stillValid: Boolean = {
        var i = 0
        while (i < NumStripes) {
          if (!reads(i).stillValid) return false
          i += 1
        }
        return true
      }
    }

    def releasableRead: ReleasableRead[Int] = new ReleasableRead[Int] {
      val reads = {
        val a = new Array[ReleasableRead[Int]](NumStripes)
        var i = 0
        while (i < NumStripes) {
          a(i) = stripes(i).bind.releasableRead
          i += 1
        }
        a
      }
      val value = {
        var s = 0
        var i = 0
        while (i < NumStripes) {
          s += reads(i).value
          i += 1
        }
        s
      }
      
      def context: Option[Txn] = Some(txn)

      def release() {
        var i = 0
        while (i < NumStripes) {
          reads(i).release()
          i += 1
        }
      }
    }

    def set(v: Int) {
      unbind.set(v)
    }

    def trySet(v: Int): Boolean = {
      tryTransform(i => v)
    }

    def readForWrite: Int = {
      var i = 0
      var s = 0
      while (i < NumStripes) {
        s += stripes(i).bind.readForWrite
        i += 1
      }
      s
    }

    def swap(v: Int): Int = {
      var s = stripes(0).swap(v)
      var i = 1
      while (i < NumStripes) {
        s += stripes(i).swap(0)
        i += 1
      }
      s
    }

    def compareAndSet(before: Int, after: Int): Boolean = {
      if (get == before) {
        set(after)
        true
      } else {
        false
      }
    }

    def compareAndSetIdentity[A <: Int with AnyRef](before: A, after: Int): Boolean = {
      throw new UnsupportedOperationException
    }

    def transform(f: Int => Int) {
      unbind.transform(f)
    }

    def getAndTransform(f: Int => Int): Int = {
      val x = get
      set(f(x))
      x
    }

    def tryTransform(f: Int => Int): Boolean = {
      var i = 0
      while (i < NumStripes) {
        if (!stripes(i).bind.tryTransform(e => e)) return false
        i += 1
      }
      stripes(0).set(f(get))
      i = 1
      while (i < NumStripes) {
        stripes(i).set(0)
        i += 1
      }
      return true
    }

    def transformIfDefined(pf: PartialFunction[Int,Int]): Boolean = {
      unbind.transformIfDefined(pf)
    }

    //////// IntRef-specific stuff

    override def += (delta: Int) {
      unbind += delta
    }
  }

  //////////////// txn operations
  
  override def get(implicit txn: Txn): Int = {
    var s = 0
    var i = 0
    while (i < NumStripes) {
      s += stripes(i).get
      i += 1
    }
    s
  }

  override def getWith[Z](f: (Int) => Z)(implicit txn: Txn): Z = {
    // TODO: something better?
    f(get)
  }

  override def set(v: Int)(implicit txn: Txn) {
    stripes(0).set(v)
    var i = 1
    while (i < NumStripes) {
      stripes(i).set(0)
      i += 1
    }
  }

  override def swap(v: Int)(implicit txn: Txn): Int = {
    var s = stripes(0).swap(v)
    var i = 1
    while (i < NumStripes) {
      s += stripes(i).swap(0)
      i += 1
    }
    s
  }

  override def transform(f: Int => Int)(implicit txn: Txn) {
    val x = get
    this += f(x) - x
  }

  override def transformIfDefined(pf: PartialFunction[Int,Int])(implicit txn: Txn): Boolean = {
    val x = get
    if (!pf.isDefinedAt(x)) {
      false
    } else {
      this += pf(x) - x
      true
    }
  }
  
  //////// IntRef-specific stuff

  override def += (delta: Int)(implicit txn: Txn) {
    if (delta != 0) {
      aStripe += delta
    }
  }

  //////////////// access to an element

  /** Provides access to one of the underlying stripes.  The caller may
   *  correctly apply transformations that use only addition, and that do not
   *  depend on the actual value.
   */
  def aStripe: TIntRef = {
    stripes(System.identityHashCode(Thread.currentThread) & (NumStripes - 1))
  }
  
  //////////////// equality stuff in Ref uses handles, must be overriden:

  override def hashCode: Int = {
    System.identityHashCode(this)
  }

  override def equals(rhs: Any): Boolean = {
    this eq rhs.asInstanceOf[AnyRef]
  }
}
