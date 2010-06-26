/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// StripedIntRef

package edu.stanford.ppl.ccstm
package impl

private[ccstm] class StripedIntRef(initialValue: Int) extends Ref[Int] {
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

  private abstract class ViewImpl extends Ref.View[Int] {
    protected def run[Z](block: Txn => Z): Z

    def unbind = StripedIntRef.this

    def get: Int = run { t => unbind.get(t) }

    def getWith[Z](f: (Int) => Z): Z = f(get)

    def await(pred: Int => Boolean) { run { t => if (!pred(unbind.get(t))) t.retry } }

    def unrecordedRead: UnrecordedRead[Int] = run { t => unbind.bind(t).unrecordedRead }

    def releasableRead: ReleasableRead[Int] = run { t => unbind.bind(t).releasableRead }

    def set(v: Int) { run { t => unbind.set(v)(t) } }

    def trySet(v: Int): Boolean = run { t => unbind.bind(t).trySet(v) }

    def readForWrite: Int = get

    def swap(v: Int): Int = run { t => unbind.swap(v)(t) }

    def compareAndSet(before: Int, after: Int): Boolean = run { t => unbind.bind(t).compareAndSet(before, after) }

    def compareAndSetIdentity[A <: Int with AnyRef](before: A, after: Int): Boolean = {
      throw new UnsupportedOperationException
    }

    def transform(f: Int => Int) { run { t => unbind.transform(f)(t) } }

    def getAndTransform(f: Int => Int): Int = run { t => unbind.bind(t).getAndTransform(f) }

    def tryTransform(f: Int => Int): Boolean = run { t => unbind.bind(t).tryTransform(f) }

    def transformIfDefined(pf: PartialFunction[Int,Int]): Boolean = run { t => unbind.transformIfDefined(pf)(t) }

    override def -= (rhs: Int)(implicit num: Numeric[Int]) { this += (-rhs) }
  }

  def single: Ref.View[Int] = new ViewImpl {
    protected def run[Z](block: (Txn) => Z): Z = STM.atomic(block)

    def mode: AccessMode = Single

    override def += (rhs: Int)(implicit num: Numeric[Int]) { if (rhs != 0) aStripe.single += rhs }
  }

  def escaped: Ref.View[Int] = new ViewImpl {
    protected def run[Z](block: (Txn) => Z): Z = {
      // This is deep magic, and a bit brittle.  It is safe because the
      // contention manager as it currently exists won't block a transaction
      // that has already performed a write.
      val ctx = ThreadContext.get
      val saved = ctx.txn
      if (saved == null) {
        STM.atomic(block)
      } else {
        saved.detach(ctx)
        try {
          val x = Ref(0)
          STM.atomic { t =>
            x.set(1)(t)
            block(t)
          }
        } finally {
          saved.attach(ctx)
        }
      }
    }

    def mode: AccessMode = Escaped

    override def += (rhs: Int)(implicit num: Numeric[Int]) { if (rhs != 0) aStripe.escaped += rhs }
  }

  def bind(implicit txn: Txn): Ref.View[Int] = new Ref.View[Int] {
    def unbind = StripedIntRef.this
    def mode: AccessMode = txn

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

    override def += (delta: Int)(implicit num: Numeric[Int]) {
      unbind += delta
    }

    override def -= (delta: Int)(implicit num: Numeric[Int]) {
      unbind += (-delta)
    }
  }

  //////////////// txn operations
  
  def get(implicit txn: Txn): Int = {
    var s = 0
    var i = 0
    while (i < NumStripes) {
      s += stripes(i).get
      i += 1
    }
    s
  }

  def getWith[Z](f: (Int) => Z)(implicit txn: Txn): Z = {
    // TODO: something better?
    f(get)
  }

  def set(v: Int)(implicit txn: Txn) {
    stripes(0).set(v)
    var i = 1
    while (i < NumStripes) {
      stripes(i).set(0)
      i += 1
    }
  }

  def swap(v: Int)(implicit txn: Txn): Int = {
    var s = stripes(0).swap(v)
    var i = 1
    while (i < NumStripes) {
      s += stripes(i).swap(0)
      i += 1
    }
    s
  }

  def transform(f: Int => Int)(implicit txn: Txn) {
    val x = get
    this += f(x) - x
  }

  def transformIfDefined(pf: PartialFunction[Int,Int])(implicit txn: Txn): Boolean = {
    val x = get
    if (!pf.isDefinedAt(x)) {
      false
    } else {
      this += pf(x) - x
      true
    }
  }
  
  //////// IntRef-specific stuff

  override def += (v: Int)(implicit txn: Txn, num: Numeric[Int]) {
    if (v != 0) {
      aStripe += v
    }
  }

  override def -= (v: Int)(implicit txn: Txn, num: Numeric[Int]) { this += (-v) }

  //////////////// access to an element

  /** Provides access to one of the underlying stripes.  The caller may
   *  correctly apply transformations that use only addition, and that do not
   *  depend on the actual value.
   */
  def aStripe: TIntRef = {
    //stripes(System.identityHashCode(Thread.currentThread) & (NumStripes - 1))
    stripes((Thread.currentThread.getId.asInstanceOf[Int] * 5) & (NumStripes - 1))
  }


  private[ccstm] def embalm(identity: Int) {
    for (s <- stripes) s.embalm(identity)
  }
  
  private[ccstm] def resurrect(identity: Int) {
    for (s <- stripes) s.resurrect(identity)
  }
}
