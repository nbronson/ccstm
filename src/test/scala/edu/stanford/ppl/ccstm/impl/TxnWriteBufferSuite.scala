/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnWriteBufferSuite

package edu.stanford.ppl.ccstm.impl


import _root_.scala.collection.jcl.IdentityHashMap
import _root_.scala.collection.mutable.HashMap
import org.scalatest.FunSuite

class TxnWriteBufferSuite extends FunSuite {

  /** TxnWriteBuffer is a trait, so this is a minimal concrete class. */
  class WB extends TxnWriteBuffer {
    override def toString = writeBufferStr
  }

  /** Only needed for identity comparisons. */
  class H[T](val ref: AnyRef, val offset: Int) extends Handle[T] {
    private[ccstm] def meta: Long = 0L
    private[ccstm] def meta_=(v: Long) {}
    private[ccstm] def metaCAS(before: Long, after: Long): Boolean = false
    private[ccstm] def data: T = null.asInstanceOf[T]
    private[ccstm] def data_=(v: T) {}
  }

  case class Put(ref: AnyRef, offset: Int, specValue: Any, handle: Handle[_])

  private class NestedMap extends IdentityHashMap[AnyRef,HashMap[Int,(Any,Handle[_])]] {
    override def apply(ref: AnyRef) = {
      get(ref) match {
        case Some(m) => m
        case None => {
          val m = new HashMap[Int,(Any,Handle[_])]
          put(ref, m)
          m
        }
      }
    }
  }

  def runTest(steps: Iterable[Put]) {
    val wb = new WB
    val reference = new NestedMap
    for (Put(ref, offset, specValue, handle) <- steps) {
      val expected = reference(ref).getOrElse(offset, (null, null))._1
      val actual = wb.writeBufferGet[Any](ref, offset, null)
      assert(expected === actual)
      reference(ref)(offset) = Tuple2.apply[Any,Handle[_]](specValue, handle)
      wb.writeBufferPut(ref, offset, specValue, handle)

      // validate the set
      val fresh = new NestedMap
      wb.writeBufferVisit(new TxnWriteBuffer.Visitor {
        def visit(specValue: Any, handle: Handle[_]): Boolean = {
          fresh(handle.ref)(handle.offset) = (specValue, handle)
          assert(wb.writeBufferGet(handle.ref, handle.offset, null) == specValue)
          true
        }
      })
      assert(fresh.size == reference.size)
      for (ref <- fresh.keys) {
        val f = fresh.get(ref).get
        val r = fresh.get(ref).get
        assert(f.size === r.size)
        for (offset <- f.keys) {
          assert(f(offset) === r(offset))
        }
      }
    }
  }

  test("single entry") {
    runTest(List(
        Put("ref1", 0, "value", new H[String]("ref1", 0)),
        Put("ref1", 0, "value2", new H[String]("ref1", 0))
      ))
  }

  test("many refs") {
    runTest(for (i <- 0 until 200) yield {
      val ref = "ref" + i
      Put(ref, 0, "value" + i, new H[String](ref, 0))
    })
  }

  test("many offsets") {
    runTest(for (i <- 0 until 200) yield Put("ref1", i, "value" + i, new H[String]("ref1", i)))
  }

  def nanosPerGet(size: Int, passes: Int, numReadHits: Int, numReadMisses: Int): Double = {
    val wb = new WB
    for (i <- 0 until size) wb.writeBufferPut("ref", i, "x" + i, new H[String]("ref", i))

    var best = Math.MAX_LONG
    for (p <- 0 until passes) {
      val t0 = System.nanoTime
      var k = 0
      var i = 0
      while (i < numReadHits) {
        wb.writeBufferGet("ref", k, "default")
        k += 1
        i += 1
        if (k == size) k = 0
      }
      k = size
      i = 0
      while (i < numReadMisses) {
        wb.writeBufferGet("ref", k, "default")
        k += 1
        i += 1
      }
      val elapsed = System.nanoTime - t0
      best = best min elapsed
    }

    best * 1.0 / (numReadHits + numReadMisses)
  }

  test("read performance") {
    for (size <- 0 until 64) {
      printf("%d entries -> %3.1f nanos/hit, %3.1f nanos/miss\n",
        size, nanosPerGet(size, 1000, 10000, 0), nanosPerGet(size, 1000, 0, 10000))
    }
  }
}