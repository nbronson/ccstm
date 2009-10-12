/* CCSTM - (c) 2009 Stanford University - PPL */

// SpecializationsSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm.collection.TArray
import edu.stanford.ppl.ccstm._


class SpecializationsSuite extends STMFunSuite {
  def runTest[T](defaultValue: T, explicitRef: Ref[T])(implicit manifest: scala.reflect.Manifest[T]) {
    test("specialization: " + manifest.erasure) {
      val ref1 = Ref(defaultValue)
      val ref2 = Ref[T]()
      val arr = new TArray[T](1)
      val exp = explicitRef
      val dv = defaultValue

      assert(dv === ref1.asInstanceOf[Source[Any]].nonTxn.get)
      assert(dv === ref2.asInstanceOf[Source[Any]].nonTxn.get)
      assert(dv == arr.refs(0).asInstanceOf[Source[Any]].nonTxn.get)
      assert(dv == explicitRef.asInstanceOf[Source[Any]].nonTxn.get)

      new Atomic { def body {
        ref1 := dv
        ref2 := dv
        arr(0) = dv // TODO: should this be "arr(0) := dv" ?
        exp := dv
      }}.run()
    }
  }

  val zeroByte: Byte = 0
  val zeroShort: Short = 0

  runTest(false, Ref(false))
  runTest(zeroByte, Ref(zeroByte))
  runTest(zeroShort, Ref(zeroShort))
  runTest('\0', Ref('\0'))
  runTest(0, Ref(0))
  runTest(0.0f, Ref(0.0f))
  runTest(0L, Ref(0L))
  runTest(0.0, Ref(0.0))
  runTest[AnyRef](null, Ref[AnyRef]())
  runTest[String](null, Ref[String](null))
  runTest[Array[Int]](null, Ref[Array[Int]](null))
  runTest[Ref[Int]](null, Ref[Ref[Int]](null))
}