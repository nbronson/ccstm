/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// SpecializationsSuite

package edu.stanford.ppl.ccstm

import collection.TArray
import scala.reflect.ClassManifest
import edu.stanford.ppl.stm.STMFunSuite
import edu.stanford.ppl.ccstm.impl._


class SpecializationsSuite extends STMFunSuite {
  def runTest[T](defaultValue: T, explicitRef: Ref[T], expectedImpl: Class[_])(implicit m: ClassManifest[T]) {
    test("specialization: " + m.erasure) {
      val ref1 = Ref(defaultValue)
      val ref2 = Ref.make[T]()
      val arr1 = new TArray[T](1)
      val arr2 = TArray[T](1)
      val exp = explicitRef
      val dv = defaultValue

      assert(dv === ref1.asInstanceOf[Source[Any]].single.get)
      assert(dv === ref2.asInstanceOf[Source[Any]].single.get)
      assert(dv == arr1.refs(0).asInstanceOf[Source[Any]].single.get)
      assert(dv == arr2.refs(0).asInstanceOf[Source[Any]].single.get)
      assert(dv == explicitRef.asInstanceOf[Source[Any]].single.get)
      assert(ref1.getClass === expectedImpl)
      assert(ref2.getClass === expectedImpl)
      assert(explicitRef.getClass === expectedImpl)

      atomic { implicit t =>
        ref1() = dv
        ref2() = dv
        arr1(0) = dv
        arr2(0) = dv
        exp() = dv
      }
    }
  }

  val zeroByte: Byte = 0
  val zeroShort: Short = 0

  runTest(false, Ref(false), classOf[TBooleanRef])
  runTest(zeroByte, Ref(zeroByte), classOf[TByteRef])
  runTest(zeroShort, Ref(zeroShort), classOf[TShortRef])
  runTest('\0', Ref('\0'), classOf[TCharRef])
  runTest(0, Ref(0), classOf[TIntRef])
  runTest(0.0f, Ref(0.0f), classOf[TFloatRef])
  runTest(0L, Ref(0L), classOf[TLongRef])
  runTest(0.0, Ref(0.0), classOf[TDoubleRef])
  runTest[AnyRef](null, Ref.make[AnyRef](), classOf[TAnyRef[_]])
  runTest[String](null, Ref[String](null), classOf[TAnyRef[_]])
  runTest[Array[Int]](null, Ref[Array[Int]](null), classOf[TAnyRef[_]])
  runTest[Ref[Int]](null, Ref[Ref[Int]](null), classOf[TAnyRef[_]])
}
