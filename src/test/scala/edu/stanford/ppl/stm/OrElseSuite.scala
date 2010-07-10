/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// OrElseSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._


class OrElseSuite extends STMFunSuite {
  test("simple") {
    val x = atomic { implicit t =>
      if (1 == 1)
        retry
      else
        ""
    } orAtomic { implicit t =>
      "abc"
    }
    assert(x === "abc")
  }

  test("chained") {
    for (expected <- 0 until 4) {
      val x = atomic { implicit t =>
        if (expected != 0) retry
        0
      } orAtomic { implicit t =>
        if (expected != 1) retry
        1
      } orAtomic { implicit t =>
        if (expected != 2) retry
        2
      } orAtomic { implicit t =>
        if (expected != 3) retry
        3
      }  
      assert(x === expected)
    }
  }

  test("empty read set") {
    intercept[IllegalStateException] {
      val x = atomic { implicit t =>
        if (1 == 1) retry
        "abc"
      }
    }
  }

  test("unit result") {
    val x = Ref(10)
    atomic { implicit t =>
      x transform { _ + 1 }
      if (x() > 0) retry
    } orAtomic { implicit t =>
      x transform { _ - 1 }
      if (x() < 0) retry
    }
    assert(x.single() === 9)
  }


  test("widening") {
    def man[T](x: T)(implicit m: Manifest[T]) = m
    class A
    class B extends A
    class C extends A

    val x = atomic { implicit t =>
      new B
    } orAtomic { implicit t =>
      new C
    }

    assert(man(x) === manifest[A])
  }

  // TODO: this test won't work until after #4 is fixed 
  ignore("nesting") {
    atomic { implicit t =>
      atomic { implicit t =>
        "abc"
      } orAtomic { implicit t =>
        "def"
      }
    }
  }
}