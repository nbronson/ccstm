/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// TxnFieldUpdaterSuite

package edu.stanford.ppl.ccstm

import edu.stanford.ppl.stm.STMFunSuite

class TxnFieldUpdaterSuite extends STMFunSuite {
  import TFUSObj._

  test("single") {
    val r1 = new TFUSObj
    assert(IField(r1).single.get === 0)
    assert(SField(r1).single.get === "abc")

    IField(r1).single.set(1)
    assert(IField(r1).single.get === 1)

    SField(r1).single.set("def")
    assert(SField(r1).single.get === "def")

    val r2 = new TFUSObj
    assert(IField(r2).single.get === 0)
    assert(SField(r2).single.get === "abc")

    IField(r2).single.set(1)
    assert(IField(r2).single.get === 1)

    SField(r2).single() = "def"
    assert(SField(r2).single.get === "def")
  }

  test("escaped") {
    val r1 = new TFUSObj
    assert(IField(r1).escaped.get === 0)
    assert(SField(r1).escaped.get === "abc")

    IField(r1).escaped.set(1)
    assert(IField(r1).escaped.get === 1)

    SField(r1).escaped.set("def")
    assert(SField(r1).escaped.get === "def")

    val r2 = new TFUSObj
    assert(IField(r2).escaped.get === 0)
    assert(SField(r2).escaped.get === "abc")

    IField(r2).escaped.set(1)
    assert(IField(r2).escaped.get === 1)

    SField(r2).escaped() = "def"
    assert(SField(r2).escaped.get === "def")
  }

  test("txn") {
    val r = new TFUSObj
    new Atomic { def body {
      assert(IField(r).get === 0)
      assert(IField(r)() === 0)
      assert(SField(r).get === "abc")
      assert(SField(r)() === "abc")

      IField(r).set(1)
      assert(IField(r).get === 1)

      SField(r)() = "def"
      assert(SField(r).get === "def")
    }}.run()

    assert(IField(r).single.get === 1)
    assert(SField(r).single.get === "def")
  }

  test("field equality") {
    assert(newIField == newIField)
    assert(newIField != newSField)
    val r = new TFUSObj
    newIField(r).single() = 2
    assert(newIField(r).single.get === 2)
  }
}

private object TFUSObj {
  val IField = newIField

  def newIField = new TxnFieldUpdater[TFUSObj,Int]("iField") {
    protected def getField(instance: TFUSObj): Int = instance._iField
    protected def setField(instance: TFUSObj, v: Int) { instance._iField = v }
  }

  val SField = newSField

  def newSField = new TxnFieldUpdater[TFUSObj,String]("sField") {
    protected def getField(instance: TFUSObj): String = instance._sField
    protected def setField(instance: TFUSObj, v: String) { instance._sField = v }
  }
}

private class TFUSObj extends TxnFieldUpdater.Base {
  @volatile private var _iField: Int = 0
  @volatile private var _sField: String = "abc"
}


private object TFUSGeneric {
  val AField = new TxnFieldUpdater.Generic[TFUSGeneric[_,_]]("aField") {
    type Instance[X] = TFUSGeneric[X,_]
    type Value[X] = X
    protected def getField[A](instance: TFUSGeneric[A,_]): A = instance._aField
    protected def setField[A](instance: TFUSGeneric[A,_], v: A) { instance._aField = v }
  }

  val BField = new TxnFieldUpdater.Generic2[TFUSGeneric[_,_]]("bField") {
    type Instance[X,Y] = TFUSGeneric[X,Y]
    type Value[X,Y] = List[(X,Y)]
    protected def getField[A,B](instance: TFUSGeneric[A,B]) = instance._bField
    protected def setField[A,B](instance: TFUSGeneric[A,B], v: List[(A,B)]) { instance._bField = v }
  }
}

private class TFUSGeneric[A,B](a0: A, b0: List[(A,B)]) extends TxnFieldUpdater.Base {
  @volatile private var _aField = a0
  @volatile private var _bField = b0
}
