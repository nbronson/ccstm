/* CCSTM - (c) 2009 Stanford University - PPL */

// TxnFieldUpdaterSuite

package edu.stanford.ppl.stm

import edu.stanford.ppl.ccstm._


class TxnFieldUpdaterSuite extends STMFunSuite {
  import TFUSObj._

  test("nonTxn") {
    val r1 = new TFUSObj
    assert(IField(r1).nonTxn.get === 0)
    assert(SField(r1).nonTxn.get === "abc")

    IField(r1).nonTxn.set(1)
    assert(IField(r1).nonTxn.get === 1)

    SField(r1).nonTxn.set("def")
    assert(SField(r1).nonTxn.get === "def")

    val r2 = new TFUSObj
    assert(IField(r2).nonTxn.get === 0)
    assert(SField(r2).nonTxn.get === "abc")

    IField(r2).nonTxn.set(1)
    assert(IField(r2).nonTxn.get === 1)

    SField(r2).nonTxn := "def"
    assert(SField(r2).nonTxn.get === "def")
  }

  test("txn") {
    val r = new TFUSObj
    new Atomic { def body {
      assert(IField(r).get === 0)
      assert(!IField(r) === 0)
      assert(IField.get(r) === 0)
      assert(SField(r).get === "abc")
      assert(!SField(r) === "abc")
      assert(SField.get(r) === "abc")

      IField.set(r, 1)
      assert(IField(r).get === 1)

      SField(r) := "def"
      assert(SField(r).get === "def")
    }}.run()

    assert(IField(r).nonTxn.get === 1)
    assert(SField(r).nonTxn.get === "def")
  }
}

private object TFUSObj {
  val IField = new TxnFieldUpdater[TFUSObj,Int](classOf[TFUSObj], "iField") {
    protected def getField(instance: TFUSObj): Int = instance._iField
    protected def setField(instance: TFUSObj, v: Int) { instance._iField = v }
  }

  val SField = new TxnFieldUpdater[TFUSObj,String](classOf[TFUSObj], "sField") {
    protected def getField(instance: TFUSObj): String = instance._sField
    protected def setField(instance: TFUSObj, v: String) { instance._sField = v }
  }
}

private class TFUSObj extends impl.MetaHolder {
  @volatile private var _iField: Int = 0
  @volatile private var _sField: String = "abc"
}
