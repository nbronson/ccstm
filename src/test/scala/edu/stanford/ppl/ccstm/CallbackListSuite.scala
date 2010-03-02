/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// CallbackListSuite

package edu.stanford.ppl.ccstm


import impl.CallbackList
import scala.collection.mutable.ListBuffer
import org.scalatest.FunSuite

class CallbackListSuite extends FunSuite {
  test("simple") {
    val visited = new ListBuffer[String]
    val cl = new CallbackList[String]
    cl.add("abc", 1)
    cl.add("def", 0)
    cl.add("xyz", 1000)
    for (e <- cl) visited += e
    assert(visited.toList === List("def", "abc", "xyz"))
  }

  test("add to smaller priority") {
    val visited = new ListBuffer[String]
    val cl = new CallbackList[String]
    cl.add("abc", 1)
    cl.add("xyz", 1000)
    for (e <- cl) {
      visited += e
      if (e == "xyz") {
        cl.add("abc2", 1)
        cl.add("def", 0)
      }
    }
    assert(visited.toList === List("abc", "xyz", "def", "abc2"))
  }

  test("add to equal priority") {
    val visited = new ListBuffer[String]
    val cl = new CallbackList[String]
    cl.add("abc", 1)
    cl.add("xyz", 1000)
    for (e <- cl) {
      visited += e
      if (e == "xyz") {
        cl.add("xyz2", 1000)
      }
    }
    assert(visited.toList === List("abc", "xyz", "xyz2"))
  }

  test("add to higher priority") {
    val visited = new ListBuffer[String]
    val cl = new CallbackList[String]
    cl.add("abc", 1)
    cl.add("xyz", 1000)
    for (e <- cl) {
      visited += e
      if (e == "abc") {
        cl.add("ghi", 20)
      }
    }
    assert(visited.toList === List("abc", "ghi", "xyz"))
  }

  test("many") {
    val visited = new ListBuffer[String]
    val cl = new CallbackList[String]
    val expected = List.range(1, 1000).map(_.toString)
    for (e <- expected) cl.add(e, 10)
    for (e <- cl) visited += e
    assert(visited.toList === expected)
  }
}
