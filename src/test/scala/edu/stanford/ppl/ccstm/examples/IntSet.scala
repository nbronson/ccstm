/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// IntSet

package edu.stanford.ppl.ccstm.examples

import edu.stanford.ppl.ccstm._
import scala.annotation.tailrec


object IntSet {
  class Unsynchronized {
    class Node(val e: Int, var next: Node)

    val header = new Node(-1, null)

    def contains(e: Int) = {
      @tailrec
      def loop(cur: Node): Boolean = {
        cur != null && (cur.e == e || loop(cur.next))
      }
      loop(header.next)
    }

    def add(e: Int) {
      @tailrec
      def loop(prev: Node) {
        val cur = prev.next
        if (cur == null || cur.e > e)
          prev.next = new Node(e, cur)
        else if (cur.e != e)
          loop(cur)
      }
      loop(header)
    }
  }

  class Transactional {
    class Node(val e: Int, next0: Node) {
      val next = Ref(next0)
    }

    val header = new Node(-1, null)

    def contains(e: Int) = atomic { implicit t =>
      @tailrec def loop(cur: Node): Boolean = {
        cur != null && (cur.e == e || loop(cur.next()))
      }
      loop(header.next())
    }

    def add(e: Int): Unit = atomic { implicit t =>
      @tailrec def loop(prev: Node) {
        val cur = prev.next()
        if (cur == null || cur.e > e)
          prev.next() = new Node(e, cur)
        else if (cur.e != e)
          loop(cur)
      }
      loop(header)
    }

    def addAll(ee: Int*): Unit = atomic { implicit t =>
      for (e <- ee) add(e)
    }

    def await(e: Int): Unit = atomic { implicit t =>
      if (!contains(e)) retry
    }
  }
}
//
//class IntSet {
//  private class Node(val element: Int, next0: Node) {
//    val next = Ref(next0)
//  }
//
//  private val dummy = new Node(e, null)
//
//  def isEmpty = head.single() != null
//
//  def contains(e: Int) = STM.atomic { t1 =>
//    implicit val t = t1
//
//    @tailrec
//    def loop(n: Node): Boolean = {
//      n != null && (n.element == e || (n.element < e && loop(n.next())))
//    }
//    loop(head())
//  }
//
//  def add(e: Int) = STM.atomic { t1 =>
//    implicit val t = t1
//
//    @tailrec
//    def loop(incoming: Ref[Node]): Boolean = {
//      val n = incoming()
//      if (n == null || n.element > e) {
//        incoming() = new Node(e, n)
//        true
//      } else {
//        n.element != e && loop(n.next)
//      }
//    }
//
//    loop(head)
//  }
//
//  def remove(e: Int) = STM.atomic { t1 =>
//    implicit val t = t1
//
//    @tailrec
//    def loop(incoming: Ref[Node]): Boolean = {
//      val n = incoming()
//      if (n != null && n.element == e) {
//        incoming() = n.next()
//        true
//      } else {
//        n != null && n.element < e && loop(n.next)
//      }
//    }
//
//    loop(head)
//  }
//
//  override def toString = STM.atomic { t1 =>
//    implicit val t = t1
//
//    val buf = new StringBuilder
//
//    @tailrec
//    def loop(first: Boolean, n: Node) {
//      if (n != null) {
//        if (!first) buf append ','
//        buf append n.element
//        loop(false, n.next())
//      }
//    }
//
//    buf append '{'
//    loop(true, head())
//    buf append '}' toString
//  }
//}
