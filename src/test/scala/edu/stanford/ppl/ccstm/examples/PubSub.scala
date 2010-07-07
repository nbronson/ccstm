/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// PubSub

package edu.stanford.ppl.ccstm
package examples

import scala.collection.immutable.Set
import java.lang.String

object PubSub {
  /** Update does not occur until txn commits. */
  case class Update[T](txn: Txn, added: Set[T], removed: Set[T])

  type Subscriber[T] = Update[T] => Unit

  class PublishingContainer[T] {
    private val subscribers = Ref(Set.empty[Subscriber[T]])
    private val children = Ref(Set.empty[T])

    // this method allows only transactional reads
    def objects = children : Source[Set[T]]

    // this holds the changes made in this txn
    private class Changes {
      var addedSubscribers = Set.empty[Subscriber[T]]
      var addedChildren = Set.empty[T]
      var removedChildren = Set.empty[T]
    }

    private val changes = new TxnLocal[Changes] {
      override protected def initialValue(txn: Txn): Changes = {
        val c = new Changes()
        txn.beforeCommit { implicit txn =>
          for (s <- subscribers() if !c.addedSubscribers.contains(s))
            s(Update(txn, c.addedChildren, c.removedChildren))
          for (s <- c.addedSubscribers)
            s(Update(txn, children(), Set.empty[T]))
        }
        c
      }
    }

    def addSubscriber(s: Subscriber[T])(implicit txn: Txn) {
      changes.get.addedSubscribers += s
      subscribers.transform(_ + s)
    }

    def add(child: T)(implicit txn: Txn) {
      if (!children().contains(child)) {
        val c = changes.get
        if (c.removedChildren.contains(child))
          c.removedChildren -= child
        else
          c.addedChildren += child
        children.transform(_ + child)
      }
    }

    def remove(child: T)(implicit txn: Txn) {
      if (children().contains(child)) {
        val c = changes.get
        if (c.addedChildren.contains(child))
          c.addedChildren -= child
        else
          c.removedChildren += child
        children.transform(_ - child)
      }
    }
  }

  class Sub(name: String) extends Subscriber[String] {
    def apply(u: Update[String]) {
      println(name + ": to be added: " + u.added + ", to be removed: " + u.removed)
    }
  }

  def main(args: Array[String]) {
    val p = new PublishingContainer[String]

    atomic { implicit txn =>
      p.add("1")
      p.addSubscriber(new Sub("s1"))
      p.add("2")
      p.add("3")
      p.addSubscriber(new Sub("s2"))
    }

    atomic { implicit txn =>
      p.add("4")
      p.add("5")
      p.remove("3")
      p.remove("4")
    }

    atomic { implicit txn =>
      p.add("4")
      p.add("5")
      p.addSubscriber(new Sub("s3"))
      p.remove("3")
      p.remove("4")
    }

    println(p.objects.single())
  }
}