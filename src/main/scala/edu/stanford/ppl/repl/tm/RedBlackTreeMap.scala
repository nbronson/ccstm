/* CCSTM - (c) 2009 Stanford University - PPL */

// RedBlackTreeMap

package edu.stanford.ppl.repl.tm


import edu.stanford.ppl.ccstm.impl.MetaHolder
import edu.stanford.ppl.ccstm._


object RedBlackTreeMap {
  private val BLACK = true
  private val RED = false
}

class RedBlackTreeMap[A,B] {
  import RedBlackTreeMap._

  private val root = Ref[Node[A,B]](null)

  private def successor(x: Node[A,B])(implicit txn: Txn) = {
    if (null == x) {
      null
    } else {
      var tmp = x.right?;
      if (null != tmp) {
        var p = tmp
        tmp = p.left?;
        while (null != tmp) {
          p = tmp
          tmp = p.left?
        }
        p
      } else {
        var p = x.parent?;
        var ch = tmp
        while (null != p && ch == p.right.?) {
          ch = p
          p = p.parent?;
        }
        p
      }
    }
  }

  private def predecessor(x: Node[A,B])(implicit txn: Txn) = {
    if (null == x) {
      null
    } else {
      var tmp = x.left?;
      if (null != tmp) {
        var p = tmp
        tmp = p.right?;
        while (null != tmp) {
          p = tmp
          tmp = p.right?
        }
        p
      } else {
        var p = x.parent?;
        var ch = tmp
        while (null != p && ch == p.left.?) {
          ch = p
          p = p.parent?;
        }
        p
      }
    }
  }


  private def colorOf(p: Node[A,B])(implicit txn: Txn) = {
    if (null == p) BLACK else p.color?
  }

  private def parentOf(p: Node[A,B])(implicit txn: Txn) = {
    if (null == p) null else p.parent?
  }

  private def setColor(p: Node[A,B], c: Boolean)(implicit txn: Txn) {
    if (null != p) p.color := c
  }

  private def leftOf(p: Node[A,B])(implicit txn: Txn) = {
    if (null == p) null else p.left?
  }

  private def rightOf(p: Node[A,B])(implicit txn: Txn) = {
    if (null == p) null else p.right?
  }

  private def rotateLeft(p: Node[A,B])(implicit txn: Txn) {
    if (null != p) {
      val r = p.right?;
      val rl = r.left?;
      p.right := rl
      if (null != rl) {
        rl.parent := p
      }
      val par = p.parent?;
      r.parent := par
      if (null == par) {
        root := r
      } else if (par.left.? == p) {
        par.left := r
      } else {
        par.right := r
      }
      r.left := p
      p.parent := r
    }
  }

  private def rotateRight(p: Node[A,B])(implicit txn: Txn) {
    if (null != p) {
      val l = p.left?;
      val lr = l.right?;
      p.left := lr
      if (null != lr) {
        lr.parent := p
      }
      val par = p.parent?;
      l.parent := par
      if (null == par) {
        root := l
      } else if (par.right.? == p) {
        par.right := l
      } else {
        par.left := l
      }
      l.right := p
      p.parent := l
    }
  }

  private def fixAfterInsertion(x0: Node[A,B])(implicit txn: Txn) {
    var x = x0
    x.color := RED

    while (null != x) {
      val xp = x.parent?;
      if (null == xp || xp.color.? == RED) {
        // done
        x = null
      } else {
        val xpp = parentOf(xp)
        val xppl = leftOf(xpp)
        val xppr = rightOf(xpp)
        if (xp == xppl) {
          if (colorOf(xppr) == RED) {
            setColor(xp, BLACK)
            setColor(xppr, BLACK)
            setColor(xpp, RED)
            x = xpp
          } else {
            if (x == rightOf(xp)) {
              rotateLeft(xp)
              setColor(xpp, BLACK)
              val xppp = parentOf(xpp)
              setColor(xppp, RED)
              rotateRight(xppp)
              x = xp
            } else {
              setColor(xp, BLACK)
              setColor(xpp, RED)
              rotateRight(xpp)
            }
          }
        } else {
          if (colorOf(xppl) == RED) {
            setColor(xp, BLACK)
            setColor(xppl, BLACK)
            setColor(xpp, RED)
            x = xpp
          } else {
            if (x == leftOf(xp)) {
              rotateRight(xp)
              setColor(xpp, BLACK)
              val xppp = parentOf(xpp)
              setColor(xppp, RED)
              rotateLeft(xppp)
              x = xp
            } else {
              setColor(parentOf(x), BLACK)
              setColor(parentOf(parentOf(x)), RED)
              rotateLeft(parentOf(parentOf(x)))
            }
          }
        }
      }
    }
    root.?.color := BLACK
  }

//  /**
//   * Delete node p, and then rebalance the tree.
//   */
//  private void deleteEntry(Node[A,B] p) {
//      modCount++;
//      size--;
//
//      // If strictly internal, copy successor's element to p and then make p
//      // point to successor.
//      if (p.left != null && p.right != null) {
//          Node[A,B] s = successor (p);
//          p.key = s.key;
//          p.value = s.value;
//          p = s;
//      } // p has 2 children
//
//      // Start fixup at replacement node, if it exists.
//      Node[A,B] replacement = (p.left != null ? p.left : p.right);
//
//      if (replacement != null) {
//          // Link replacement to parent
//          replacement.parent = p.parent;
//          if (p.parent == null)
//              root = replacement;
//          else if (p == p.parent.left)
//              p.parent.left  = replacement;
//          else
//              p.parent.right = replacement;
//
//          // Null out links so they are OK to use by fixAfterDeletion.
//          p.left = p.right = p.parent = null;
//
//          // Fix replacement
//          if (p.color == BLACK)
//              fixAfterDeletion(replacement);
//      } else if (p.parent == null) { // return if we are the only node.
//          root = null;
//      } else { //  No children. Use self as phantom replacement and unlink.
//          if (p.color == BLACK)
//              fixAfterDeletion(p);
//
//          if (p.parent != null) {
//              if (p == p.parent.left)
//                  p.parent.left = null;
//              else if (p == p.parent.right)
//                  p.parent.right = null;
//              p.parent = null;
//          }
//      }
//  }
//
//  /** From CLR */
//  private void fixAfterDeletion(Node[A,B] x) {
//      while (x != root && colorOf(x) == BLACK) {
//          if (x == leftOf(parentOf(x))) {
//              Node[A,B] sib = rightOf(parentOf(x));
//
//              if (colorOf(sib) == RED) {
//                  setColor(sib, BLACK);
//                  setColor(parentOf(x), RED);
//                  rotateLeft(parentOf(x));
//                  sib = rightOf(parentOf(x));
//              }
//
//              if (colorOf(leftOf(sib))  == BLACK &&
//                  colorOf(rightOf(sib)) == BLACK) {
//                  setColor(sib, RED);
//                  x = parentOf(x);
//              } else {
//                  if (colorOf(rightOf(sib)) == BLACK) {
//                      setColor(leftOf(sib), BLACK);
//                      setColor(sib, RED);
//                      rotateRight(sib);
//                      sib = rightOf(parentOf(x));
//                  }
//                  setColor(sib, colorOf(parentOf(x)));
//                  setColor(parentOf(x), BLACK);
//                  setColor(rightOf(sib), BLACK);
//                  rotateLeft(parentOf(x));
//                  x = root;
//              }
//          } else { // symmetric
//              Node[A,B] sib = leftOf(parentOf(x));
//
//              if (colorOf(sib) == RED) {
//                  setColor(sib, BLACK);
//                  setColor(parentOf(x), RED);
//                  rotateRight(parentOf(x));
//                  sib = leftOf(parentOf(x));
//              }
//
//              if (colorOf(rightOf(sib)) == BLACK &&
//                  colorOf(leftOf(sib)) == BLACK) {
//                  setColor(sib, RED);
//                  x = parentOf(x);
//              } else {
//                  if (colorOf(leftOf(sib)) == BLACK) {
//                      setColor(rightOf(sib), BLACK);
//                      setColor(sib, RED);
//                      rotateLeft(sib);
//                      sib = leftOf(parentOf(x));
//                  }
//                  setColor(sib, colorOf(parentOf(x)));
//                  setColor(parentOf(x), BLACK);
//                  setColor(leftOf(sib), BLACK);
//                  rotateRight(parentOf(x));
//                  x = root;
//              }
//          }
//      }
//
//      setColor(x, BLACK);
//  }
//

}


private class Node[A,B](val key: A, initialValue: B) extends MetaHolder {
  import Node._

  @volatile private var _color: Boolean = true
  @volatile private var _value: B = initialValue
  @volatile private var _parent: Node[A,B] = null
  @volatile private var _left: Node[A,B] = null
  @volatile private var _right: Node[A,B] = null

  def color = Color(this)
  def value = Value[B](this)
  def parent = Parent[A,B](this)
  def left = Left[A,B](this)
  def right = Right[A,B](this)
}

private object Node {
  val Color = new TxnFieldUpdater[Node[_,_],Boolean](classOf[Node[_,_]], "color") {
    protected def getField(instance: Node[_,_]): Boolean = instance._color
    protected def setField(instance: Node[_,_], v: Boolean) { instance._color = v }
  }

//  def Value[B] = new TxnFieldUpdater[Node[_,B],B](classOf[Node[_,B]], "value") {
//    protected def getField(instance: Node[_,B]): B = instance._value
//    protected def setField(instance: Node[_,B], v: B) { instance._value = v }
//  }
  private val UntypedValue = new TxnFieldUpdater[Node[_,Any],Any](classOf[Node[_,Any]], "value") {
    protected def getField(instance: Node[_,Any]): Any = instance._value
    protected def setField(instance: Node[_,Any], v: Any) { instance._value = v }
  }
  def Value[B] = UntypedValue.asInstanceOf[TxnFieldUpdater[Node[_,B],B]]
  
//  def Parent[A,B] = new TxnFieldUpdater[Node[A,B],Node[A,B]](classOf[Node[A,B]], "parent") {
//    protected def getField(instance: Node[A,B]): Node[A,B] = instance._parent
//    protected def setField(instance: Node[A,B], v: Node[A,B]) { instance._parent = v }
//  }
    private val UntypedParent = new TxnFieldUpdater[Node[Any,Any],Node[Any,Any]](classOf[Node[Any,Any]], "parent") {
      protected def getField(instance: Node[Any,Any]): Node[Any,Any] = instance._parent
      protected def setField(instance: Node[Any,Any], v: Node[Any,Any]) { instance._parent = v }
    }
    def Parent[A,B] = UntypedParent.asInstanceOf[TxnFieldUpdater[Node[A,B],Node[A,B]]]

//  def Left[A,B] = new TxnFieldUpdater[Node[A,B],Node[A,B]](classOf[Node[A,B]], "left") {
//    protected def getField(instance: Node[A,B]): Node[A,B] = instance._left
//    protected def setField(instance: Node[A,B], v: Node[A,B]) { instance._left = v }
//  }
  private val UntypedLeft = new TxnFieldUpdater[Node[Any,Any],Node[Any,Any]](classOf[Node[Any,Any]], "left") {
    protected def getField(instance: Node[Any,Any]): Node[Any,Any] = instance._left
    protected def setField(instance: Node[Any,Any], v: Node[Any,Any]) { instance._left = v }
  }
  def Left[A,B] = UntypedLeft.asInstanceOf[TxnFieldUpdater[Node[A,B],Node[A,B]]]

//  def Right[A,B] = new TxnFieldUpdater[Node[A,B],Node[A,B]](classOf[Node[A,B]], "right") {
//    protected def getField(instance: Node[A,B]): Node[A,B] = instance._right
//    protected def setField(instance: Node[A,B], v: Node[A,B]) { instance._right = v }
//  }
  private val UntypedRight = new TxnFieldUpdater[Node[Any,Any],Node[Any,Any]](classOf[Node[Any,Any]], "right") {
    protected def getField(instance: Node[Any,Any]): Node[Any,Any] = instance._right
    protected def setField(instance: Node[Any,Any], v: Node[Any,Any]) { instance._right = v }
  }
  def Right[A,B] = UntypedRight.asInstanceOf[TxnFieldUpdater[Node[A,B],Node[A,B]]]
}
