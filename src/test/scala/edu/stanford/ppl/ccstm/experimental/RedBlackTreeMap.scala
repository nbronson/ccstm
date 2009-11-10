/* CCSTM - (c) 2009 Stanford University - PPL */

// RedBlackTreeMap

package edu.stanford.ppl.ccstm.experimental


import edu.stanford.ppl.ccstm.impl.MetaHolder
import edu.stanford.ppl.ccstm._


class RedBlackTreeMap[A <% Ordered[A],B] {

  private def BLACK = true
  private def RED = false

  private def root(implicit txn: Txn): RBNode[A,B] = rootRef.get
  private def root_=(v: RBNode[A,B])(implicit txn: Txn) { rootRef.set(v) }
  private val rootRef = Ref[RBNode[A,B]](null)

  //////////////// public interface

  def clear()(implicit txn: Txn) {
    root = null
    // TODO: size = 0
  }

  def containsKey(key: A)(implicit txn: Txn) = null != getNode(key)

  def get(key: A)(implicit txn: Txn): Option[B] = {
    getNode(key) match {
      case null => None
      case n => Some(n.value)
    }
  }

  def put(key: A, value: B)(implicit txn: Txn): Option[B] = {
    if (null == key) throw new NullPointerException

    var t = root
    if (null == t) {
      // easy
      root = new RBNode(key, value, null)
      //TODO: size = 1
      return None
    }

    val k: Ordered[A] = key
    var cmp = 0
    var parent: RBNode[A,B] = null
    do {
      parent = t
      cmp = k.compare(t.key)
      if (cmp < 0) {
        t = t.left
      } else if (cmp > 0) {
        t = t.right
      } else {
        return t.setValue(value)
      }
    } while (null != t)

    val n = new RBNode(key, value, parent)
    if (cmp < 0) {
      parent.left = n
    } else {
      parent.right = n
    }
    fixAfterInsertion(n)
    //TODO: size += 1
    return None
  }

  def remove(key: A)(implicit txn: Txn): Option[B] = {
    val x = getNode(key)
    if (null == x) {
      None
    } else {
      val z = x.value
      deleteNode(x)
      Some(z)
    }
  }
  
  //////////////// internal implementation

  private def getNode(key: A)(implicit txn: Txn): RBNode[A,B] = {
    val k: Ordered[A] = key
    var p = root
    while (null != p) {
      val cmp = k.compare(p.key)
        if (cmp < 0) {
          p = p.left
        } else if (cmp > 0) {
          p = p.right
        } else {
          return p
        }
    }
    return null
  }

  private def successor(x: RBNode[A,B])(implicit txn: Txn) = {
    if (null == x) {
      null
    } else {
      var tmp = x.right
      if (null != tmp) {
        var p = tmp
        tmp = p.left
        while (null != tmp) {
          p = tmp
          tmp = p.left
        }
        p
      } else {
        var p = x.parent
        var ch = tmp
        while (null != p && ch == p.right) {
          ch = p
          p = p.parent
        }
        p
      }
    }
  }

  private def predecessor(x: RBNode[A,B])(implicit txn: Txn) = {
    if (null == x) {
      null
    } else {
      var tmp = x.left
      if (null != tmp) {
        var p = tmp
        tmp = p.right
        while (null != tmp) {
          p = tmp
          tmp = p.right
        }
        p
      } else {
        var p = x.parent
        var ch = tmp
        while (null != p && ch == p.left) {
          ch = p
          p = p.parent
        }
        p
      }
    }
  }


  private def colorOf(p: RBNode[A,B])(implicit txn: Txn) = {
    if (null == p) BLACK else p.color
  }

  private def parentOf(p: RBNode[A,B])(implicit txn: Txn) = {
    if (null == p) null else p.parent
  }

  private def setColor(p: RBNode[A,B], c: Boolean)(implicit txn: Txn) {
    if (null != p && p.color != c) p.color = c
  }

  private def leftOf(p: RBNode[A,B])(implicit txn: Txn) = {
    if (null == p) null else p.left
  }

  private def rightOf(p: RBNode[A,B])(implicit txn: Txn) = {
    if (null == p) null else p.right
  }

  private def rotateLeft(p: RBNode[A,B])(implicit txn: Txn) {
    if (null != p) {
      val r = p.right
      val rl = r.left
      p.right = rl
      if (null != rl) {
        rl.parent = p
      }
      val par = p.parent
      r.parent = par
      if (null == par) {
        root = r
      } else if (par.left == p) {
        par.left = r
      } else {
        par.right = r
      }
      r.left = p
      p.parent = r
    }
  }

  private def rotateRight(p: RBNode[A,B])(implicit txn: Txn) {
    if (null != p) {
      val l = p.left
      val lr = l.right
      p.left = lr
      if (null != lr) {
        lr.parent = p
      }
      val par = p.parent
      l.parent = par
      if (null == par) {
        root = l
      } else if (par.right == p) {
        par.right = l
      } else {
        par.left = l
      }
      l.right = p
      p.parent = l
    }
  }

  private def fixAfterInsertion(x0: RBNode[A,B])(implicit txn: Txn) {
    var x = x0
    x.color = RED

    while (null != x) {
      var xp = x.parent
      if (null == xp || xp.color != RED) {
        // done
        x = null
      } else {
        var xpp = parentOf(xp)
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
              x = xp
              rotateLeft(x)
              xp = parentOf(x)
              xpp = parentOf(xp)
            }
            setColor(xp, BLACK)
            setColor(xpp, RED)
            rotateRight(xpp)
          }
        } else {
          if (colorOf(xppl) == RED) {
            setColor(xp, BLACK)
            setColor(xppl, BLACK)
            setColor(xpp, RED)
            x = xpp
          } else {
            if (x == leftOf(xp)) {
              x = xp
              rotateRight(x)
              xp = parentOf(x)
              xpp = parentOf(xp)
            }
            setColor(xp, BLACK)
            setColor(xpp, RED)
            rotateLeft(xpp)
          }
        }
      }
    }
    setColor(root, BLACK)
  }

  private def deleteNode(x: RBNode[A,B])(implicit txn: Txn) {
    // TODO: size -= 1

    val xp = x.parent
    val xl = x.left
    val xr = x.right

    if (null != xl && null != xr) {
      val s = successor(x)
      val repl = new RBNode(x.color, s.key, s.value, xp, xl, xr)
      if (null != xp) {
        if (x == xp.left) {
          xp.left = repl
        } else {
          assert(x == xp.right)
          xp.right = repl
        }
      } else {
        root = repl
      }
      if (null != xl) {
        xl.parent = repl
      }
      if (null != xr) {
        xr.parent = repl
      }
      deleteNode(s)
      return
    }

    val repl = if (null != xl) xl else xr

    if (null != repl) {
      repl.parent = xp
      if (null == xp) {
        root = repl
      } else if (x == xp.left) {
        xp.left = repl
      } else {
        xp.right = repl
      }

      x.left = null
      x.right = null
      x.parent = null

      if (x.color == BLACK) {
        fixAfterDeletion(repl)
      }
    } else if (null == xp) {
      root = null
    } else {
      if (x.color == BLACK) {
        fixAfterDeletion(x)
      }

      if (null != xp) {
        if (x == xp.left) {
          xp.left = null
        } else {
          assert(x == xp.right)
          xp.right = null
        }
        x.parent = null
      }
    }
  }

  private def fixAfterDeletion(x0: RBNode[A,B])(implicit txn: Txn) {
    var x = x0
    assert(x != null || root == null)
    while (x != root && colorOf(x) == BLACK) {
      var xp = parentOf(x)
      if (x == leftOf(xp)) {
        var sib = rightOf(xp)

        if (colorOf(sib) == RED) {
          sib.color = BLACK
          setColor(xp, RED)
          rotateLeft(xp)
          xp = parentOf(x)
          sib = rightOf(xp)
        }

        val sl = leftOf(sib)
        var sr = rightOf(sib)
        val src = colorOf(sr)
        if (colorOf(sl) == BLACK && src == BLACK) {
          setColor(sib, RED)
          x = xp
          assert(x != null || root == null)
        } else {
          if (src == BLACK) {
            setColor(sl, BLACK)
            setColor(sib, RED)
            rotateRight(sib)
            xp = parentOf(x)
            sib = rightOf(xp)
            sr = rightOf(sib)
          }
          setColor(sib, colorOf(xp))
          setColor(xp, BLACK)
          setColor(sr, BLACK)
          rotateLeft(xp)
          x = root
          assert(x != null || root == null)
        }
      } else {
        var sib = leftOf(xp)

        if (colorOf(sib) == RED) {
          sib.color = BLACK
          setColor(xp, RED)
          rotateRight(xp)
          xp = parentOf(x)
          sib = leftOf(xp)
        }

        val sr = rightOf(sib)
        var sl = leftOf(sib)
        val slc = colorOf(sl)
        if (colorOf(sr) == BLACK && slc == BLACK) {
          setColor(sib, RED)
          x = xp
          assert(x != null || root == null)
        } else {
          if (slc == BLACK) {
            setColor(sr, BLACK)
            setColor(sib, RED)
            rotateLeft(sib)
            xp = parentOf(x)
            sib = leftOf(xp)
            sl = leftOf(sib)
          }
          setColor(sib, colorOf(xp))
          setColor(xp, BLACK)
          setColor(sl, BLACK)
          rotateRight(xp)
          x = root
          assert(x != null || root == null)
        }
      }
    }

    setColor(x, BLACK)
  }
}


private class RBNode[A,B](color0: Boolean, val key: A, value0: B, parent0: RBNode[A,B], left0: RBNode[A,B], right0: RBNode[A,B]) extends MetaHolder {
  import RBNode._

  def this(key0: A, value0: B, parent0: RBNode[A,B]) = this(true, key0, value0, parent0, null, null)

  @volatile private var _color: Boolean = color0
  @volatile private var _value: B = value0
  @volatile private var _parent: RBNode[A,B] = parent0
  @volatile private var _left: RBNode[A,B] = left0
  @volatile private var _right: RBNode[A,B] = right0

  def color(implicit txn: Txn): Boolean = colorRef.get
  def color_=(v: Boolean)(implicit txn: Txn) { colorRef.set(v) }
  def colorRef = Color(this)
  
  def value(implicit txn: Txn): B = valueRef.get
  def value_=(v: B)(implicit txn: Txn) { valueRef.set(v) }
  def valueRef = Value[B](this)

  def setValue(v: B)(implicit txn: Txn): Option[B] = {
    val prev = value
    value = v
    Some(prev)
  }

  def parent(implicit txn: Txn): RBNode[A,B] = parentRef.get
  def parent_=(v: RBNode[A,B])(implicit txn: Txn) { parentRef.set(v) }
  def parentRef = Parent[A,B](this)

  def left(implicit txn: Txn): RBNode[A,B] = leftRef.get
  def left_=(v: RBNode[A,B])(implicit txn: Txn) { leftRef.set(v) }
  def leftRef = Left[A,B](this)

  def right(implicit txn: Txn): RBNode[A,B] = rightRef.get
  def right_=(v: RBNode[A,B])(implicit txn: Txn) { rightRef.set(v) }
  def rightRef = Right[A,B](this)
}

private object RBNode {
  val Color = new TxnFieldUpdater[RBNode[_,_],Boolean](classOf[RBNode[_,_]], "color") {
    protected def getField(instance: RBNode[_,_]): Boolean = instance._color
    protected def setField(instance: RBNode[_,_], v: Boolean) { instance._color = v }
  }

//  def Value[B] = new TxnFieldUpdater[RBNode[_,B],B](classOf[RBNode[_,B]], "value") {
//    protected def getField(instance: RBNode[_,B]): B = instance._value
//    protected def setField(instance: RBNode[_,B], v: B) { instance._value = v }
//  }
  private val UntypedValue = new TxnFieldUpdater[RBNode[_,Any],Any](classOf[RBNode[_,Any]], "value") {
    protected def getField(instance: RBNode[_,Any]): Any = instance._value
    protected def setField(instance: RBNode[_,Any], v: Any) { instance._value = v }
  }
  def Value[B] = UntypedValue.asInstanceOf[TxnFieldUpdater[RBNode[_,B],B]]
  
//  def Parent[A,B] = new TxnFieldUpdater[RBNode[A,B],RBNode[A,B]](classOf[RBNode[A,B]], "parent") {
//    protected def getField(instance: RBNode[A,B]): RBNode[A,B] = instance._parent
//    protected def setField(instance: RBNode[A,B], v: RBNode[A,B]) { instance._parent = v }
//  }
    private val UntypedParent = new TxnFieldUpdater[RBNode[Any,Any],RBNode[Any,Any]](classOf[RBNode[Any,Any]], "parent") {
      protected def getField(instance: RBNode[Any,Any]): RBNode[Any,Any] = instance._parent
      protected def setField(instance: RBNode[Any,Any], v: RBNode[Any,Any]) { instance._parent = v }
    }
    def Parent[A,B] = UntypedParent.asInstanceOf[TxnFieldUpdater[RBNode[A,B],RBNode[A,B]]]

//  def Left[A,B] = new TxnFieldUpdater[RBNode[A,B],RBNode[A,B]](classOf[RBNode[A,B]], "left") {
//    protected def getField(instance: RBNode[A,B]): RBNode[A,B] = instance._left
//    protected def setField(instance: RBNode[A,B], v: RBNode[A,B]) { instance._left = v }
//  }
  private val UntypedLeft = new TxnFieldUpdater[RBNode[Any,Any],RBNode[Any,Any]](classOf[RBNode[Any,Any]], "left") {
    protected def getField(instance: RBNode[Any,Any]): RBNode[Any,Any] = instance._left
    protected def setField(instance: RBNode[Any,Any], v: RBNode[Any,Any]) { instance._left = v }
  }
  def Left[A,B] = UntypedLeft.asInstanceOf[TxnFieldUpdater[RBNode[A,B],RBNode[A,B]]]

//  def Right[A,B] = new TxnFieldUpdater[RBNode[A,B],RBNode[A,B]](classOf[RBNode[A,B]], "right") {
//    protected def getField(instance: RBNode[A,B]): RBNode[A,B] = instance._right
//    protected def setField(instance: RBNode[A,B], v: RBNode[A,B]) { instance._right = v }
//  }
  private val UntypedRight = new TxnFieldUpdater[RBNode[Any,Any],RBNode[Any,Any]](classOf[RBNode[Any,Any]], "right") {
    protected def getField(instance: RBNode[Any,Any]): RBNode[Any,Any] = instance._right
    protected def setField(instance: RBNode[Any,Any], v: RBNode[Any,Any]) { instance._right = v }
  }
  def Right[A,B] = UntypedRight.asInstanceOf[TxnFieldUpdater[RBNode[A,B],RBNode[A,B]]]
}