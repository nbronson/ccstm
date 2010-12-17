/* scala-stm - (c) 2009-2010, Stanford University, PPL */

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.experimental._

/** `TxnHashTrie` implements a transactional mutable hash trie using Ref-s,
 *  with lazy cloning to allow efficient snapshots.  Fundamental operations are
 *  provided for hash tries representing either sets or maps, both of which are
 *  represented as a Ref.Bound[Node[A, B]].  If the initial empty leaf is
 *  `emptySetValue` then no values can be stored in the hash trie, and
 *  operations that take or return values will expect and produce null.
 *
 *  @author Nathan Bronson
 */
object TxnHashTrie {

  private def LogBF = 4
  private def BF = 16

  // It would seem that the leaf copying is inefficient when compared to a tree
  // that allows more sharing, but a back-of-the-envelope calculation indicates
  // that the crossover point for the total bytes allocates to construct an
  // immutable node holding N elements is about 12.  Even at N=32 the total
  // bytes required by this Leaf implementation is only about 2/3 more than an
  // ideal balanced tree, and those bytes are accessed in a more cache friendly
  // fashion.
  private def MaxLeafCapacity = 14

  private def keyHash[A](key: A): Int = if (null == key) 0 else mixBits(key.hashCode)

  private def mixBits(h: Int) = {
    // make sure any bit change results in a change in the bottom LogBF bits
    val s = LogBF
    val x = h ^ (h >>> (s * 3)) ^ (h >>> (s * 6))
    x ^ (x >>> s) ^ (x >>> (s * 2))
  }

  private def indexFor(shift: Int, hash: Int) = (hash >>> shift) & (BF - 1)

  //////// shared instances
  
  private val emptyLeaf = new Leaf[Any, Unit](new Array[Int](0), new Array[AnyRef](0))

  //////// publicly-visible stuff

  sealed abstract class Node[A, B] {
    def cappedSize(cap: Int): Int
    def setForeach[U](f: A => U)
    def mapForeach[U](f: ((A, B)) => U)
    def setIterator: Iterator[A]
    def mapIterator: Iterator[(A, B)]
  }

  sealed trait BuildingNode[A, B] {
    def endBuild: Node[A, B]
  }

  final class Leaf[A, B](val hashes: Array[Int],
                         val kvs: Array[AnyRef]) extends Node[A, B] with BuildingNode[A, B] {

    def getKey(i: Int): A = kvs(2 * i).asInstanceOf[A]
    def getValue(i: Int): B = kvs(2 * i + 1).asInstanceOf[B]
    def setKey(i: Int, k: A) { kvs(2 * i) = k.asInstanceOf[AnyRef] }
    def setValue(i: Int, v: B) { kvs(2 * i + 1) = v.asInstanceOf[AnyRef] }

    def endBuild = this

    def cappedSize(cap: Int): Int = hashes.length

    def contains(hash: Int, key: A): Boolean = find(hash, key) >= 0

    def mapGet(hash: Int, key: A): Option[B] = {
      val i = find(hash, key)
      if (i < 0)
        None
      else
        Some(getValue(i))
    }

    def get(i: Int): Option[B] = {
      if (i < 0)
        None
      else
        Some(getValue(i))
    }

    def find(hash: Int, key: A): Int = {
      var i = hashes.length
      while (i > 0) {
        i -= 1
        val h = hashes(i)
        if (h == hash && keyEqual(key.asInstanceOf[AnyRef], kvs(2 * i)))
          return i
        if (h < hash)
          return ~(i + 1)
      }
      return ~0
    }

    private def keyEqual(lhs: AnyRef, rhs: AnyRef): Boolean = {
      if (lhs eq rhs)
        true
      else if (null == lhs)
        false
      else
        lhs.equals(rhs)
    }

    def noChange[B](i: Int, value: B): Boolean = {
      i >= 0 && (kvs(2 * i + 1) eq value.asInstanceOf[AnyRef])
    }

    def withPut(gen: Long, shift: Int, hash: Int, key: A, value: B, i: Int): Node[A, B] = {
      if (i < 0)
        withInsert(~i, hash, key, value).splitIfNeeded(gen, shift)
      else
        withUpdate(i, value)
    }

    def withBuildingPut(shift: Int, hash: Int, key: A, value: B, i: Int): BuildingNode[A, B] = {
      if (i < 0)
        withInsert(~i, hash, key, value).buildingSplitIfNeeded(shift)
      else
        withUpdate(i, value)
    }

    private def withUpdate(i: Int, value: B): Leaf[A, B] = {
      // reuse hashes
      val nkvs = new Array[AnyRef](kvs.length)
      System.arraycopy(kvs, 0, nkvs, 0, kvs.length)
      nkvs(2 * i + 1) = value.asInstanceOf[AnyRef]
      new Leaf[A, B](hashes, nkvs)
    }

    private def withInsert(i: Int, hash: Int, key: A, value: B): Leaf[A, B] = {
      val z = newLeaf(hashes.length + 1)
      val j = hashes.length - i
      
      System.arraycopy(hashes, 0, z.hashes, 0, i)
      System.arraycopy(hashes, i, z.hashes, i + 1, j)
      z.hashes(i) = hash

      System.arraycopy(kvs, 0, z.kvs, 0, 2 * i)
      System.arraycopy(kvs, 2 * i, z.kvs, 2 * i + 2, 2 * j)
      z.setKey(i, key)
      z.setValue(i, value)

      z
    }

    def withRemove(i: Int): Leaf[A, B] = {
      if (i < 0)
        this
      else {
        val z = newLeaf(hashes.length - 1)
        if (z.hashes.length > 0) {
          val j = z.hashes.length - i

          System.arraycopy(hashes, 0, z.hashes, 0, i)
          System.arraycopy(hashes, i + 1, z.hashes, i, j)

          System.arraycopy(kvs, 0, z.kvs, 0, 2 * i)
          System.arraycopy(kvs, 2 * i + 2, z.kvs, 2 * i, 2 * j)
        }
        z
      }
    }

    def splitIfNeeded(gen: Long, shift: Int): Node[A, B] = if (!shouldSplit) this else split(gen, shift)

    def buildingSplitIfNeeded(shift: Int): BuildingNode[A, B] = if (!shouldSplit) this else buildingSplit(shift)

    def shouldSplit: Boolean = {
      // if the hash function is bad we might be oversize but unsplittable
      hashes.length > MaxLeafCapacity && hashes(hashes.length - 1) != hashes(0)
    }

    def split(gen: Long, shift: Int): Branch[A, B] = {
      val children = new Array[Node[A, B]](BF)
      splitInto(shift, children)
      val refs = new Array[Ref.Bound[Node[A, B]]](BF)
      var i = 0
      while (i < BF) {
        refs(i) = Ref(children(i)).nonTxn
        i += 1
      }
      new Branch[A, B](gen, false, refs)      
    }

    def buildingSplit(shift: Int): BuildingBranch[A, B] = {
      val children = new Array[BuildingNode[A, B]](BF)
      splitInto(shift, children)
      new BuildingBranch[A, B](children)
    }

    private def splitInto[L >: Leaf[A, B]](shift: Int, children: Array[L]) {
      val sizes = new Array[Int](BF)
      var i = 0
      while (i < hashes.length) {
        sizes(indexFor(shift, hashes(i))) += 1
        i += 1
      }
      i = 0
      while (i < BF) {
        children(i) = newLeaf(sizes(i))
        i += 1
      }
      i = hashes.length - 1
      while (i >= 0) {
        val slot = indexFor(shift, hashes(i))
        sizes(slot) -= 1
        val pos = sizes(slot)
        val dst = children(slot).asInstanceOf[Leaf[A, B]]
        dst.hashes(pos) = hashes(i)
        dst.kvs(2 * pos) = kvs(2 * i)
        dst.kvs(2 * pos + 1) = kvs(2 * i + 1)

        // If the hashes were very poorly distributed one leaf might get
        // everything.  We could resplit now, but it doesn't seem to be worth
        // it.  If we wait until the next insert we can never get more than
        // 32 / LogBF extra.
        //// if (pos == 0 && dst.shouldSplit)
        ////  children(slot).value = dst.split(gen, shift + LogBF)
        i -= 1
      }
    }

    private def newLeaf(n: Int): Leaf[A, B] = {
      if (n == 0) {
        emptyLeaf.asInstanceOf[Leaf[A, B]]
      } else {
        new Leaf[A, B](new Array[Int](n), new Array[AnyRef](2 * n))
      }
    }

    def setForeach[U](f: A => U) {
      var i = 0
      while (i < hashes.length) {
        f(getKey(i))
        i += 1
      }
    }

    def mapForeach[U](f: ((A, B)) => U) {
      var i = 0
      while (i < hashes.length) {
        f((getKey(i), getValue(i)))
        i += 1
      }
    }

    def setIterator: Iterator[A] = new Iterator[A] {
      var pos = 0
      def hasNext = pos < hashes.length
      def next: A = { val z = getKey(pos) ; pos += 1 ; z }
    }

    def mapIterator: Iterator[(A, B)] = new Iterator[(A,B)] {
      var pos = 0
      def hasNext = pos < hashes.length
      def next: (A, B) = { val z = (getKey(pos), getValue(pos)) ; pos += 1 ; z }
    }
  }

  class BuildingBranch[A, B](val children: Array[BuildingNode[A, B]]) extends BuildingNode[A, B] {
    def endBuild: Node[A, B] = {
      val refs = new Array[Ref.Bound[Node[A, B]]](BF)
      var i = 0
      while (i < BF) {
        refs(i) = Ref(children(i).endBuild).nonTxn
        i += 1
      }
      new Branch(0L, false, refs)
    }
  }

  class Branch[A, B](val gen: Long, val frozen: Boolean, val children: Array[Ref.Bound[Node[A, B]]]) extends Node[A, B] {

    // size may only be called on a frozen branch, so we can cache the result
    private var _cachedSize = -1

    def cappedSize(cap: Int): Int = {
      val n0 = _cachedSize
      if (n0 >= 0) {
        n0
      } else {
        var n = 0
        var i = 0
        while (i < BF && n < cap) {
          n += children(i).get.cappedSize(cap - n)
          i += 1
        }
        if (n < cap)
          _cachedSize = n // everybody tried their hardest
        n
      }
    }

    def withFreeze: Branch[A, B] = new Branch(gen, true, children)

    def clone(newGen: Long): Branch[A, B] = {
      val cc = new Array[Ref.Bound[Node[A, B]]](BF)
      var i = 0
      while (i < cc.length) {
        cc(i) = Ref(children(i).get).nonTxn
        i += 1
      }
      new Branch[A, B](newGen, false, cc)
    }

    def setForeach[U](f: A => U) {
      var i = 0
      while (i < BF) {
        children(i).get.setForeach(f)
        i += 1
      }
    }

    def mapForeach[U](f: ((A, B)) => U) {
      var i = 0
      while (i < BF) {
        children(i).get.mapForeach(f)
        i += 1
      }
    }

    private abstract class Iter[Z] extends Iterator[Z] {

      def childIter(c: Node[A, B]): Iterator[Z]

      private var pos = -1
      private var iter: Iterator[Z] = null
      advance()

      /* @tailrec */ private def advance(): Boolean = {
        if (pos == BF - 1) {
          iter = null
          false
        } else {
          pos += 1
          val c = children(pos).get
          if (c eq emptyLeaf)
            advance() // keep looking, nothing is here
          else {
            iter = childIter(c)
            iter.hasNext || advance() // keep looking if we got a dud
          }
        }
      }

      def hasNext = null != iter && iter.hasNext

      def next: Z = {
        val z = iter.next
        if (!iter.hasNext)
          advance()
        z
      }
    }

    def setIterator: Iterator[A] = new Iter[A] {
      def childIter(c: Node[A, B]) = c.setIterator
    }

    def mapIterator: Iterator[(A, B)] = new Iter[(A,B)] {
      def childIter(c: Node[A, B]) = c.mapIterator
    }
  }

  //////////////// construction

  def emptyMapNode[A, B]: Node[A, B] = emptyLeaf.asInstanceOf[Node[A, B]]

  def emptyMapBuildingNode[A, B]: BuildingNode[A, B] = emptyLeaf.asInstanceOf[BuildingNode[A, B]]

  def buildingPut[A, B](root: BuildingNode[A, B], k: A, v: B): BuildingNode[A, B] = buildingPut(root, 0, keyHash(k), k, v)

  private def buildingPut[A, B](current: BuildingNode[A, B], shift: Int, hash: Int, key: A, value: B): BuildingNode[A, B] = {
    current match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (leaf.noChange(i, value)) leaf else leaf.withBuildingPut(shift, hash, key, value, i)
      }
      case branch: BuildingBranch[A, B] => {
        val i = indexFor(shift, hash)
        branch.children(i) = buildingPut(branch.children(i), shift + LogBF, hash, key, value)
        branch
      }
    }
  }

  //////////////// hash trie operations on escaped Ref.Bound

  def frozenRoot[A, B](root: Ref.Bound[Node[A, B]]): Node[A, B] = {
    !root match {
      case leaf: Leaf[A, B] => leaf // leaf is already immutable
      case branch: Branch[A, B] if branch.frozen => branch
      case branch: Branch[A, B] => {
        // If this CAS fails it means someone else already installed a frozen
        // branch, and we can benefit from their work.
        val b = branch.withFreeze
        root.compareAndSetIdentity(branch, b)
        b
      }
    }
  }

  def clone[A, B](root: Ref.Bound[Node[A, B]]): Ref.Bound[Node[A, B]] = Ref(frozenRoot(root)).nonTxn

  def sizeGE[A, B](root: Ref.Bound[Node[A, B]], n: Int): Boolean = frozenRoot(root).cappedSize(n) >= n
  
  def size[A, B](root: Ref.Bound[Node[A, B]]): Int = frozenRoot(root).cappedSize(Int.MaxValue)

  def contains[A, B](root: Ref.Bound[Node[A, B]], key: A): Boolean = contains(root, 0, keyHash(key), key)

  /* @tailrec */ private def contains[A, B](n: Ref.Bound[Node[A, B]], shift: Int, hash: Int, key: A): Boolean = {
    !n match {
      case leaf: Leaf[A, B] => leaf.contains(hash, key)
      case branch: Branch[A, B] => contains(branch.children(indexFor(shift, hash)), shift + LogBF, hash, key)
    }
  }

  def getOrThrow[A, B](root: Ref.Bound[Node[A, B]], key: A): B = getOrThrow(root, 0, keyHash(key), key)

  /* @tailrec */ private def getOrThrow[A, B](n: Ref.Bound[Node[A, B]], shift: Int, hash: Int, key: A): B = {
    !n match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (i < 0)
          throw new NoSuchElementException("key not found: " + key)
        leaf.getValue(i)
      }
      case branch: Branch[A, B] => getOrThrow(branch.children(indexFor(shift, hash)), shift + LogBF, hash, key)
    }
  }

  def get[A, B](root: Ref.Bound[Node[A, B]], key: A): Option[B] = get(root, 0, keyHash(key), key)

  /* @tailrec */ private def get[A, B](n: Ref.Bound[Node[A, B]], shift: Int, hash: Int, key: A): Option[B] = {
    !n match {
      case leaf: Leaf[A, B] => leaf.mapGet(hash, key)
      case branch: Branch[A, B] => get(branch.children(indexFor(shift, hash)), shift + LogBF, hash, key)
    }
  }

  def put[A, B](root: Ref.Bound[Node[A, B]], key: A, value: B): Option[B] = rootPut(root, keyHash(key), key, value, 0)

  /* @tailrec */ private def rootPut[A, B](root: Ref.Bound[Node[A, B]], hash: Int, key: A, value: B, failures: Int): Option[B] = {
    if (failures < 10) {
      !root match {
        case leaf: Leaf[A, B] => {
          val i = leaf.find(hash, key)
          if (leaf.noChange(i, value) || root.compareAndSetIdentity(leaf, leaf.withPut(0L, 0, hash, key, value, i)))
            leaf.get(i) // success, read from old leaf
          else
            rootPut(root, hash, key, value, failures + 1)
        }
        case branch: Branch[A, B] => {
          val b = if (!branch.frozen) branch else unshare(branch.gen + 1, root, branch)
          if (null != b)
            childPut(root, b, b.children(indexFor(0, hash)), LogBF, hash, key, value, 0)
          else
            rootPut(root, hash, key, value, failures + 1)
        }
      }
    } else
      failingPut(root, hash, key, value)
  }

  private def unshare[A, B](rootGen: Long, current: Ref.Bound[Node[A, B]], branch: Branch[A, B]): Branch[A, B] = {
    val b = branch.clone(rootGen)
    if (current.compareAndSetIdentity(branch, b)) b else null
  }

  private def failingPut[A, B](root: Ref.Bound[Node[A, B]], hash: Int, key: A, value: B): Option[B] = {
    // running in a transaction guarantees that CAS won't fail
    STM.atomic { txn => rootPut(root.unbind, hash, key, value)(txn) }
  }

  /* @tailrec */ private def childPut[A, B](root: Ref.Bound[Node[A, B]],
                                      rootNode: Branch[A, B],
                                      current: Ref.Bound[Node[A, B]],
                                      shift: Int,
                                      hash: Int,
                                      key: A,
                                      value: B,
                                      failures: Int): Option[B] = {
    if (failures < 10) {
      !current match {
        case leaf: Leaf[A, B] => {
          val i = leaf.find(hash, key)
          if (leaf.noChange(i, value) || STM.ccasi(
                  root.unbind, rootNode, current.unbind,
                  leaf, leaf.withPut(rootNode.gen, shift, hash, key, value, i)))
            leaf.get(i) // success
          else if ((!root) ne rootNode)
            failingPut(root, hash, key, value) // root retry
          else
            childPut(root, rootNode, current, shift, hash, key, value, failures + 1) // local retry
        }
        case branch: Branch[A, B] => {
          val b = if (branch.gen == rootNode.gen) branch else unshare(rootNode.gen, current, branch)
          if (null != b)
            childPut(root, rootNode, b.children(indexFor(shift, hash)), shift + LogBF, hash, key, value, failures)
          else
            childPut(root, rootNode, current, shift, hash, key, value, failures + 1) // failure, try again
        }
      }
    } else
      failingPut(root, hash, key, value)
  }

  def remove[A, B](root: Ref.Bound[Node[A, B]], key: A): Option[B] = rootRemove(root, keyHash(key), key, 0)

  /* @tailrec */ private def rootRemove[A, B](root: Ref.Bound[Node[A, B]], hash: Int, key: A, failures: Int): Option[B] = {
    if (failures < 10) {
      !root match {
        case leaf: Leaf[A, B] => {
          val i = leaf.find(hash, key)
          if (i < 0 || root.compareAndSetIdentity(leaf, leaf.withRemove(i)))
            leaf.get(i) // success, read from old leaf
          else
            rootRemove(root, hash, key, failures + 1)
        }
        case branch: Branch[A, B] => {
          val i = indexFor(0, hash)
          if (branch.frozen && !contains(branch.children(i), LogBF, hash, key))
            None
          else {
            val b = if (!branch.frozen) branch else unshare(branch.gen + 1, root, branch)
            if (null != b)
              childRemove(root, b, b.children(i), LogBF, hash, key, (b ne branch), 0)
            else
              rootRemove(root, hash, key, failures + 1)
          }
        }
      }
    } else
      failingRemove(root, hash, key)
  }

  private def failingRemove[A, B](root: Ref.Bound[Node[A, B]], hash: Int, key: A): Option[B] = {
    // running in a transaction guarantees that CAS won't fail
    STM.atomic { txn => rootRemove(root.unbind, hash, key)(txn) }
  }

  /* @tailrec */ private def childRemove[A, B](root: Ref.Bound[Node[A, B]],
                                         rootNode: Branch[A, B],
                                         current: Ref.Bound[Node[A, B]],
                                         shift: Int,
                                         hash: Int,
                                         key: A,
                                         checked: Boolean,
                                         failures: Int): Option[B] = {
    if (failures < 10) {
      !current match {
        case leaf: Leaf[A, B] => {
          val i = leaf.find(hash, key)
          if (i < 0)
            None // no change, key wasn't present
          else if (STM.ccasi(root.unbind, rootNode, current.unbind, leaf, leaf.withRemove(i)))
            leaf.get(i) // success
          else if ((!root) ne rootNode)
            failingRemove(root, hash, key) // root retry
          else
            childRemove(root, rootNode, current, shift, hash, key, checked, failures + 1) // local retry
        }
        case branch: Branch[A, B] => {
          val i = indexFor(shift, hash)
          if (!checked && branch.gen != rootNode.gen && !contains(branch.children(i), shift + LogBF, hash, key))
            None // child is absent
          else {
            val b = if (branch.gen == rootNode.gen) branch else unshare(rootNode.gen, current, branch)
            if (null != b)
              childRemove(root, rootNode, b.children(i), shift + LogBF, hash, key, checked || (b ne branch), failures)
            else
              childRemove(root, rootNode, current, shift, hash, key, checked, failures + 1)
          }
        }
      }
    } else
      failingRemove(root, hash, key)
  }

  def setForeach[A, B, U](root: Ref.Bound[Node[A, B]], f: A => U) { frozenRoot(root).setForeach(f) }

  def mapForeach[A, B, U](root: Ref.Bound[Node[A, B]], f: ((A, B)) => U) { frozenRoot(root).mapForeach(f) }

  def setIterator[A, B](root: Ref.Bound[Node[A, B]]): Iterator[A] = frozenRoot(root).setIterator

  def mapIterator[A, B](root: Ref.Bound[Node[A, B]]): Iterator[(A, B)] = frozenRoot(root).mapIterator


  //////////////// hash trie operations on Ref, requiring an Txn

  def frozenRoot[A, B](root: Ref[Node[A, B]])(implicit txn: Txn): Node[A, B] = {
    !root match {
      case leaf: Leaf[A, B] => leaf // leaf is already immutable
      case branch: Branch[A, B] if branch.frozen => branch
      case branch: Branch[A, B] => {
        val b = branch.withFreeze
        root := b
        b
      }
    }
  }

  def contains[A, B](root: Ref[Node[A, B]], key: A)(implicit txn: Txn): Boolean = contains(root, 0, keyHash(key), key)(txn)

  /* @tailrec */ private def contains[A, B](n: Ref[Node[A, B]], shift: Int, hash: Int, key: A)(implicit txn: Txn): Boolean = {
    !n match {
      case leaf: Leaf[A, B] => leaf.contains(hash, key)
      case branch: Branch[A, B] => contains(branch.children(indexFor(shift, hash)).unbind, shift + LogBF, hash, key)(txn)
    }
  }

  def getOrThrow[A, B](root: Ref[Node[A, B]], key: A)(implicit txn: Txn): B = getOrThrow(root, 0, keyHash(key), key)(txn)

  /* @tailrec */ private def getOrThrow[A, B](n: Ref[Node[A, B]], shift: Int, hash: Int, key: A)(implicit txn: Txn): B = {
    !n match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (i < 0)
          throw new NoSuchElementException("key not found: " + key)
        leaf.getValue(i)
      }
      case branch: Branch[A, B] => getOrThrow(branch.children(indexFor(shift, hash)).unbind, shift + LogBF, hash, key)(txn)
    }
  }

  def get[A, B](root: Ref[Node[A, B]], key: A)(implicit txn: Txn): Option[B] = get(root, 0, keyHash(key), key)(txn)

  /* @tailrec */ private def get[A, B](n: Ref[Node[A, B]], shift: Int, hash: Int, key: A)(implicit txn: Txn): Option[B] = {
    !n match {
      case leaf: Leaf[A, B] => leaf.mapGet(hash, key)
      case branch: Branch[A, B] => get(branch.children(indexFor(shift, hash)).unbind, shift + LogBF, hash, key)(txn)
    }
  }

  def put[A, B](root: Ref[Node[A, B]], key: A, value: B)(implicit txn: Txn): Option[B] = rootPut(root, keyHash(key), key, value)(txn)

  private def rootPut[A, B](root: Ref[Node[A, B]], hash: Int, key: A, value: B)(implicit txn: Txn): Option[B] = {
    !root match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (!leaf.noChange(i, value))
          root := leaf.withPut(0L, 0, hash, key, value, i)
        leaf.get(i)
      }
      case branch: Branch[A, B] => {
        val b = if (!branch.frozen) branch else unshare(branch.gen + 1, root, branch)
        childPut(b.gen, b.children(indexFor(0, hash)).unbind, LogBF, hash, key, value)(txn)
      }
    }
  }

  private def unshare[A, B](rootGen: Long, current: Ref[Node[A, B]], branch: Branch[A, B])(implicit txn: Txn): Branch[A, B] = {
    val b = branch.clone(rootGen)
    current := b
    b
  }

  /* @tailrec */ private def childPut[A, B](rootGen: Long, current: Ref[Node[A, B]], shift: Int, hash: Int, key: A, value: B
          )(implicit txn: Txn): Option[B] = {
    !current match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (!leaf.noChange(i, value))
          current := leaf.withPut(rootGen, shift, hash, key, value, i)
        leaf.get(i)
      }
      case branch: Branch[A, B] => {
        val b = if (branch.gen == rootGen) branch else unshare(rootGen, current, branch)
        childPut(rootGen, b.children(indexFor(shift, hash)).unbind, shift + LogBF, hash, key, value)(txn)
      }
    }
  }

  def remove[A, B](root: Ref[Node[A, B]], key: A)(implicit txn: Txn): Option[B] = rootRemove(root, keyHash(key), key)(txn)

  private def rootRemove[A, B](root: Ref[Node[A, B]], hash: Int, key: A)(implicit txn: Txn): Option[B] = {
    !root match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (i >= 0)
          root := leaf.withRemove(i)
        leaf.get(i)
      }
      case branch: Branch[A, B] => {
        val i = indexFor(0, hash)
        if (branch.frozen && !contains(branch.children(i).unbind, LogBF, hash, key))
          None
        else {
          val b = if (!branch.frozen) branch else unshare(branch.gen + 1, root, branch)
          childRemove(b.gen, b.children(i).unbind, LogBF, hash, key, (b ne branch))(txn)
        }
      }
    }
  }

  /* @tailrec */ private def childRemove[A, B](rootGen: Long, current: Ref[Node[A, B]], shift: Int, hash: Int, key: A, checked: Boolean
          )(implicit txn: Txn): Option[B] = {
    !current match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (i >= 0)
          current := leaf.withRemove(i)
        leaf.get(i)
      }
      case branch: Branch[A, B] => {
        val i = indexFor(shift, hash)
        if (!checked && branch.gen != rootGen && !contains(branch.children(i).unbind, shift + LogBF, hash, key))
          None // child is absent
        else {
          val b = if (branch.gen == rootGen) branch else unshare(rootGen, current, branch)
          childRemove(rootGen, b.children(i).unbind, shift + LogBF, hash, key, checked || (b ne branch))(txn)
        }
      }
    }
  }

  def setForeach[A, B, U](root: Ref[Node[A, B]], f: A => U)(implicit txn: Txn) {
    // no need to freeze the root, because we know that the entire visit is
    // part of an atomic block
    (!root).setForeach(f)
  }

  def mapForeach[A, B, U](root: Ref[Node[A, B]], f: ((A, B)) => U)(implicit txn: Txn) { (!root).mapForeach(f) }

  def setIterator[A, B](root: Ref[Node[A, B]])(implicit txn: Txn): Iterator[A] = frozenRoot(root).setIterator

  def mapIterator[A, B](root: Ref[Node[A, B]])(implicit txn: Txn): Iterator[(A, B)] = frozenRoot(root).mapIterator
}
