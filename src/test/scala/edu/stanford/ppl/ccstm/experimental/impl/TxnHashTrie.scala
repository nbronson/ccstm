/* scala-stm - (c) 2009-2010, Stanford University, PPL */

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm._
import edu.stanford.ppl.ccstm.experimental._
import edu.stanford.ppl.ccstm.impl.FastSimpleRandom

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

  //private def MaxContendedLeafCapacity = 4

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

    def withPut(gen: Long, shift: Int, hash: Int, key: A, value: B, i: Int, contended: Boolean): Node[A, B] = {
      if (i < 0)
        withInsert(~i, hash, key, value).splitIfNeeded(gen, shift, contended)
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

    def splitIfNeeded(gen: Long, shift: Int, contended: Boolean): Node[A, B] = if (!shouldSplit(contended)) this else split(gen, shift)

    def buildingSplitIfNeeded(shift: Int): BuildingNode[A, B] = if (!shouldSplit(false)) this else buildingSplit(shift)

    def shouldSplit(contended: Boolean): Boolean = {
      (contended || hashes.length > MaxLeafCapacity) && hashes(hashes.length - 1) != hashes(0)
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
}

abstract class TxnHashTrie[A, B](var root: Ref.Bound[TxnHashTrie.Node[A, B]]) {
  import TxnHashTrie._

  //////////////// txn contention tracking

  private final def pct = 10000

  val contentionThreshold = (System.getProperty("trie.contention", "1.0").toDouble * pct).toInt
  var contentionEstimate = 0

  private def recordNoContention() {
    if (FastSimpleRandom.nextInt(32) == 0) {
      val e = contentionEstimate
      contentionEstimate = e - (e >> 4) // this is 15/16, applied every 32nd time, so about 99.8%
    }
  }

  private def recordContention() {
    val e = contentionEstimate
    contentionEstimate = e + ((100 * pct - e) >> 9) // 100 * pct is the max
  }

  private def isContended = contentionEstimate > contentionThreshold


  //////////////// hash trie operations on escaped Ref.Bound

  protected def ntFrozenRoot(): Node[A, B] = {
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

  protected def ntClone(): Ref.Bound[Node[A, B]] = Ref(ntFrozenRoot()).nonTxn

  protected def ntSizeGE(n: Int): Boolean = ntFrozenRoot().cappedSize(n) >= n
  
  protected def ntSize(): Int = ntFrozenRoot().cappedSize(Int.MaxValue)

  protected def ntContains(key: A): Boolean = ntContains(root, 0, keyHash(key), key)

  /* @tailrec */ private def ntContains(n: Ref.Bound[Node[A, B]], shift: Int, hash: Int, key: A): Boolean = {
    !n match {
      case leaf: Leaf[A, B] => leaf.contains(hash, key)
      case branch: Branch[A, B] => ntContains(branch.children(indexFor(shift, hash)), shift + LogBF, hash, key)
    }
  }

  protected def ntGetOrThrow(key: A): B = ntGetOrThrow(root, 0, keyHash(key), key)

  /* @tailrec */ private def ntGetOrThrow(n: Ref.Bound[Node[A, B]], shift: Int, hash: Int, key: A): B = {
    !n match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (i < 0)
          throw new NoSuchElementException("key not found: " + key)
        leaf.getValue(i)
      }
      case branch: Branch[A, B] => ntGetOrThrow(branch.children(indexFor(shift, hash)), shift + LogBF, hash, key)
    }
  }

  protected def ntGet(key: A): Option[B] = ntGet(root, 0, keyHash(key), key)

  /* @tailrec */ private def ntGet(n: Ref.Bound[Node[A, B]], shift: Int, hash: Int, key: A): Option[B] = {
    !n match {
      case leaf: Leaf[A, B] => leaf.mapGet(hash, key)
      case branch: Branch[A, B] => ntGet(branch.children(indexFor(shift, hash)), shift + LogBF, hash, key)
    }
  }

  protected def ntPut(key: A, value: B): Option[B] = ntRootPut(keyHash(key), key, value, 0)

  /* @tailrec */ private def ntRootPut(hash: Int, key: A, value: B, failures: Int): Option[B] = {
    if (failures < 10) {
      !root match {
        case leaf: Leaf[A, B] => {
          val i = leaf.find(hash, key)
          if (leaf.noChange(i, value) || root.compareAndSetIdentity(leaf, leaf.withPut(0L, 0, hash, key, value, i, failures > 0)))
            leaf.get(i) // success, read from old leaf
          else
            ntRootPut(hash, key, value, failures + 1)
        }
        case branch: Branch[A, B] => {
          val b = if (!branch.frozen) branch else ntUnshare(branch.gen + 1, root, branch)
          if (null != b)
            ntChildPut(b, b.children(indexFor(0, hash)), LogBF, hash, key, value, 0)
          else
            ntRootPut(hash, key, value, failures + 1)
        }
      }
    } else
      failingPut(hash, key, value)
  }

  private def ntUnshare(rootGen: Long, current: Ref.Bound[Node[A, B]], branch: Branch[A, B]): Branch[A, B] = {
    val b = branch.clone(rootGen)
    if (current.compareAndSetIdentity(branch, b)) b else null
  }

  private def failingPut(hash: Int, key: A, value: B): Option[B] = {
    // running in a transaction guarantees that CAS won't fail
    STM.atomic { txn => txRootPut(hash, key, value)(txn) }
  }

  /* @tailrec */ private def ntChildPut(rootNode: Branch[A, B],
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
                  leaf, leaf.withPut(rootNode.gen, shift, hash, key, value, i, failures > 0)))
            leaf.get(i) // success
          else if ((!root) ne rootNode)
            failingPut(hash, key, value) // root retry
          else
            ntChildPut(rootNode, current, shift, hash, key, value, failures + 1) // local retry
        }
        case branch: Branch[A, B] => {
          val b = if (branch.gen == rootNode.gen) branch else ntUnshare(rootNode.gen, current, branch)
          if (null != b)
            ntChildPut(rootNode, b.children(indexFor(shift, hash)), shift + LogBF, hash, key, value, failures)
          else
            ntChildPut(rootNode, current, shift, hash, key, value, failures + 1) // failure, try again
        }
      }
    } else
      failingPut(hash, key, value)
  }

  protected def ntRemove(key: A): Option[B] = ntRootRemove(keyHash(key), key, 0)

  /* @tailrec */ private def ntRootRemove(hash: Int, key: A, failures: Int): Option[B] = {
    if (failures < 10) {
      !root match {
        case leaf: Leaf[A, B] => {
          val i = leaf.find(hash, key)
          if (i < 0 || root.compareAndSetIdentity(leaf, leaf.withRemove(i)))
            leaf.get(i) // success, read from old leaf
          else
            ntRootRemove(hash, key, failures + 1)
        }
        case branch: Branch[A, B] => {
          val i = indexFor(0, hash)
          if (branch.frozen && !ntContains(branch.children(i), LogBF, hash, key))
            None
          else {
            val b = if (!branch.frozen) branch else ntUnshare(branch.gen + 1, root, branch)
            if (null != b)
              ntChildRemove(b, b.children(i), LogBF, hash, key, (b ne branch), 0)
            else
              ntRootRemove(hash, key, failures + 1)
          }
        }
      }
    } else
      failingRemove(hash, key)
  }

  private def failingRemove(hash: Int, key: A): Option[B] = {
    // running in a transaction guarantees that CAS won't fail
    STM.atomic { txn => txRootRemove(hash, key)(txn) }
  }

  /* @tailrec */ private def ntChildRemove(rootNode: Branch[A, B],
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
            failingRemove(hash, key) // root retry
          else
            ntChildRemove(rootNode, current, shift, hash, key, checked, failures + 1) // local retry
        }
        case branch: Branch[A, B] => {
          val i = indexFor(shift, hash)
          if (!checked && branch.gen != rootNode.gen && !ntContains(branch.children(i), shift + LogBF, hash, key))
            None // child is absent
          else {
            val b = if (branch.gen == rootNode.gen) branch else ntUnshare(rootNode.gen, current, branch)
            if (null != b)
              ntChildRemove(rootNode, b.children(i), shift + LogBF, hash, key, checked || (b ne branch), failures)
            else
              ntChildRemove(rootNode, current, shift, hash, key, checked, failures + 1)
          }
        }
      }
    } else
      failingRemove(hash, key)
  }

  protected def ntSetForeach[U](f: A => U) { ntFrozenRoot().setForeach(f) }

  protected def ntMapForeach[U](f: ((A, B)) => U) { ntFrozenRoot().mapForeach(f) }

  protected def ntSetIterator(): Iterator[A] = ntFrozenRoot().setIterator

  protected def ntMapIterator(): Iterator[(A, B)] = ntFrozenRoot().mapIterator


  //////////////// hash trie operations on Ref, requiring an Txn

  protected def txFrozenRoot()(implicit txn: Txn): Node[A, B] = {
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

  protected def txContains(key: A)(implicit txn: Txn): Boolean = txContains(root.unbind, 0, keyHash(key), key)(txn)

  /* @tailrec */ private def txContains(n: Ref[Node[A, B]], shift: Int, hash: Int, key: A)(implicit txn: Txn): Boolean = {
    !n match {
      case leaf: Leaf[A, B] => leaf.contains(hash, key)
      case branch: Branch[A, B] => txContains(branch.children(indexFor(shift, hash)).unbind, shift + LogBF, hash, key)(txn)
    }
  }

  protected def txGetOrThrow(key: A)(implicit txn: Txn): B = txGetOrThrow(root.unbind, 0, keyHash(key), key)(txn)

  /* @tailrec */ private def txGetOrThrow(n: Ref[Node[A, B]], shift: Int, hash: Int, key: A)(implicit txn: Txn): B = {
    !n match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (i < 0)
          throw new NoSuchElementException("key not found: " + key)
        leaf.getValue(i)
      }
      case branch: Branch[A, B] => txGetOrThrow(branch.children(indexFor(shift, hash)).unbind, shift + LogBF, hash, key)(txn)
    }
  }

  protected def txGet(key: A)(implicit txn: Txn): Option[B] = txGet(root.unbind, 0, keyHash(key), key)(txn)

  /* @tailrec */ private def txGet(n: Ref[Node[A, B]], shift: Int, hash: Int, key: A)(implicit txn: Txn): Option[B] = {
    !n match {
      case leaf: Leaf[A, B] => leaf.mapGet(hash, key)
      case branch: Branch[A, B] => txGet(branch.children(indexFor(shift, hash)).unbind, shift + LogBF, hash, key)(txn)
    }
  }

  protected def txPut(key: A, value: B)(implicit txn: Txn): Option[B] = txRootPut(keyHash(key), key, value)(txn)

  private def txRootPut(hash: Int, key: A, value: B)(implicit txn: Txn): Option[B] = {
    root.unbind.get match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (!leaf.noChange(i, value))
          set(root.unbind, leaf.withPut(0L, 0, hash, key, value, i, isContended))
        leaf.get(i)
      }
      case branch: Branch[A, B] => {
        val b = if (!branch.frozen) branch else txUnshare(branch.gen + 1, root.unbind, branch)
        txChildPut(b.gen, b.children(indexFor(0, hash)).unbind, LogBF, hash, key, value)(txn)
      }
    }
  }

  private def set(ref: Ref[Node[A, B]], node: Node[A, B])(implicit txn: Txn) {
    if (ref.tryWrite(node)) {
      recordNoContention()
    } else {
      ref := node
      recordContention()
    }
  }

  private def txUnshare(rootGen: Long, current: Ref[Node[A, B]], branch: Branch[A, B])(implicit txn: Txn): Branch[A, B] = {
    val b = branch.clone(rootGen)
    current := b
    b
  }

  /* @tailrec */ private def txChildPut(rootGen: Long, current: Ref[Node[A, B]], shift: Int, hash: Int, key: A, value: B
          )(implicit txn: Txn): Option[B] = {
    !current match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (!leaf.noChange(i, value))
          set(current, leaf.withPut(rootGen, shift, hash, key, value, i, isContended))
        leaf.get(i)
      }
      case branch: Branch[A, B] => {
        val b = if (branch.gen == rootGen) branch else txUnshare(rootGen, current, branch)
        txChildPut(rootGen, b.children(indexFor(shift, hash)).unbind, shift + LogBF, hash, key, value)(txn)
      }
    }
  }

  protected def txRemove(key: A)(implicit txn: Txn): Option[B] = txRootRemove(keyHash(key), key)(txn)

  private def txRootRemove(hash: Int, key: A)(implicit txn: Txn): Option[B] = {
    root.unbind.get match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (i >= 0)
          set(root.unbind, leaf.withRemove(i))
        leaf.get(i)
      }
      case branch: Branch[A, B] => {
        val i = indexFor(0, hash)
        if (branch.frozen && !txContains(branch.children(i).unbind, LogBF, hash, key))
          None
        else {
          val b = if (!branch.frozen) branch else txUnshare(branch.gen + 1, root.unbind, branch)
          txChildRemove(b.gen, b.children(i).unbind, LogBF, hash, key, (b ne branch))(txn)
        }
      }
    }
  }

  /* @tailrec */ private def txChildRemove(rootGen: Long, current: Ref[Node[A, B]], shift: Int, hash: Int, key: A, checked: Boolean
          )(implicit txn: Txn): Option[B] = {
    !current match {
      case leaf: Leaf[A, B] => {
        val i = leaf.find(hash, key)
        if (i >= 0)
          set(current, leaf.withRemove(i))
        leaf.get(i)
      }
      case branch: Branch[A, B] => {
        val i = indexFor(shift, hash)
        if (!checked && branch.gen != rootGen && !txContains(branch.children(i).unbind, shift + LogBF, hash, key))
          None // child is absent
        else {
          val b = if (branch.gen == rootGen) branch else txUnshare(rootGen, current, branch)
          txChildRemove(rootGen, b.children(i).unbind, shift + LogBF, hash, key, checked || (b ne branch))(txn)
        }
      }
    }
  }

  protected def txSetForeach[U](f: A => U)(implicit txn: Txn) {
    // no need to freeze the root, because we know that the entire visit is
    // part of an atomic block
    root.unbind.get.setForeach(f)
  }

  protected def txMapForeach[U](f: ((A, B)) => U)(implicit txn: Txn) { root.unbind.get.mapForeach(f) }

  protected def txSetIterator()(implicit txn: Txn): Iterator[A] = txFrozenRoot().setIterator

  protected def txMapIterator()(implicit txn: Txn): Iterator[(A, B)] = txFrozenRoot().mapIterator
}
