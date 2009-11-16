/* CCSTM - (c) 2009 Stanford University - PPL */

// IndexedMap

package edu.stanford.ppl.ccstm.experimental.bench

import reflect.Manifest
import edu.stanford.ppl.ccstm.experimental.TMap
import edu.stanford.ppl.ccstm.experimental.impl._

/** An <code>IndexedMap</code> is a map from keys of type <i>K</i> to values of
 *  type <i>V</i> that additionally maintains zero or more indices.  These
 *  indices allow direct lookup of the values by a function defined on
 *  <i>V</i>.  Lookups, insertions, updates, and removals on the main map are
 *  linearizable, and lookups on the indices are also linearizable.  All
 *  indices must be added before a reference to the indexed map is shared, and
 *  before any values are added to the indexed map.
 *  <p>
 *  Consider:<pre>
 *    case class Record(login: String, dept: String, office: Option[String])
 *
 *    val m = new IndexedMap[String,Record]
 *    val byDept = m.addIndex(r => Some(r.dept))
 *    val byOffice = m.addIndex(_.office)
 *
 *    m.put("bob", "Sales", Some("411"))
 *    m.put("ron", "R&D", Some("411"))
 *    m.put("alice, "R&D", Some("415"))
 *    m.put("john", "Sales", None)
 *
 *    assert(byDept("Sales").map(_._1) == Set("bob", "john"))
 *    assert(byOffice("411").map(_._1) == Set("bob", "ron"))
 *    assert(byOffice("415").map(_._1) == Set("alice"))
 *    assert(byDept.get("HR") == None)
 *    assert(byOffice.keySet == Set("411", "415"))
 *
 *    m.remove("alice")
 *    assert(byOffice.size == 1)
 *  </pre>
 */
trait IndexedMap[K,V] extends scala.collection.mutable.Map[K,V] {

  /** Creates and returns an index on this map that automatically associates
   *  <i>c</i> with <i>v</i> for every value <i>v</i> in the indexed map for
   *  which <code>f</code>(<i>v</i>) is <code>Some</code>(<i>c</i>).
   *  <p>
   *  This method may not be called after any values have been inserted into
   *  the indexed map.
   */
  def addIndex[C](f: V => Option[C])(implicit cm: Manifest[C]): scala.collection.Map[C,scala.collection.immutable.Set[(K,V)]]
}

object IndexedMap {
  def createByName[K,V](name: String)(implicit km: Manifest[K], vm: Manifest[V]): IndexedMap[K,V] = {
    if (name == "l_h") {
      new CoarseLockIndexedMap[K,V]
    } else {
      new STMIndexedMap[K,V](new STMIndexedMap.TMapFactory {
        def newInstance[A, B](implicit am: Manifest[A], bm: Manifest[B]): TMap[A,B] = {
          name match {
            case "b_h_basic" => new BoostedHashMap_Basic[A,B]
            case "b_h_enum" => new BoostedHashMap_Enum[A,B]
            case "b_h_enum_rw" => new BoostedHashMap_Enum_RW[A,B]
            case "b_h_gc" => new BoostedHashMap_GC[A,B]
            case "b_h_gc_rw" => new BoostedHashMap_GC_RW[A,B]
            case "b_h_gc_enum" => new BoostedHashMap_GC_Enum[A,B]
            case "b_h_gc_enum_rw" => new BoostedHashMap_GC_Enum_RW[A,B]
            case "b_h_rw" => new BoostedHashMap_RW[A,B]
            case "p_h_basic" => new PredicatedHashMap_Basic[A,B]
            case "p_h_enum" => new PredicatedHashMap_Enum[A,B]
            case "p_h_rc" => new PredicatedHashMap_RC[A,B]
            case "p_h_gc" => new PredicatedHashMap_GC[A,B]
            case "p_h_lazy" => new PredicatedHashMap_LazyGC[A,B]
            case "t_h" => new ChainingHashMap[A,B]
          }
        }
      })
    }
  }
}