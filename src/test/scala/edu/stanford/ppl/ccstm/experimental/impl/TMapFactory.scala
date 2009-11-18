/* CCSTM - (c) 2009 Stanford University - PPL */

// TMapFactory

package edu.stanford.ppl.ccstm.experimental.impl

import edu.stanford.ppl.ccstm.experimental.TMap
import reflect.Manifest


object TMapFactory {
  def apply[A,B](name: String)(implicit am: Manifest[A], bm: Manifest[B]): TMap[A,B] = {
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
      case "p_sl_basic" => new PredicatedSkipListMap_Basic[A,B]
      case "t_h" => new ChainingHashMap[A,B]
      case "t_rb" => new RedBlackTreeMap[A,B]
    }
  }
}