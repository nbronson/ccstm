/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Handle

package edu.stanford.ppl.ccstm.impl


/** A <code>Handle</code> defines the operations that must be available for a
 *  particular memory location in order for it to be managed by the STM.  The
 *  identity of the location is determined by <code>ref</code> and
 *  <code>offset</code>, with <code>ref</code> be used only in comparisons
 *  using <code>eq</code> or <code>ne</code> (no methods of <code>ref</code>
 *  will ever be invoked).  Metadata may be shared between multiple locations.
 */
private[ccstm] trait Handle[@specialized(Int) T] {
  private[ccstm] def meta: Long
  private[ccstm] def meta_=(v: Long)
  private[ccstm] def metaCAS(before: Long, after: Long): Boolean
  private[ccstm] def ref: AnyRef
  private[ccstm] def offset: Int
  private[ccstm] def metaOffset: Int
  private[ccstm] def data: T
  private[ccstm] def data_=(v: T)
}
