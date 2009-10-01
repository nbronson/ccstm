/* CCSTM - (c) 2009 Stanford University - PPL */

// ReadSet

package edu.stanford.ppl.ccstm.impl


/** An immutable read set, used for explicit retry. */
private[impl] case class ReadSet(size: Int, versions: Array[STMImpl.Version], handles: Array[Handle[_]])
