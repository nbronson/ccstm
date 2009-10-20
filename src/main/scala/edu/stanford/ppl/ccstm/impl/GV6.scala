/* CCSTM - (c) 2009 Stanford University - PPL */

// GV6

package edu.stanford.ppl.ccstm.impl

import java.util.concurrent.atomic.AtomicLong


private[impl] trait GV6 {

  /** The global timestamp.  We use TL2's GV6 scheme to avoid the need to
   *  increment this during every transactional commit.  Non-transactional
   *  writes are even more conservative, incrementing the global version only
   *  when absolutely required.  This helps reduce contention (especially when
   *  there are many non-transactional writes), but it means we must always
   *  validate transactions that are not read-only.
   */
  private[impl] val globalVersion = new AtomicLong(1)

  /** The approximate ratio of the number of commits to the number of
   *  increments of <code>globalVersion</code>, as in TL2's GV6 scheme.  If
   *  greater than one, the actual choice to commit or not is made with a
   *  random number generator.
   */
  private val silentCommitRatio = ((Runtime.getRuntime.availableProcessors + 1) / 2) min 16

  /** If <i>x</i> is a signed integer evenly chosen from a uniform distribution
   *  between Integer.MIN_VALUE and Integer.MAX_VALUE, then the test
   *  <code>(x <= silentCommitCutoff)</code> will succeed approximately
   *  <code>1.0 / silentCommitRatio</code> of the time.
   */
  private val silentCommitCutoff = {
    ((1 << 31) + ((1L << 32) / silentCommitRatio) - 1).asInstanceOf[Int]
  }

  private val silentCommitRand = FastPoorRandom

  /** Returns a value that is greater than <code>prevVersion</code> and greater
   *  than the value of <code>globalVersion</code> on entry.  May increase
   *  <code>globalVersion</code>.
   */
  private[impl] def nonTxnWriteVersion(prevVersion: Long): Long = {
    // TODO: do a GV6-style thing, and only increment globalVersion some of the time 
    freshReadVersion(prevVersion) + 1
  }

  /** Returns a version to use for reading in a new transaction. */
  private[impl] def freshReadVersion: Long = globalVersion.get

  /** Guarantees that <code>globalVersion.get</code> is &ge;
   *  <code>minRV</code>, and returns <code>globalVersion.get</code>.
   */
  private[impl] def freshReadVersion(minRV: Long): Long = {
    var g = globalVersion.get
    while (g < minRV) {
      if (globalVersion.compareAndSet(g, minRV)) {
        return minRV
      }
      // failed, retry
      g = globalVersion.get
    }
    return g
  }

  /** Returns a value that is greater than <code>gvSnap</code>, possibly
   *  incrementing <code>globalVersion</code>.
   */
  private[impl] def freshCommitVersion(gvSnap: Long): Long = {
    if (silentCommitRatio <= 1 || silentCommitRand.nextInt <= silentCommitCutoff) {
      globalVersion.compareAndSet(gvSnap, gvSnap + 1)
      // no need to retry on failure, because somebody else succeeded
    }
    gvSnap + 1
  }  
}