/* CCSTM - (c) 2009 Stanford University - PPL */

// GV6

package edu.stanford.ppl.ccstm.impls


import java.util.concurrent.atomic.AtomicLong

private[impls] trait GV6 {
  type Version = Long

  /** The global timestamp.  We use TL2's GV6 scheme to avoid the need to
   *  increment this during every transactional commit.  Non-transactional
   *  writes are even more conservative, incrementing the global version only
   *  when absolutely required.  This helps reduce contention (especially when
   *  there are many non-transactional writes), but it means we must always
   *  validate transactions that are not read-only.
   */
  private[impls] val globalVersion = new AtomicLong(1)

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

  private val silentCommitRand = new FastPoorRandom

  /** Guarantees that <code>globalVersion</code> is greater than
   *  or equal to <code>prevVersion</code>, and returns a value greater than
   *  the value of <code>globalVersion</code> present on entry and greater
   *  than <code>prevVersion</code>.
   */
  private[impls] def nonTxnWriteVersion(prevVersion: Version): Version = {
    freshReadVersion(prevVersion) + 1
  }

  /** Returns a read version for use in a new transaction. */
  private[impls] def freshReadVersion: Version = globalVersion.get

  /** Guarantees that <code>globalVersion</code> is &ge; <code>minRV</code>,
   *  and returns a <code>globalVersion.get</code>.
   */
  private[impls] def freshReadVersion(minRV: Version): Version = {
    var g = globalVersion.get
    while (g < minRV) {
      if (globalVersion.compareAndSet(g, minRV)) {
        // succeeded, we're done
        return minRV
      }
      // failed, retry
      g = globalVersion.get
    }
    return g
  }

  /** Returns a value that is greater than the <code>globalVersion</code> on
   *  entry, possibly incrementing <code>globalVersion</code>.
   */
  private[impls] def freshCommitVersion: Version = {
    val g = globalVersion.get
    if (silentCommitRatio <= 1 || silentCommitRand.nextInt <= silentCommitCutoff) {
      globalVersion.compareAndSet(g, g + 1)
      // no need to retry on failure, because somebody else succeeded
    }
    g + 1
  }  
}