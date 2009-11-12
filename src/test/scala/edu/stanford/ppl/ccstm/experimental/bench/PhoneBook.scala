/* CCSTM - (c) 2009 Stanford University - PPL */

// PhoneBook

package edu.stanford.ppl.ccstm.experimental.bench

import edu.stanford.ppl.ccstm.impl.FastSimpleRandom


/** Tests <code>IndexedMap</code> performance using an
 *  <code>IndexedMap[Int,(Name,Number)]</code>.  A fixed number <i>N</i> of
 *  names and numbers are generated ahead of time, then connections are added
 *  until the average number of links to a name is <i>LF</i>.  The average
 *  number of links per number will also be <i>LF</i>.
 */

class PhoneBook(n: Int, mapName: String) {
  import edu.stanford.ppl.ParUtil._
  
  //println("generating " + n + " names and numbers")
  val names = randArray(n, randomName(_))
  val numbers = randArray(n, randomNumber(_))
  val imap = IndexedMap.createByName[Int,(String,String)](mapName)


  def addLinks(numLinks: Int, numThreads: Int): Long = {

    val NumRands = 128
    require((NumRands % numThreads) == 0)
    val rands = Array.fromFunction(i => new FastSimpleRandom(i))(NumRands)

    //println("adding " + numLinks + " links using " + numThreads + " threads")

    val elapsed = timeParallel(numThreads)(index => {
      var i = index
      while (i < numLinks) {
        val r = rands(i & (NumRands - 1))
        imap.put(r.nextInt(), (names(r.nextInt(n)), numbers(r.nextInt(n))))
        i += numThreads
      }
    })

    println(elapsed + " millis to add " + numLinks + " links using " + numThreads +
            " threads, " + numLinks / (elapsed * 0.001) + " links/sec")

    return elapsed
  }

  

  private def randArray(n: Int, f: (FastSimpleRandom => String)): Array[String] = {
    val rand = new FastSimpleRandom(0)
    val s = new java.util.HashSet[String]
    while (s.size() < n) s.add(f(rand))
    val z = new Array[String](n)
    s.toArray(z)
    z
  }

  private def randomName(rand: FastSimpleRandom): String = {
    val cs = "bcdfghjklmnpqrstvwxz"
    val vs = "aeiouy"
    def c = cs.charAt(rand.nextInt(cs.length))
    def v = vs.charAt(rand.nextInt(vs.length))

    val buf = new StringBuilder(10)
    (buf.append(c).append(v).append(c).append(' ')
        .append(c).append(v).append(c)
        .append(c).append(v).append(c)).toString
  }

  private def randomNumber(rand: FastSimpleRandom): String = {
    def d = ('0'+rand.nextInt(10)).toChar

    val buf = new StringBuilder(12)
    (buf.append(d).append(d).append(d).append('-')
        .append(d).append(d).append(d).append('-')
        .append(d).append(d).append(d).append(d)).toString
  }
}