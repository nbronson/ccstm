CCSTM is a library-based STM for Scala.  It is available under a BSD license.

For the most thorough discussion of its design philsophy and structure, you
might start with the Scala Days 2010 Workshop paper:

* [doc/scaladays2010bronson.pdf](http://github.com/nbronson/ccstm/raw/master/doc/scaladays2010bronson.pdf)

If you are navigating in the code the most important interfaces are the Ref
trait and the STM object

* [src/main/scala/edu/stanford/ppl/ccstm/Ref.scala](http://github.com/nbronson/ccstm/blob/master/src/main/scala/edu/stanford/ppl/ccstm/Ref.scala)
* [src/main/scala/edu/stanford/ppl/ccstm/STM.scala](src/main/scala/edu/stanford/ppl/ccstm/STM.scala)
* [src/main/scala/edu/stanford/ppl/ccstm/Ref.scala](http://github.com/nbronson/ccstm/blob/master/src/main/scala/edu/stanford/ppl/ccstm/Ref.scala)
* [src/main/scala/edu/stanford/ppl/ccstm/STM.scala](src/main/scala/edu/stanford/ppl/ccstm/STM.scala)
