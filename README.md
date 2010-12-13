**Important:** Work on a library-based STM for Scala has
shifted to the ScalaSTM project, whose home page is at
[http://nbronson.github.com/scala-stm](http://nbronson.github.com/scala-stm).
The source for the ScalaSTM project is at
[http://github.com/nbronson/scala-stm](http://github.com/nbronson/scala-stm).

The ScalaSTM API draws from CCSTM, but includes design improvements by
the Scala STM Expert Group and the ability to dynamically select the
underlying STM implementation.  ScalaSTM's reference implementation was
started from the nesting branch of CCSTM, which adds partial rollback
to the core CCSTM algorithm.

Documentation for CCSTM is available from
[http://ppl.stanford.edu/ccstm](http://ppl.stanford.edu/ccstm).
