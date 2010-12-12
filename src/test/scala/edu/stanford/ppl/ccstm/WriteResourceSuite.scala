/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// WriteResourceSuite

package edu.stanford.ppl.ccstm


import org.scalatest.FunSuite
import java.text.ParseException
import scala.collection.mutable.ArrayBuffer
import java.util.concurrent.CountDownLatch

import edu.stanford.ppl.ParUtil._

class WriteResourceSuite extends FunSuite {
  
  test("WriteResource commit") {
    var counts = Array(0, 0, 0)
    atomic { implicit t =>
      t.addWriteResource(new Txn.WriteResource {
        def prepare(txn: Txn): Boolean = {
          assert(txn.status === Txn.Preparing)
          counts(0) += 1
          true
        }
        def performRollback(txn: Txn) { assert(false) }
        def performCommit(txn: Txn) { counts(2) += 1 }
      })
    }
    assert(counts(0) === 1)
    assert(counts(2) === 1)
  }

  test("WriteResource with rollback") {
    var counts = Array(0, 0, 0)
    atomic { implicit t =>
      t.addWriteResource(new Txn.WriteResource {
        def prepare(txn: Txn): Boolean = {
          assert(txn.status === Txn.Preparing)
          counts(0) += 1
          // only succeed after 3 failures
          counts(0) == 4
        }
        def performRollback(txn: Txn) {
          assert(txn.status.isInstanceOf[Txn.RollingBack])
          assert(counts(2) == 0)
          counts(1) += 1
        }
        def performCommit(txn: Txn) {
          assert(txn.status == Txn.Committing)
          counts(2) += 1
        }
      })
    }
    assert(counts(0) === 4)
    assert(counts(1) === 3)
    assert(counts(2) === 1)
  }

  test("WriteResource exception") {
    var counts = Array(0, 0, 0)
    intercept[ParseException] {
      atomic { implicit t =>
        t.addWriteResource(new Txn.WriteResource {
          def prepare(txn: Txn): Boolean = {
            assert(txn.status === Txn.Preparing)
            counts(0) += 1
            throw new ParseException("", 0)
          }
          def performRollback(txn: Txn) { counts(1) += 1 }
          def performCommit(txn: Txn) { assert(false) }
        })
      }
      assert(false)
    }
    assert(counts(0) === 1)
    assert(counts(1) === 1)
  }

  test("multiple WriteResource") {
    var counts = new Array[Int](3 * 10)
    var invocation = new ArrayBuffer[Int]
    atomic { implicit t =>
      for (id <- 0 until 10) {
        t.addWriteResource(new Txn.WriteResource {
          def prepare(txn: Txn): Boolean = {
            assert(txn.status === Txn.Preparing)
            invocation += id
            counts(3 * id) += 1
            // each fails once
            counts(3 * id) >= 2
          }
          def performRollback(txn: Txn) {
            assert(txn.status.isInstanceOf[Txn.RollingBack])
            counts(3 * id + 1) += 1
          }
          def performCommit(txn: Txn) {
            assert(txn.status == Txn.Committing)
            counts(3 * id + 2) += 1
          }
        })
      }
    }
    for (id <- 0 until 10) {
      assert(counts(3 * id) === 11 - id)
      assert(counts(3 * id + 1) === 10)
      assert(counts(3 * id + 2) === 1)
    }
  }

  test("WriteResource invulnerability") {
    val stage1 = new CountDownLatch(1)
    val stage2 = new CountDownLatch(50)
    val x = Ref(0)

    var mainResult: Throwable = null
    val main = new Thread() {
      override def run() {
        try {
          atomic { implicit t =>
            assert(x() === 0)
            x() = 1
            stage1.countDown()
            // beforeCommit is not invulnerable, this was used to test the test
//            t.beforeCommit({ _ =>
//              stage2.await();
//              Thread.sleep(500)
//            })
            t.addWriteResource(new Txn.WriteResource {
              def prepare(txn: Txn): Boolean = {
                stage2.await()
                // now all of the adversary threads are started, give them a bit
                // more time
                Thread.sleep(500)
                true
              }
              def performRollback(txn: Txn) { assert(false) }
              def performCommit(txn: Txn) {}
            })
          }
        } catch {
          case x => mainResult = x
        }
      }
    }
    main.start()
    stage1.await()

    for (adversary <- 0 par_until 50) {
      stage2.countDown()
      if (adversary == 0) {
        x.single.transform({ _ + 1 })
      } else {
        atomic { implicit t =>
          x() = x() + 1
        }
      }
    }

    main.join()
    if (mainResult != null) throw mainResult
  }
}