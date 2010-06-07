package edu.stanford.ppl.ccstm.examples

// RemoteWriteResource

import scala.actors.Actor
import scala.util.Random

import edu.stanford.ppl.ccstm._

/** This example demonstrates using an `Actor` as a `WriteResource`.  It
 *  models a client actor that executes a transaction, with a server actor
 *  that is given the final commit/rollback decision.
 */
object RemoteWriteResource {
  
  case class Request(msg: String)
  case object Success
  case object Failure

  val server = new Actor {
    private val rand = new Random

    def act {
      loop {
        react {
          case Request(msg) => {
            val z = if (rand.nextBoolean) Success else Failure
            println(msg + " => " + z)
            sender ! z
          }
        }
      }
    }
  }
  server.start

  val successful = Ref(Nil : List[String])

  val client = new Actor {
    def act {
      loop {
        react {
          case Request(msg) => {
            println("client: " + msg)
            var attempt = 0
            atomic { implicit t =>
              attempt += 1
              successful() = (msg + "/" + attempt) :: successful()

              t.addWriteResource(new Txn.WriteResource {
                def prepare(txn: Txn): Boolean = {
                  if (txn.status.mightCommit) {
                    // At this point, the server has complete control over
                    // whether to commit or rollback.
                    server ! Request(msg + " attempt " + attempt)
                    receive {
                      case Success => true
                      case Failure => false
                    }
                  } else {
                    println(msg + " attempt + " + attempt + " failed before send to server")
                    false
                  }
                }
                def performCommit(txn: Txn) {}
                def performRollback(txn: Txn) {}
              }, Int.MaxValue) // higher priorities go last
            }
          }
        }
      }
    }
  }
  client.start

  def main(args: Array[String]) {
    for (i <- 0 until 10)
      client ! Request("request " + i)

    Thread.sleep(1000)

    println("successful = " + successful.single())

    System.exit(0)
  }
}