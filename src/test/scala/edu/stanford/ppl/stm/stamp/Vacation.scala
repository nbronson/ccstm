/* CCSTM - (c) 2009-2010 Stanford University - PPL */
//
//// Vacation
//
//package edu.stanford.ppl.stm.stamp
//
//import edu.stanford.ppl.ccstm.collection.TAnyRef
//import edu.stanford.ppl.ccstm.{Txn, Atomic}
//
//
//object Vacation {
//  object Action extends Enumeration {
//    type Action = Value
//    val MakeReservation, DeleteCustomer, UpdateTables = Value
//  }
//  import Action._
//
//  object ReservationType extends Enumeration {
//    type ReservationType = Value
//    val Car, Flight, Room = Value
//  }
//  import ReservationType._
//
//  class ReservationInfo
//
//  class Customer(id: Int) {
//    val reservationInfoHead = new TAnyRef[ReservationInfo](null)
//
//    def getBill(implicit txn: Txn) = {
//      var h = reservationInfoHead.get
//      var sum = 0
//      while (h != null) {
//        sum += h.price
//        h = h.next.get
//      }
//    }
//  }
//
//  class Client(id: Int,
//               manager: Manager,
//               numOperation: Int,
//               numQueryPerTransaction: Int,
//               queryRange: Int,
//               percentUser: Int) {
//    val rand = new scala.util.Random(id)
//
//    def randAction = {
//      val r = rand.nextInt(100)
//      if (r < percentUser) MakeReservation else if ((r & 1) != 0) DeleteCustomer else UpdateTables
//    }
//
//    def runThread(threadId: Int) {
//      val types = new Array[ReservationType](numQueryPerTransaction)
//      val ids = new Array[Int](numQueryPerTransaction)
//      val ops = new Array[Boolean](numQueryPerTransaction)
//      val prices = new Array[Int](numQueryPerTransaction)
//
//      for (i <- 0 until numOperation) {
//        randAction match {
//          case MakeReservation => {
//            val maxPrices = Array(-1, -1, -1)
//            val maxIds = Array(-1, -1, -1)
//            val numQuery = rand.nextInt(numQueryPerTransaction)
//            val custId = rand.nextInt(queryRange) + 1
//            for (n <- 0 until numQuery) {
//              types(n) = ReservationType(rand.nextInt(ReservationType.maxId))
//              ids(n) = rand.nextInt(queryRange) + 1
//            }
//            var isFound = false
//            new Atomic { def body {
//              for (n <- 0 until numQuery) {
//                val t = types(n)
//                val id = ids(n)
//                val price = (t match {
//                  case Car => manager.queryCarPrice(id)
//                  case Flight => manager.queryFlightPrice(id)
//                  case Room => manager.queryRoomPrice(id)
//                })
//                if (price > maxPrices(t.id)) {
//                  maxPrices(t.id) = price
//                  maxIds(t.id) = id
//                  isFound = true
//                }
//              }
//              if (isFound) manager.addCustomer(custId)
//              if (maxIds(Car.id) > 0) manager.reserveCar(custId, maxIds(Car.id))
//              if (maxIds(Flight.id) > 0) manager.reserveFlight(custId, maxIds(Flight.id))
//              if (maxIds(Room.id) > 0) manager.reserveRoom(custId, maxIds(Room.id))
//            }}.run()
//          }
//          case DeleteCustomer => {
//            val custId = rand.nextInt(queryRange) + 1
//            new Atomic { def body {
//              val bill = manager.queryCustomerBill(custId)
//              if (bill >= 0) {
//                manager.deleteCustomer(custId)
//              }
//            }}.run()
//          }
//          case UpdateTables => {
//            val numUpdate = rand.nextInt(numQueryPerTransaction) + 1
//            for (n <- 0 until numUpdate) {
//              types(n) = ReservationType(rand.nextInt(ReservationType.maxId))
//              ids(n) = rand.nextInt(queryRange) + 1
//              ops(n) = rand.nextBoolean
//              if (ops(n)) prices(n) = rand.nextInt(5) * 10 + 50
//            }
//            new Atomic { def body {
//              for (n <- 0 until numUpdate) {
//                val t = types(n)
//                val id = ids(n)
//                val doAdd = ops(n)
//                if (doAdd) {
//                  val newPrice = prices(n)
//                  t match {
//                    case Car => manager.addCar(id, 100, newPrice)
//                    case Flight => manager.addFlight(id, 100, newPrice)
//                    case Room => manager.addRoom(id, 100, newPrice)
//                  }
//                } else {
//                  t match {
//                    case Car => manager.deleteCar(id, 100)
//                    case Flight => manager.deleteFlight(id)
//                    case Room => manager.deleteRoom(id, 100)
//                  }
//                }
//              }
//            }}.run()
//          }
//        }
//      }
//    }
//  }
//}
