/* CCSTM - (c) 2009-2010 Stanford University - PPL */

// Account

package edu.stanford.ppl.stm.bank


import edu.stanford.ppl.ccstm._

object Account {
  var yieldALot = false

  def addInterest(accounts: Array[Account], rate: Float)(implicit txn: Txn) {
    for (a <- accounts) {
      a.deposit(a.balance.get * rate)
      if (yieldALot) Thread.`yield`
    }
  }

  def computeTotal(accounts: Array[Account])(implicit txn: Txn) = {
    var total = 0.0
    for (a <- accounts) {
      total += a.balance.get
      if (yieldALot) Thread.`yield`
    }
    total
  }

  def transfer(src: Account, dst: Account, amount: Float)(implicit txn: Txn) = {
    dst.deposit(amount)
    if (yieldALot) Thread.`yield`
    src.withdraw(amount)
  }
}

trait Account {
  def name: String
  def balance: Source[Float]
  def deposit(amount: Float)(implicit txn: Txn)
  def withdraw(amount: Float)(implicit txn: Txn)
}
