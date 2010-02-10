/* CCSTM - (c) 2009 Stanford University - PPL */

// CheckingAccount

package edu.stanford.ppl.stm.bank


import edu.stanford.ppl.ccstm.{Ref, Txn}

class CheckingAccount(val name: String, initialBalance: Float) extends Account {

  private val _balance = Ref(initialBalance)

  def balance = _balance.source

  def deposit(amount: Float)(implicit txn: Txn) {
    assert(amount >= 0)
    _balance := !_balance + amount
  }

  def withdraw(amount: Float)(implicit txn: Txn) {
    assert(amount >= 0)
    if (_balance.get < amount) {
      throw new OverdraftException("Cannot withdraw $" + amount + " from $" + _balance.get)
    }
    _balance := !_balance - amount
  }

}

class CheckingAccountReadForWrite(val name: String, initialBalance: Float) extends Account {

  private val _balance = Ref(initialBalance)

  def balance = _balance.source

  def deposit(amount: Float)(implicit txn: Txn) {
    assert(amount >= 0)
    _balance := _balance.bind.readForWrite + amount
  }

  def withdraw(amount: Float)(implicit txn: Txn) {
    assert(amount >= 0)
    if (_balance.get < amount) {
      throw new OverdraftException("Cannot withdraw $" + amount + " from $" + _balance.get)
    }
    _balance := _balance.bind.readForWrite - amount
  }

}

class CheckingAccountTransform(val name: String, initialBalance: Float) extends Account {

  private val _balance = Ref(initialBalance)

  def balance = _balance.source

  def deposit(amount: Float)(implicit txn: Txn) {
    assert(amount >= 0)
    _balance.transform(_ + amount)
  }

  def withdraw(amount: Float)(implicit txn: Txn) {
    assert(amount >= 0)
    if (_balance.transformIfDefined(new PartialFunction[Float,Float] {
      def isDefinedAt(x: Float) = amount < x
      def apply(x: Float) = x - amount
    }) == false)
      throw new OverdraftException("Cannot withdraw $" + amount + " from $" + _balance.get)
  }

}