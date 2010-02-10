package org.multiverse.benchmark.bank;

import org.multiverse.api.annotations.AtomicMethod;
import org.multiverse.api.annotations.AtomicObject;

/**
 * @author Pascal Felber
 * @since 0.1
 */
@AtomicObject
public class CheckingAccount extends Account {

	final private String m_name;
	private float m_balance;

	public CheckingAccount() {
		m_name = "Empty";
		m_balance = 0;
	}

	public CheckingAccount(String name) {
		m_name = name;
		m_balance = 0;
	}

	public String getName() {
		return m_name;
	}

    @AtomicMethod(readonly = true)
	public float getBalance() {
		return m_balance;
	}

    @AtomicMethod
	public void deposit(float amount) {
		m_balance += amount;
	}

    @AtomicMethod
	public void withdraw(float amount) throws OverdraftException {
		if (m_balance < amount)
			throw new OverdraftException("Cannot withdraw $" + amount + " from $" + m_balance);
		m_balance -= amount;
	}
}
