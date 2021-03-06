import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)
    val uid: Int = bank.generateAccountId

    def withdraw(amount: Double): Unit = balance.synchronized {
        amount match {
            case `amount` if `amount` < 0 => throw new IllegalAmountException("Negative deposits are invalid")
            case `amount` if `amount` <= balance.amount => balance.amount -= amount
            case _ => throw new NoSufficientFundsException("You're too poor")
        }
    }
    def deposit(amount: Double): Unit = balance.synchronized {
        amount match {
            case `amount` if `amount` > 0 => balance.amount += amount
            case _ => throw new IllegalAmountException("Negative deposits are invalid")
        }
    }
    def getBalanceAmount: Double = balance.synchronized { balance.amount }

    def transferTo(account: Account, amount: Double): Unit = {
        bank addTransactionToQueue (this, account, amount)
    }

}
