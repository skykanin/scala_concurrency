import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = this.synchronized {
        amount match {
            case `amount` if `amount` <= balance.amount => balance.amount -= amount
            case _ => throw new NoSufficientFundsException("You're too poor")
        }
    }
    def deposit(amount: Double): Unit = this.synchronized {
        balance.amount += amount
    }
    def getBalanceAmount: Double = this.synchronized { balance.amount }

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
