import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        // Should return a list of all Transaction-objects stored in transactions
        transactions.toList.map { case (_, transaction) => transaction }
    }

    def allTransactionsCompleted: Boolean = {
        // Should return whether all Transaction-objects in transactions are completed
        getTransactions forall { _.isCompleted }
    }

    def withdraw(amount: Double): Unit = amount match {
        case `amount` if `amount` < 0 => throw new IllegalAmountException("You can't withdraw negative amounts")
        case `amount` if `amount` > getBalanceAmount => throw new NoSufficientFundsException("Withdrawal too big")
        case _ => balance.synchronized { balance.amount -= amount }
    }

    def deposit(amount: Double): Unit = amount match {
        case `amount` if `amount` < 0 => throw new IllegalAmountException("You can't deposit negative amounts")
        case _ => balance.synchronized { balance.amount += amount }
    }

    def getBalanceAmount: Double = balance.synchronized { balance.amount }

    def sendTransactionToBank(t: Transaction): Unit = {
        // Should send a message containing t to the bank of this account
        val bankRef: ActorRef = BankManager.findBank(bankId)
        bankRef ! t
    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {

        val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

        if (reserveTransaction(t)) {
            try {
                withdraw(amount)
                sendTransactionToBank(t)

            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
        }
        t
    }

    def reserveTransaction(t: Transaction): Boolean = {
      if (!transactions.contains(t.id)) {
        transactions += (t.id -> t)
        return true
      }
      false
    }

    override def receive: PartialFunction[Any, Unit] = {
        case IdentifyActor => sender ! this

        case TransactionRequestReceipt(to, transactionId, transaction) => to match {
            // Process receipt
            case `to` if `to` == getFullAddress =>
                val temp = transactions(transactionId)
                temp.status = transaction.status
                temp.receiptReceived = true
                if (temp.status == TransactionStatus.FAILED) deposit(transaction.amount)
            case _ => BankManager.findBank(to.take(4)) ! TransactionRequestReceipt(to, transactionId, transaction)
        }

        case BalanceRequest => sender ! getBalanceAmount // Should return current balance

        case t: Transaction => {
          // Handle incoming transaction
            if (t.to == getFullAddress) {
                try {
                    deposit(t.amount)
                    t.status = TransactionStatus.SUCCESS
                    BankManager.findBank(bankId) ! TransactionRequestReceipt(t.from, t.id, t)
                } catch {
                    case _: IllegalAmountException =>
                        t.status = TransactionStatus.FAILED
                        val bankId = t.from.dropRight(4)
                        BankManager.findBank(bankId) ! TransactionRequestReceipt(t.from, t.id, t)
                }
            } else {
                transferTo(t.to, t.amount)
            }
        }
        case msg => println(s"From Account: $msg")
    }

}
