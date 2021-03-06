import java.util.NoSuchElementException

import akka.actor._
import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.duration._
import akka.util.Timeout

case class GetAccountRequest(accountId: String)

case class CreateAccountRequest(initialBalance: Double)

case class IdentifyActor()

class Bank(val bankId: String) extends Actor {

    val accountCounter = new AtomicInteger(1000)

    def createAccount(initialBalance: Double): ActorRef = {
        // Should create a new Account Actor and return its actor reference. Accounts should be assigned with unique ids (increment with 1).
        accountCounter.set(accountCounter.get()+1)
        BankManager.createAccount(accountCounter.get().toString, bankId, initialBalance)
    }

    def findAccount(accountId: String): Option[ActorRef] = {
        // Use BankManager to look up an account with ID accountId
        try {
            Some(BankManager.findAccount(bankId, accountId))
        } catch {
            case _: NoSuchElementException => None
        }
    }

    def findOtherBank(bankId: String): Option[ActorRef] = {
        // Use BankManager to look up a different bank with ID bankId
        try {
            Some(BankManager.findBank(bankId))
        } catch {
            case _: NoSuchElementException => None
        }
    }

    override def receive: PartialFunction[Any, Unit] = {
        case CreateAccountRequest(initialBalance) => sender ! createAccount(initialBalance) // Create a new account
        case GetAccountRequest(id) => sender ! findAccount(id) // Return account
        case IdentifyActor => sender ! this
        case t: Transaction => processTransaction(t)

        case t: TransactionRequestReceipt => {
            // Forward receipt
            if (t.toAccountNumber.take(4) == bankId) {
                BankManager.findAccount(bankId, t.toAccountNumber.takeRight(4)) ! t
            } else  {
                BankManager.findBank(t.toAccountNumber.take(4)) ! t
            }
        }

        case msg => println(s"From Bank: $msg")
    }

    def processTransaction(t: Transaction): Unit = {
        implicit val timeout: Timeout = new Timeout(5.seconds)
        val isInternal = t.to.length <= 4
        val toBankId = if (isInternal) bankId else t.to.substring(0, 4)
        val toAccountId = if (isInternal) t.to else t.to.substring(4)
        val transactionStatus = t.status
        
        // This method should forward Transaction t to an account or another bank, depending on the "to"-address.
        // HINT: Make use of the variables that have been defined above.
        if (toBankId == bankId && transactionStatus == TransactionStatus.PENDING) {
            try {
                BankManager.findAccount(toBankId, toAccountId) ! t
            } catch {
                case _: NoSuchElementException =>
                    t.status = TransactionStatus.FAILED
                    BankManager.findAccount(bankId, t.from.takeRight(4)) ! TransactionRequestReceipt(t.from, t.id, t)
            }
        } else if (toBankId != bankId && transactionStatus == TransactionStatus.PENDING) {
            try {
                BankManager.findBank(toBankId) ! t
            } catch {
                case _: NoSuchElementException =>
                    t.status = TransactionStatus.FAILED
                    BankManager.findAccount(t.from.take(4), t.from.takeRight(4)) ! TransactionRequestReceipt(t.from, t.id, t)
            }
        } else if (toBankId == bankId && transactionStatus == TransactionStatus.FAILED) {
            BankManager.findAccount(bankId, t.from.takeRight(4)) ! t
        } else if (toBankId != bankId && transactionStatus == TransactionStatus.FAILED) {
            BankManager.findBank(t.from.take(4)) ! t
        } else {
            println("Incorrect state")
            t.status = TransactionStatus.FAILED
        }
    }
}