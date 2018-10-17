import java.util.concurrent.atomic.AtomicInteger

import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

    private val uid = new AtomicInteger(0)
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = new ForkJoinPool

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
      transactionsQueue push new Transaction(
        transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
        processTransactions()
    }

    // Hint: use a counter 
    def generateAccountId: Int = {
        val read = uid.get()
        uid.set(uid.get() + 1)
        read
    }

    private def processTransactions(): Unit = {
      executorContext.execute { () => {
        val running = transactionsQueue.pop
        running.run()
        processedTransactions push running
        }
      }
      Thread.sleep(500)
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
