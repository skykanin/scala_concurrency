import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {
    private val queue: mutable.Queue[Transaction] = mutable.Queue.empty[Transaction]
    // Remove and return the first element from the queue
    def pop: Transaction = queue.synchronized {
      queue.dequeue
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = queue.synchronized {
      this.queue match {
        case `queue` if queue.isEmpty => true
        case _ => false
      }
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = queue.synchronized {
      queue.enqueue(t)
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = queue.synchronized { queue.front }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = queue.synchronized { queue.iterator }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttempts: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run(): Unit = {
      var attempts = allowedAttempts

      def doTransaction(): Unit = {
          from withdraw amount
          to deposit amount
      }

      def attempt(): Unit =
        if (from.uid < to.uid) from synchronized {
            to synchronized {
              doTransaction()
            }
        } else to synchronized {
            from synchronized {
              doTransaction()
            }
        }

      // Extend this method to satisfy requirements.
      while(attempts > 0) {
        try {
          attempt()
          status = TransactionStatus.SUCCESS
        } catch {
          case e: NoSufficientFundsException => attempts -= 1
            status = TransactionStatus.FAILED
          case f: IllegalAmountException => attempts -= 1
            status = TransactionStatus.FAILED
        }
      }
    }
}
