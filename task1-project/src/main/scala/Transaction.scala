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
        case queue.length == 0 => true
        case _ => false
      }
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = queue.synchronized {
      queue.enqueue(t)
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = queue.synchronized { this.queue.front }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = queue.synchronized { this.queue.iterator }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttempts: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run(): Unit = {

      def doTransaction(): Unit = {
          from withdraw amount
          to deposit amount
      }

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
      // TODO Implement allowed attempts here
      for (i <- allowedAttempts) {
        if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction()
          }
        } else to synchronized {
          from synchronized {
            doTransaction()
          }
        }
      }
    }
}
