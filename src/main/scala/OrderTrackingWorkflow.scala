import ValidationStep._
import Domain._

object OrderTrackingWorkflow {
    def placeOrderWorkflow(unvalidatedOrder: UnvalidatedOrder): Unit = {
        val orderValidated = placeOrder(unvalidatedOrder)
        println(orderValidated)
    }
}