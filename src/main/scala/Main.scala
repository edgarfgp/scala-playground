import Domain.OrderQuantity.UnitQty
import Domain.ProductCode.Widget
import Domain._
import OrderTrackingWorkflow._

object Main  {

    def main(args: Array[String]): Unit = {
        placeOrderWorkflow(
            UnvalidatedOrder("ABC123",
                UnvalidatedCustomerInfo("Edgar", "Gonzalez", "edgargonzalez.info@gmail.com"),
                UnvalidatedAddress("Maraton 72", "Portal 4 1H", "Albacete", "", "", ""), UnvalidatedAddress("", "", "", "", "", ""),
                List(UnValidatedOrderLine("ABC123", "ABC123", Widget("12"), UnitQty(80), 23.0)), 100.8))
    }
}