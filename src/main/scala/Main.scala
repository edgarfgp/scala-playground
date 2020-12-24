import Domain.OrderQuantity.UnitQty
import Domain.ProductCode.Widget
import Domain._

object Main {

    def main(args: Array[String]): Unit = {

        val unvalidatedOrder = UnvalidatedOrder("1111",
            CustomerInfo(PersonalName("Edgar", None, "Gonzalez"), "edgargonzalez.info@gmail.com"),
            Address("Calle 72", "Portal 4 1H", "Madrid", "", "", ""),
            Address("Calle 72", "Portal 4 1H", "Madrid", "", "", ""),
            List(UnValidatedOrderLine("ABC123", "ABC123", Widget("12"), UnitQty(80), 23.0)), 100.8)

        OrderTrackingWorkflow.placeOrderWorkflow(unvalidatedOrder)
    }
}