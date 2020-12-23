
import Domain.PlaceOrderEvent.{AcknowledgmentSent, BillPlaced, Placed}
import Domain.SendResult.{NotSent, Sent}
import Domain._
import Extensions._

object AcknowledgmentStep {

    def acknowledgmentOrder(pricedOrder: PricedOrder) : Option[OrderAcknowledgmentSent] = {
        val letter = createOrderAcknowledgmentLetter(pricedOrder)
        val acknowledgment = OrderAcknowledgment(pricedOrder.customerInfo.emailAddress, letter)
        sendOrderAcknowledgment(acknowledgment) match {
            case Sent =>
                val orderAcknowledgmentSent = OrderAcknowledgmentSent(pricedOrder.orderId, pricedOrder.customerInfo.emailAddress)
                Some(orderAcknowledgmentSent)
            case NotSent =>
                None
        }
    }

    def createBillingEvent(pricedOrder: PricedOrder) : Option[BillPlaced] = {
        val billingAmount = pricedOrder.amountToBill.validateBillingAmount match {
            case Left(value) => value
            case Right(_) => null
        }

        if(billingAmount > 0) {
            Some(BillPlaced(BillableOrderPlaced(pricedOrder.orderId, pricedOrder.billingAddress, billingAmount)))
        }else{
            None
        }

    }

    def createEvents (pricedOrder: PricedOrder, orderAcknowledgmentSent: Option[OrderAcknowledgmentSent]) : List[PlaceOrderEvent] = {
        val event1 = List(Placed(pricedOrder))
        val event2 = listToOption(orderAcknowledgmentSent.map(order => AcknowledgmentSent(order)))
        val event3 = listToOption(createBillingEvent(pricedOrder).map(bill => bill))
        for {
            result <- List(event1, event2, event3).flatten
        } yield result
    }

    private def createOrderAcknowledgmentLetter(pricedOrder: PricedOrder) : HtmlString = {
        val htmlString = s"Thanks Mr.${pricedOrder.customerInfo.name} for using our services."
        htmlString
    }

    private def sendOrderAcknowledgment(orderAcknowledgment: OrderAcknowledgment) : SendResult = {
        if(orderAcknowledgment.emailAddress.nonEmpty & orderAcknowledgment.letter.nonEmpty){
            Sent
        }else{
            NotSent
        }
    }
}