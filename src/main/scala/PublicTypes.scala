import CompoundTypes.{Address, CustomerInfo}
import SimpleTypes._

import java.net.URI

package object PublicTypes {
    final case class UnvalidatedCustomerInfo(
        firstName : String,
        lastName : String,
        emailAddress : String)

    final case class UnvalidatedAddress(
        addressLine1 : String,
        addressLine2 : String,
        addressLine3 : String,
        addressLine4 : String,
        city : String,
        zipCode : String)

    final case class UnvalidatedOrderLine(
        orderLineId : String,
        productCode : String,
        quantity : BigDecimal)

    final case class UnvalidatedOrder(
        orderId : String,
        customerInfo : UnvalidatedCustomerInfo,
        shippingAddress : UnvalidatedAddress,
        billingAddress : UnvalidatedAddress,
        lines : List[UnvalidatedOrderLine])

    final case class OrderAcknowledgmentSent(orderId : OrderId, emailAddress : EmailAddress)

    final case class PricedOrderLine(
        orderLineId : OrderLineId,
        productCode : ProductCode,
        quantity : OrderQuantity,
        linePrice : Price)

    final case class PricedOrder(
        orderId : OrderId,
        customerInfo : CustomerInfo,
        shippingAddress : Address,
        billingAddress : Address,
        amountToBill : BillingAmount,
        lines : List[PricedOrderLine])

    final case class BillableOrderPlaced(
        orderId : OrderId,
        billingAddress: Address,
        amountToBill : BillingAmount)

    sealed trait PlaceOrderEvent

    object PlaceOrderEvent {

        final case class OrderPlacedEvent(event : PricedOrder) extends PlaceOrderEvent

        final case class BillableOrderPlacedEvent(event : BillableOrderPlaced) extends PlaceOrderEvent

        final case class AcknowledgmentSentEvent(event : OrderAcknowledgmentSent) extends PlaceOrderEvent
    }

    type ValidationError = String

    type PricingError = String

    final case class ServiceInfo(name : String, endpoint: URI)

    final case class RemoteServiceError(service : ServiceInfo, exception : Exception)

    sealed trait PlaceOrderError

    object PlaceOrderError {

        final case class Validation(error : ValidationError) extends PlaceOrderError

        final case class Pricing(error : PricingError) extends PlaceOrderError

        final case class RemoteService(error : RemoteServiceError) extends PlaceOrderError
    }

    type PlaceOrder = UnvalidatedOrder => Either[PlaceOrderError, List[PlaceOrderEvent]]

    type PlaceOrderWithoutEffects =
        UnvalidatedOrder => List[PlaceOrderEvent]

}
