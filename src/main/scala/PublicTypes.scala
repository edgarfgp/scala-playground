import CompoundTypes.{Address, CustomerInfo}
import SimpleTypes.{BillingAmount, EmailAddress, OrderId, OrderLineId, OrderQuantity, PdfAttachment, Price, ProductCode}

import java.net.URI

package object PublicTypes {

    // ==================================
    // This file contains the definitions of PUBLIC types (exposed at the boundary of the bounded context)
    // related to the PlaceOrder workflow
    // ==================================

    // ------------------------------------
    // inputs to the workflow

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

    // ------------------------------------
    // outputs from the workflow (success case)

    /// Event will be created if the Acknowledgment was successfully posted
    final case class OrderAcknowledgmentSent(orderId : OrderId, emailAddress : EmailAddress)


    // priced state
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
    /// Event to send to shipping context


    type OrderPlaced = PricedOrder

    final case class BillableOrderPlaced(
        orderId : OrderId,
        billingAddress: Address,
        amountToBill : BillingAmount)

    /// The possible events resulting from the PlaceOrder workflow
    /// Not all events will occur, depending on the logic of the workflow
    sealed trait PlaceOrderEvent

    object PlaceOrderEvent {

        final case class OrderPlacedEvent(event : OrderPlaced) extends PlaceOrderEvent

        final case class BillableOrderPlacedEvent(event : BillableOrderPlaced) extends PlaceOrderEvent

        final case class AcknowledgmentSentEvent(event : OrderAcknowledgmentSent) extends PlaceOrderEvent
    }

    /// All the things that can go wrong in this workflow
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

    // ------------------------------------
    // the workflow itself
    type PlaceOrder = UnvalidatedOrder => Either[PlaceOrderError, List[PlaceOrderEvent]]

    type PlaceOrderWithoutEffects =
        UnvalidatedOrder => List[PlaceOrderEvent]

}
