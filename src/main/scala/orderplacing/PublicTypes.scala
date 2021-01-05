package orderplacing

import orderplacing.SimpleTypes._
import orderplacing.CompoundTypes._

import java.net.URI

object PublicTypes {

    final case class UnvalidatedCustomerInfo(firstName: String, lastName: String, emailAddress: String, vipStatus: String)

    final case class UnvalidatedAddress(addressLine1: String, addressLine2: String, addressLine3: String, addressLine4: String, city: String, zipCode: String, country: String, state: UsStateCode)

    final case class UnvalidatedOrderLine(orderLineId: String, productCode: String, quantity: BigDecimal)

    final case class UnvalidatedOrder(orderId: String, customerInfo: UnvalidatedCustomerInfo, shippingAddress: UnvalidatedAddress, billingAddress: UnvalidatedAddress, lines: List[UnvalidatedOrderLine], promotionCode: String)

    final case class OrderAcknowledgmentSent(orderId: OrderId, emailAddress: EmailAddress)

    final case class PricedOrder(orderId: OrderId, customerInfo: CustomerInfo, shippingAddress: Address, billingAddress: Address, amountToBill: BillingAmount, lines: List[PricedOrderLine], pricingMethod: PricingMethod)

    final case class BillableOrderPlaced(orderId: OrderId, billingAddress: Address, amountToBill: BillingAmount)

    final case class ShippableOrderLine(productCode: ProductCode, quantity: OrderQuantity)

    final case class ShippableOrderPlaced(orderId: OrderId, shippingAddress: Address, shipmentLines: List[ShippableOrderLine], pdf: PdfAttachment)

    sealed trait PlaceOrderEvent

    object PlaceOrderEvent {

        final case class ShippableOrderPlacedEvent(event: ShippableOrderPlaced) extends PlaceOrderEvent

        final case class BillableOrderPlacedEvent(event: BillableOrderPlaced) extends PlaceOrderEvent

        final case class AcknowledgmentSentEvent(event: OrderAcknowledgmentSent) extends PlaceOrderEvent

    }

    type ValidationError = String

    type PricingError = String

    final case class ServiceInfo(name: String, endpoint: URI)

    final case class RemoteServiceError(service: ServiceInfo, exception: Exception)

    sealed trait PlaceOrderError

    object PlaceOrderError {

        final case class Validation(error: ValidationError) extends PlaceOrderError

        final case class Pricing(error: PricingError) extends PlaceOrderError

        final case class RemoteService(error: RemoteServiceError) extends PlaceOrderError

    }

    sealed trait ShippingMethod

    object ShippingMethod {

        final object PostalService extends ShippingMethod

        final object Fedex24 extends ShippingMethod

        final object Fedex48 extends ShippingMethod

        final object Ups48 extends ShippingMethod

    }

    final case class ShippingInfo(shippingMethod: ShippingMethod, shippingCost: Price)

    final case class PricedOrderWithShippingMethod(shippingInfo: ShippingInfo, pricedOrder: PricedOrder)

}
