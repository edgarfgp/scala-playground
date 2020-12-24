import java.util.Date

package object Domain {

    trait PlaceOrderError

    object PlaceOrderError {

        final case class OrderValidation(error: ValidationError) extends PlaceOrderError

        final case class AddressInvalidFormat(error: ValidationError) extends PlaceOrderError

        final case class AddressNotFound(error: ValidationError) extends PlaceOrderError

        final case class Pricing(error: ValidationError) extends PlaceOrderError

        final case class ProductOutOfStock(error: ValidationError) extends PlaceOrderError

        final case class Remote(error: ValidationError) extends PlaceOrderError

    }

    trait OrderTakingCommand

    object OrderTakingCommand {

        final case class Place(placeOrder: Command[UnvalidatedOrder]) extends OrderTakingCommand

        final case class Change(changeOrder: Command[UnvalidatedOrder]) extends OrderTakingCommand

        final case class Cancel(cancelOrder: Command[UnvalidatedOrder]) extends OrderTakingCommand

    }

    trait Order

    object Order {

        final case class Unvalidated(unvalidatedOrder: UnvalidatedOrder) extends Order

        final case class Validated(validatedOrder: ValidatedOrder) extends Order

        final case class Priced(pricedOrder: PricedOrder) extends Order

    }

    trait SendResult

    object SendResult {

        final case object Sent extends SendResult

        final case object NotSent extends SendResult

    }

    sealed trait ProductCode

    final object ProductCode {

        final case class Widget(widgetCode: String) extends ProductCode

        final case class Gizmo(gizmoCode: String) extends ProductCode

    }

    sealed trait OrderQuantity

    final object OrderQuantity {

        final case class UnitQty(quantity: Int) extends OrderQuantity

        final case class Kilos(kg: BigDecimal) extends OrderQuantity

    }

    sealed trait CustomerEmailState

    object CustomerEmailState {

        final case class Unverified(emailAddress: String) extends CustomerEmailState

        final case class Verified(verifiedEmailAddress: String) extends CustomerEmailState

    }

    sealed trait ContactInfo

    object ContactInfo {

        final case class EmailOnly(emailOnly: String) extends ContactInfo

        final case class AddressOnly(addressOnly: String) extends ContactInfo

        final case class EmailAndAddress(emailAndAddress: BothContactMethods) extends ContactInfo

    }

    trait PlaceOrderEvent

    object PlaceOrderEvent {

        final case class Placed(orderPlaced: PricedOrder) extends PlaceOrderEvent

        final case class BillPlaced(billableOrderPlaced: BillableOrderPlaced) extends PlaceOrderEvent

        final case class AcknowledgmentSent(orderAcknowledgmentSent: OrderAcknowledgmentSent) extends PlaceOrderEvent

    }

    final case class Address(addressLine1: String, addressLine2: String, addressLine3: String, addressLine4: String, city: String, zipCode: String)

    final case class OrderPlaced(orderId: String, customerInfo: CustomerInfo, shippingAddress: Address, billingAddress: Address, orderLines: List[PricedOrderLine], amountToBill: BigDecimal)

    final case class BillableOrderPlaced(orderId: String, billingAddress: Address, amount: BigDecimal)

    final case class CustomerInfo(name: PersonalName, emailAddress: String)

    final case class PersonalName(firstName: String, middleName: Option[String], lastName: String)

    final case class UnvalidatedOrder(orderId: String, customerInfo: CustomerInfo, shippingAddress: Address, billingAddress: Address, orderLines: List[UnValidatedOrderLine], amountToBill: BigDecimal)

    final case class ValidatedOrder(orderId: String, customerInfo: CustomerInfo, shippingAddress: Address, billingAddress: Address, orderLines: List[ValidatedOrderLine], amountToBill: BigDecimal)

    final case class UnValidatedOrderLine(orderLineId: String, orderId: String, productCode: ProductCode, orderQuantity: OrderQuantity, price: BigDecimal)

    final case class ValidatedOrderLine(orderLineId: String, orderId: String, productCode: ProductCode, orderQuantity: OrderQuantity, price: BigDecimal)

    final case class PricedOrderLine(orderLineId: String, orderId: String, productCode: ProductCode, orderQuantity: OrderQuantity, price: BigDecimal)

    final case class ValidationError(fieldName: String, errorDescription: String)

    final case class ServiceInfo(name: String, endPoint: String)

    final case class RemoteServiceError(service: ServiceInfo, exception: Exception)

    final case class OrderAcknowledgmentSent(orderId: String, emailAddress: String)

    final case class OrderAcknowledgment(emailAddress: String, letter: String)

    final case class BothContactMethods(emailContactInfo: String, postalContactInfo: String)

    final case class PricedOrder(orderId: String, customerInfo: CustomerInfo, shippingAddress: Address, billingAddress: Address, orderLines: List[PricedOrderLine], amountToBill: BigDecimal)

    case class Command[T](data: T, timeStamp: Date, userId: String)

    case class Contact(name: String, contactInfo: ContactInfo)

}
