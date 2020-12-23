import java.util.Date

package object Domain {

    trait PlaceOrderError
    object PlaceOrderError {
        final case class Validation(validationError: ValidationError) extends PlaceOrderError
        final case class Pricing(pricingError: PricingError) extends PlaceOrderError
        final case class ProductOutOfStock(productCode: ProductCode) extends PlaceOrderError
        final case class RemoteService(remoteService: RemoteService) extends PlaceOrderError
    }


    trait AddressValidationError
    object AddressValidationError {
        final case class InvalidFormat(invalidFormat: String) extends AddressValidationError
        final case class AddressNotFound(addressNotFound: String) extends AddressValidationError
    }

    trait OrderTakingCommand
    object OrderTakingCommand {
        final case class Place(placeOrder: PlaceOrder) extends OrderTakingCommand
        final case class Change(changeOrder: ChangeOrder) extends OrderTakingCommand
        final case class Cancel(cancelOrder: CancelOrder) extends OrderTakingCommand
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
        final case class Verified(verifiedEmailAddress: VerifiedEmailAddress) extends CustomerEmailState
    }

    sealed trait ContactInfo
    object ContactInfo {
        final case class EmailOnly(emailOnly: EmailContactInfo) extends ContactInfo
        final case class AddressOnly(addressOnly: PostalContactInfo) extends ContactInfo
        final case class EmailAndAddress(emailAndAddress: BothContactMethods) extends ContactInfo
    }

    trait PlaceOrderEvent
    object PlaceOrderEvent {
        final case class Placed(orderPlaced: PricedOrder) extends PlaceOrderEvent
        final case class BillPlaced(billableOrderPlaced: BillableOrderPlaced) extends PlaceOrderEvent
        final case class AcknowledgmentSent(orderAcknowledgmentSent: OrderAcknowledgmentSent) extends PlaceOrderEvent
    }

    type HtmlString = String
    type VerifiedEmailAddress = String
    type EmailContactInfo
    type PostalContactInfo

    final case class OrderPlaced(
        orderId: String,
        customerInfo: ValidatedCustomerInfo,
        shippingAddress: ValidatedAddress,
        billingAddress: ValidatedAddress,
        orderLines: List[PricedOrderLine],
        amountToBill: BigDecimal)

    final case class BillableOrderPlaced(orderId: String, billingAddress: ValidatedAddress, amount: BigDecimal)

    type PlaceOrder = Command[UnvalidatedOrder]
    type ChangeOrder = Command[UnvalidatedOrder]
    type CancelOrder = Command[UnvalidatedOrder]
    type AddressValidationService = UnvalidatedAddress => Option[ValidatedAddress]
    type CheckProductCodeExists = ProductCode => Boolean
    type CheckAddressExists = UnvalidatedAddress => ValidatedAddress
    type ValidateOrder = (CheckProductCodeExists, CheckAddressExists, UnvalidatedOrder) => ValidatedOrder
    type GetProductPrice = ProductCode => BigDecimal
    type PriceOrder = (GetProductPrice, ValidatedOrder) => PricedOrder

    final case class UnvalidatedAddress(
        addressLine1 : String,
        addressLine2: String,
        addressLine3 :String,
        addressLine4: String,
        city: String,
        zipCode: String){
    }

    final case class ValidatedAddress(
        addressLine1 : String,
        addressLine2: String,
        addressLine3 : String,
        addressLine4: String,
        city: String,
        zipCode: String)


    final case class ValidatedCustomerInfo(name: PersonalName, emailAddress: String)

    final case class PersonalName(firstName: String, middleName: Option[String], lastName: String)

    final case class UnvalidatedOrder(
        orderId: String,
        customerInfo: UnvalidatedCustomerInfo,
        shippingAddress: UnvalidatedAddress,
        billingAddress: UnvalidatedAddress,
        orderLines: List[UnValidatedOrderLine] ,
        amountToBill: BigDecimal)

    final case class ValidatedOrder(
        orderId: String,
        customerInfo: ValidatedCustomerInfo,
        shippingAddress: ValidatedAddress,
        billingAddress: ValidatedAddress,
        orderLines: List[ValidatedOrderLine] ,
        amountToBill: BigDecimal)

    final case class UnValidatedOrderLine(
        orderLineId: String,
        orderId: String,
        productCode: ProductCode,
        orderQuantity: OrderQuantity,
        price: BigDecimal)

    final case class ValidatedOrderLine(
        orderLineId: String,
        orderId: String,
        productCode: ProductCode,
        orderQuantity: OrderQuantity,
        price: BigDecimal)

    final case class PricedOrderLine(
       orderLineId: String,
       orderId: String,
       productCode: ProductCode,
       orderQuantity: OrderQuantity,
       price: BigDecimal)

    final case class ValidationError(fieldName: String, errorDescription: String)
    final case class PricingError(fieldName: String, errorDescription: String)
    final case class ServiceInfo(name: String, endPoint: String)
    final case class RemoteServiceError(service : ServiceInfo, exception: Exception)
    final case class OrderAcknowledgmentSent(orderId : String, emailAddress: VerifiedEmailAddress)
    final case class OrderAcknowledgment(emailAddress: VerifiedEmailAddress, letter: HtmlString)

    final case class BothContactMethods(emailContactInfo: EmailContactInfo, postalContactInfo: PostalContactInfo)
    final case class PricedOrder(
        orderId: String,
        customerInfo: ValidatedCustomerInfo,
        shippingAddress: ValidatedAddress,
        billingAddress: ValidatedAddress,
        orderLines: List[PricedOrderLine],
        amountToBill: BigDecimal)

    final case class UnvalidatedCustomerInfo(firstName: String, lastName: String, emailAddress: String)

    case class Command[T](data: T, timeStamp: Date, userId: String)

    case class Contact(name: String, contactInfo: ContactInfo)

/*
    type CheckNumber = Int
    type ContactId = Int
    type EnvelopContents = String

    type CardNumber = String


    //type UnvalidatedOrder

    type QuoteForm
    type OrderFrom
    type ProductCatalog
    //type PricedOrder
    type InvoiceId = String
    type PhoneNumber = String

    // Type function definitions
    type ConvertPaymentCurrency = Currency => Payment
    type CategorizeInboundMail = EnvelopContents => CategorizeEmail
    type CalculatePrices = (OrderFrom, ProductCatalog) => PricedOrder

    sealed trait CardType
    sealed trait PaymentMethod
    sealed trait Currency
    sealed trait PaymentError
    sealed trait CategorizeEmail
    sealed trait Invoice

    final case class UnpaidInvoice(id: InvoiceId)
    final case class PaidInvoice(id: InvoiceId)
    final object PaymentMethod {

        final case class Check(checkType: CheckNumber) extends PaymentMethod

        final case class Card(card: CreditCardInfo) extends PaymentMethod

        final case object Cash extends PaymentMethod

    }


    final object CategorizeEmail {

        final case class Quote(quoteForm: QuoteForm) extends CategorizeEmail

        final case class Form(orderFrom: OrderFrom) extends CategorizeEmail

    }

    final object Invoice {

        final case class Unpaid(unpaidInvoice: UnpaidInvoice) extends Invoice

        final case class Paid(paidInvoice: PaidInvoice) extends Invoice

    }

    final object CardType {

        final case object Visa extends CardType

        final case object MasterCard extends CardType

    }

    final object Currency {

        final case object EUR extends Currency

        final case object USD extends Currency

    }

    final object PaymentError {

        final object CardTypeNotRecognized extends PaymentError

        final object PaymentRejected extends PaymentError

        final object PaymentProviderOffline extends PaymentError

    }

    final case class CreditCardInfo(cardType: CardType, cardNumber: CardNumber)

    final case class Payment(amount: BigDecimal, currency: Currency, method: PaymentMethod)
*/


//    {
//
//        def canEqual(a: Any) = a.isInstanceOf[Contact]
//
//        override def equals(that: Any): Boolean =
//            that match {
//                case that: Contact => that.canEqual(this) &&
//                    this.hashCode == that.hashCode
//                case _ => false
//            }
//
//        override def hashCode: Int = {
//            val prime = 31
//            var result = 1
//            result = prime * result + phoneNumber.hashCode;
//            result = prime * result + emailAddress.hashCode;
//            result = prime * result
//            result
//        }
//    }
}
