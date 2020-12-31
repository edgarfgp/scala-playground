import CompoundTypes.{OrderAcknowledgment, ValidatedOrder}
import PlaceOrderImplementation.{AddressValidationError, SendResult}
import PublicTypes.{OrderAcknowledgmentSent, PlaceOrderError, PlaceOrderEvent, PricedOrder, PricedOrderWithShippingMethod, UnvalidatedAddress, UnvalidatedOrder, ValidationError}
import SimpleTypes.{CheckedAddress, HtmlString, Price, ProductCode}

object InternalTypes {

    type CalculateShippingCost = PricedOrder => Price

    type AddShippingInfoToOrder = CalculateShippingCost => PricedOrder => PricedOrderWithShippingMethod

    type CheckProductCodeExists = ProductCode => Boolean

    type CheckAddressExists = UnvalidatedAddress => Either[AddressValidationError, CheckedAddress]

    type GetProductPrice = ProductCode => Price

    type CreateOrderAcknowledgmentLetter = PricedOrder => HtmlString

    type SendOrderAcknowledgment = OrderAcknowledgment => SendResult

    type CreateEvents = PricedOrder => Option[OrderAcknowledgmentSent] => List[PlaceOrderEvent]

    type PlaceOrder = UnvalidatedOrder => Either[PlaceOrderError, List[PlaceOrderEvent]]

    type ValidateOrder = CheckProductCodeExists => CheckAddressExists => UnvalidatedOrder => Either[ValidationError, ValidatedOrder]

    type AcknowledgeOrder =
        CreateOrderAcknowledgmentLetter
            => SendOrderAcknowledgment
            => PricedOrderWithShippingMethod
            => Option[OrderAcknowledgmentSent]


}
