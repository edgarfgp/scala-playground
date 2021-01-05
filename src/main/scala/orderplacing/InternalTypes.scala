
package orderplacing

import orderplacing.CompoundTypes._
import orderplacing.PlaceOrderImplementation._
import orderplacing.PublicTypes._
import orderplacing.SimpleTypes._

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

    type PlaceOrderEventDto = Map[String, Object]

    type FreeVipShipping = PricedOrderWithShippingMethod => PricedOrderWithShippingMethod

    type AcknowledgeOrder =
        CreateOrderAcknowledgmentLetter
            => SendOrderAcknowledgment
            => PricedOrderWithShippingMethod
            => Option[OrderAcknowledgmentSent]

    type TryGetProductPrice = ProductCode => Option[Price]

    type PriceOrder = GetPricingFunction => ValidatedOrder => Either[PricingError, PricedOrder]

    type GetPricingFunction = PricingMethod => GetProductPrice

    type GetStandardPrices = Unit => GetProductPrice

    type GetPromotionPrices = PromotionCode => TryGetProductPrice


}
