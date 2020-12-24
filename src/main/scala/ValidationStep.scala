import Domain.OrderQuantity._
import Domain.ProductCode._
import Domain._
import Extensions._
import AcknowledgmentStep._
import Domain.PlaceOrderError._

object ValidationStep {

    private def checkAddressExists(unvalidatedAddress: Address): Either[PlaceOrderError, Address] = {
        unvalidatedAddress.validateAddress
    }

    private def validatedOrder(unvalidatedOrder: UnvalidatedOrder): Either[PlaceOrderError, ValidatedOrder] = {
        // FIXME Add proper validation
        val orderId = eitherToValue(unvalidatedOrder.orderId.validateOrderId)
        val customerInfo = toCustomerInfo(unvalidatedOrder.customerInfo)
        val shippingAddress = toAddress(unvalidatedOrder.shippingAddress)
        val billingAddress = toAddress(unvalidatedOrder.billingAddress)
        val amountToBill = unvalidatedOrder.amountToBill
        val orderLines = unvalidatedOrder.orderLines.map(ol => toValidateOrderLine(ol))

        if (orderId.nonEmpty) {
            Right(ValidatedOrder(orderId, customerInfo, shippingAddress, billingAddress, orderLines, amountToBill))
        } else {
            Left(OrderValidation(ValidationError("", "")))
        }
    }

    private def toCustomerInfo(unvalidatedCustomerInfo: CustomerInfo): CustomerInfo = {
        val validateCustomerInfo = eitherToValue(unvalidatedCustomerInfo.validateCustomerInfo)
        validateCustomerInfo
    }


    private def toAddress(unvalidatedAddress: Address): Address = {
        checkAddressExists(unvalidatedAddress) match {
            case Left(value) => value match {
                case AddressInvalidFormat(invalidFormat) => throw new Exception(s"${invalidFormat.fieldName} $invalidFormat.errorDescription")
                case AddressNotFound(addressNotFound) => throw new Exception(s"${addressNotFound.fieldName} $addressNotFound.errorDescription")
            }

            case Right(value) => value
        }
    }

    private def toValidateOrderLine(unvalidatedOrderLine: UnValidatedOrderLine): ValidatedOrderLine = {
        val orderLineId = eitherToValue(unvalidatedOrderLine.orderLineId.validateOrderLineId)
        val orderId = eitherToValue(unvalidatedOrderLine.orderId.validateOrderId)
        val productCode = toProductCode(unvalidatedOrderLine.productCode)
        val orderQuantity = toOrderQuantity(unvalidatedOrderLine.productCode,
            unvalidatedOrderLine.orderQuantity)

        val orderPrice = unvalidatedOrderLine.price

        ValidatedOrderLine(orderLineId, orderId, productCode, orderQuantity, orderPrice)
    }

    private def toOrderQuantity(productCode: ProductCode, orderQuantity: OrderQuantity): OrderQuantity = {
        (productCode, orderQuantity) match {
            case (Widget(_), UnitQty(quantity)) =>
                val code = eitherToValue(quantity.validateOrderQuantity)
                UnitQty(code)

            case (Gizmo(_), Kilos(kg)) =>
                val code = eitherToValue(kg.validateOrderQuantityKilos)
                Kilos(code)
            // FIXME Find a better way to do this
            case _ => null
        }
    }

    private def toProductCode(productCode: ProductCode): ProductCode = {
        eitherToValue(productCode.validateProductCode)
    }

    private def priceOrder(validatedOrder: ValidatedOrder): Either[PlaceOrderError, PricedOrder] = {
        val lines = validatedOrder.orderLines.map(line => toPricedOrderLine(line))
        val amountToBill = validatedOrder.orderLines.map(line => line.price).sum
        if (lines.nonEmpty && amountToBill >= 0) {
            Right(PricedOrder(validatedOrder.orderId, validatedOrder.customerInfo, validatedOrder.shippingAddress, validatedOrder.billingAddress, lines, amountToBill))
        } else {
            Left(Pricing(ValidationError("", "")))
        }

    }

    private def toPricedOrderLine(validatedOrderLine: ValidatedOrderLine): PricedOrderLine = {
        val qty = validatedOrderLine.orderQuantity.value
        val price = getProductPrice(validatedOrderLine.productCode, validatedOrderLine)
        val linePrice = multiplyPrice(qty, optionToValue(price))

        PricedOrderLine(validatedOrderLine.orderLineId, validatedOrderLine.orderId, validatedOrderLine.productCode, validatedOrderLine.orderQuantity, linePrice)
    }

    private def multiplyPrice(qty: OrderQuantity, price: BigDecimal): BigDecimal = {
        qty match {
            case UnitQty(unitQty) =>
                unitQty * price
            case Kilos(kg) =>
                kg * price
        }
    }

    private def getProductPrice(productCode: ProductCode, validatedOrderLine: ValidatedOrderLine): Option[BigDecimal] = {
        if (validatedOrderLine.productCode == productCode) {
            Some(validatedOrderLine.price)
        } else {
            None
        }
    }

    private def changeOrderLinePrice(order: ValidatedOrder, orderLineId: String, newPrice: BigDecimal): ValidatedOrder = {
        // Find orderLine in order.OrderLines using the orderLineId
        val orderLine = order.orderLines.find(ol => ol.orderLineId == orderLineId)

        // name a new version of the OrderLine with the new price
        val newOrderLine = optionToValue(orderLine)
        newOrderLine.copy(price = newPrice)
        // Create a new list of lines, replacing newline with the old line
        val newOrderLines = order.orderLines.map(ol => if (ol.orderLineId == orderLineId) {
            newOrderLine
        } else {
            ol
        })

        // make a new amount of Bill
        val amountOfBill = newOrderLines.map(lines => lines.price).sum

        // make a new version of the order
        val newOrder = order.copy(orderLines = newOrderLines, amountToBill = amountOfBill)
        newOrder
    }

    def validatedOrderAdapted(input: UnvalidatedOrder): Either[PlaceOrderError, ValidatedOrder] = {
        validatedOrder(input)
    }

    def priceOrderAdapted(validatedOrder: ValidatedOrder): Either[PlaceOrderError, PricedOrder] = {
        priceOrder(validatedOrder)
    }

    def placeOrder(input: UnvalidatedOrder): Either[PlaceOrderError, List[PlaceOrderEvent]] = {
        validatedOrderAdapted(input) match {
            case Right(validatedOrder) =>
                priceOrderAdapted(validatedOrder) match {
                    case Right(pricedOrder) =>
                        acknowledgmentOrder(pricedOrder) match {
                            case Some(acknowledgment) =>
                                val result = createEvents(pricedOrder, Some(acknowledgment))
                                Right(result)
                            case None => Right(List())
                        }
                    case Left(validationError) => Left(validationError)
                }

            case Left(validationError) => Left(validationError)
        }
    }

}
