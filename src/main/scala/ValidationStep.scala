import Domain.OrderQuantity.{Kilos, UnitQty}
import Domain.ProductCode.{Gizmo, Widget}
import Domain._
import Extensions._

object ValidationStep {

    def predicateToPassTru(errorMessage : String, productCode: ProductCode) : ProductCode = {
        productCode.validateProductCode match {
            case Right(_) => throw new Exception(errorMessage)
            case Left(productCode) =>
                productCode
        }
    }

     def checkAddressExists(unvalidatedAddress: UnvalidatedAddress) : ValidatedAddress ={
        val validatedAddress = eitherToValue(unvalidatedAddress.validateAddress)
        validatedAddress
    }


    def validatedOrder(unvalidatedOrder: UnvalidatedOrder): ValidatedOrder = {
        val orderId = eitherToValue(unvalidatedOrder.orderId.validateOrderId)
        val customerInfo = toCustomerInfo(unvalidatedOrder.customerInfo)
        val shippingAddress = toAddress(unvalidatedOrder.shippingAddress)
        val billingAddress = toAddress(unvalidatedOrder.billingAddress)
        val amountToBill = unvalidatedOrder.amountToBill
        val orderLines = unvalidatedOrder.orderLines.map(ol => toValidateOrderLine(ol))

        ValidatedOrder(orderId, customerInfo, shippingAddress, billingAddress, orderLines, amountToBill)
    }

    def toCustomerInfo(unvalidatedCustomerInfo: UnvalidatedCustomerInfo) : ValidatedCustomerInfo = {
        val validateCustomerInfo = eitherToValue(unvalidatedCustomerInfo.validateCustomerInfo)
        validateCustomerInfo
    }


    def toAddress(unvalidatedAddress: UnvalidatedAddress) : ValidatedAddress = {
        checkAddressExists(unvalidatedAddress)
    }

    def toValidateOrderLine(unvalidatedOrderLine: UnValidatedOrderLine) : ValidatedOrderLine = {
        val orderLineId = eitherToValue(unvalidatedOrderLine.orderLineId.validateOrderLineId)
        val orderId = eitherToValue(unvalidatedOrderLine.orderId.validateOrderId)
        val productCode = toProductCode(unvalidatedOrderLine.productCode)
        val orderQuantity =  toOrderQuantity(unvalidatedOrderLine.productCode,
            unvalidatedOrderLine.orderQuantity)

        val orderPrice = unvalidatedOrderLine.price

        ValidatedOrderLine(orderLineId, orderId, productCode, orderQuantity, orderPrice)
    }

    def toOrderQuantity(productCode: ProductCode, orderQuantity: OrderQuantity) : OrderQuantity = {
        (productCode, orderQuantity) match {
            case (Widget(_), UnitQty(quantity)) =>
                val code = eitherToValue(quantity.validateOrderQuantity)
                UnitQty(code)

            case (Gizmo(_), Kilos(kg)) =>
                val code = eitherToValue(kg.validateOrderQuantityKilos)
                Kilos(code)
        }
    }

    def toProductCode(productCode: ProductCode) : ProductCode = {
        val errorMessage = s"Invalid: $productCode"
        val validatedProductCode = eitherToValue(productCode.validateProductCode)
        val checkProduct = predicateToPassTru(errorMessage, validatedProductCode)
        checkProduct
    }

    def priceOrder(validatedOrder: ValidatedOrder) : PricedOrder = {
        val lines = validatedOrder.orderLines.map(line => toPricedOrderLine(line))

        val amountToBill = validatedOrder.orderLines.map(line => line.price).sum

        PricedOrder(validatedOrder.orderId, validatedOrder.customerInfo, validatedOrder.shippingAddress, validatedOrder.billingAddress, lines, amountToBill)
    }

    def toPricedOrderLine(validatedOrderLine : ValidatedOrderLine) : PricedOrderLine = {
        val qty = validatedOrderLine.orderQuantity.value
        val price = getProductPrice(validatedOrderLine.productCode, validatedOrderLine)
        val linePrice = multiplyPrice(qty, optionToValue(price))

        PricedOrderLine(validatedOrderLine.orderLineId, validatedOrderLine.orderId, validatedOrderLine.productCode, validatedOrderLine.orderQuantity,  linePrice)
    }

    private def multiplyPrice(qty: OrderQuantity, price : BigDecimal) : BigDecimal = {
        qty match {
            case UnitQty(unitQty) =>
                unitQty * price
            case Kilos(kg) =>
                kg * price
        }
    }

    def getProductPrice(productCode: ProductCode, validatedOrderLine: ValidatedOrderLine) : Option[BigDecimal] = {
        if(validatedOrderLine.productCode == productCode){
            Some(validatedOrderLine.price)
        }else{
            None
        }
    }

    def changeOrderLinePrice(order: ValidatedOrder, orderLineId: String, newPrice: BigDecimal): ValidatedOrder = {
        // Find orderLine in order.OrderLines using the orderLineId
        val orderLine = order.orderLines.find(ol => ol.orderLineId == orderLineId)

        // name a new version of the OrderLine with the new price
        val newOrderLine = optionToValue(orderLine)
        newOrderLine.copy(price = newPrice)
        // Create a new list of lines, replacing newline with the old line
        val newOrderLines = order.orderLines.map(ol => if (ol.orderLineId == orderLineId) { newOrderLine } else { ol })

        // make a new amount of Bill
        val amountOfBill = newOrderLines.map(lines => lines.price).sum

        // make a new version of the order
        val newOrder = order.copy(orderLines = newOrderLines, amountToBill = amountOfBill)
        newOrder
    }
}
