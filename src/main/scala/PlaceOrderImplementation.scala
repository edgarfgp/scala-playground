import CompoundTypes._
import InternalTypes._
import PlaceOrderDTO.Utils.listOfOption
import PlaceOrderImplementation.AddressValidationError._
import PlaceOrderImplementation.SendResult._
import PublicTypes.PlaceOrderError._
import PublicTypes.PlaceOrderEvent._
import PublicTypes.ShippingMethod._
import PublicTypes._
import SimpleTypes.PricedOrderLine.{CommentLine, ProductLine}
import SimpleTypes._

object PlaceOrderImplementation {

    sealed trait AddressValidationError
    object AddressValidationError {
        final object InvalidFormat extends  AddressValidationError
        final object AddressNotFound extends  AddressValidationError
    }

    sealed trait SendResult
    object SendResult {
        final object Sent extends SendResult
        final object NotSent extends SendResult
    }

    def checkProductExists: CheckProductCodeExists = _ => true

    def checkAddressExists: CheckAddressExists = unvalidatedAddress => Right(unvalidatedAddress)

    def getProductPrice: GetProductPrice = _ => Price.unsafeCreate(1000000)

    def createOrderAcknowledgmentLetter : CreateOrderAcknowledgmentLetter = _ =>  "some text"

    def sendOrderAcknowledgment: SendOrderAcknowledgment = _ => Sent

    def toCustomerInfo(unvalidatedCustomerInfo : UnvalidatedCustomerInfo) : Either[ValidationError, CustomerInfo] = {
        for {
            firstName <- String50.create("firstName", unvalidatedCustomerInfo.firstName)
            lastName <- String50.create("lastName", unvalidatedCustomerInfo.lastName)
            emailAddress <- EmailAddress.create("emailAddress", unvalidatedCustomerInfo.emailAddress)
            vipStatus <- VipStatus.create("vipStatus", unvalidatedCustomerInfo.vipStatus)
            customerInfo = CustomerInfo(PersonalName(firstName,lastName), emailAddress, vipStatus)
        } yield  customerInfo
    }

    def toAddress(checkedAddress: CheckedAddress) : Either[ValidationError, Address] = {
        for{
            addressLine1 <- String50.create("addressLine1", checkedAddress.addressLine1)
            addressLine2 <- String50.createOption("addressLine2", checkedAddress.addressLine2)
            addressLine3 <- String50.createOption("addressLine3", checkedAddress.addressLine3)
            addressLine4 <- String50.createOption("addressLine4", checkedAddress.addressLine4)
            city <- String50.create("city", checkedAddress.city)
            zipCode <- ZipCode.create("zipCode", checkedAddress.zipCode)
            country <- String50.create("country", checkedAddress.country)
            state <- UsStateCode.create("state", checkedAddress.state)
        } yield Address(addressLine1, addressLine2, addressLine3, addressLine4, city, zipCode, country, state)
    }


    def  toCheckedAddress(checkAddressExists: CheckAddressExists, unvalidatedAddress: UnvalidatedAddress): Either[HtmlString, CheckedAddress] = checkAddressExists(unvalidatedAddress) match {
            case Left(addressValidationError) =>
                addressValidationError match {
                    case InvalidFormat => Left("Address has bad format")
                    case AddressNotFound =>  Left("Address not found")
                }
            case Right(checkAddress) => Right(checkAddress)
        }

    def toOrderId(orderId: String): Either[ValidationError, OrderId] = OrderId.create("orderId", orderId)

    def toOrderLineId(orderLineId: String): Either[ValidationError, OrderLineId] = OrderLineId.create("orderLineId", orderLineId)

    def toProductCode(checkProductCodeExists: CheckProductCodeExists, productCode: String): Either[ValidationError, ProductCode] =
        ProductCode.create("productCode", productCode).flatMap { productCode =>
            if (checkProductCodeExists(productCode)) Right(productCode)
            else Left(s"Invalid $productCode")
        }

    def toOrderQuantity(productCode: ProductCode, quantity: BigDecimal) : Either[ValidationError, OrderQuantity] =
        OrderQuantity.create("orderQuantity", productCode, quantity)

    def toValidatedOrderLine(checkProductCodeExists: CheckProductCodeExists, unvalidatedOrderLine: UnvalidatedOrderLine): Either[ValidationError, ValidatedOrderLine] =
        for {
            orderLineId <- toOrderLineId(unvalidatedOrderLine.orderLineId)
            productCode <- toProductCode(checkProductCodeExists, unvalidatedOrderLine.productCode)
            quantity <- toOrderQuantity(productCode, unvalidatedOrderLine.quantity)
        } yield ValidatedOrderLine(orderLineId, productCode, quantity)


    def toPricedOrderLine(getProductPrice: GetProductPrice, validatedOrderLine: ValidatedOrderLine) : Either[PricingError, PricedOrderLine] = {
        val qty = OrderQuantity.value(validatedOrderLine.quantity)
        val price = getProductPrice(validatedOrderLine.productCode)
        Price.multiply(qty, price) match {
            case Left(value) => Left(value)
            case Right(value) =>
                val pricedOrderProductLine = PricedOrderProductLine(
                    validatedOrderLine.orderLineId,
                    validatedOrderLine.productCode,
                    validatedOrderLine.quantity,
                    value)
                Right(ProductLine(pricedOrderProductLine))
        }
    }

    def freeVipShipping : FreeVipShipping = {
        order =>
            val updatedShippingInfo = order.pricedOrder.customerInfo.vipStatus match {
                case VipStatus.Normal => order.shippingInfo
                case VipStatus.Vip => order.shippingInfo.copy(shippingCost =  Price.unsafeCreate(0.0), shippingMethod = Fedex24)
            }

            order.copy(shippingInfo = updatedShippingInfo)
    }

    def getLinePrice(line: PricedOrderLine) : Price = line match {
        case ProductLine(productLine) => productLine.linePrice
        case PricedOrderLine.CommentLine(_) => Price.unsafeCreate(0.0)
    }
    def addCommentLine(pricingMethod: PricingMethod, lines: List[PricedOrderLine]) : List[PricedOrderLine] = {
        pricingMethod match {
            case PricingMethod.Standard => lines
            case PricingMethod.Promotion(promotionCode) =>
                val commentLine = CommentLine(s"Applied promotion $promotionCode")
                commentLine :: lines
        }
    }

    def priceOrder: PriceOrder = {
        getPricingFunction => validatedOrder =>
            val getProductPrice = getPricingFunction(validatedOrder.pricingMethod)
            val (_, lines) = validatedOrder.lines.map(line => toPricedOrderLine(getProductPrice, line))
                .foldRight[(List[PricingError], List[PricedOrderLine])](Nil,Nil) {
                    case (Left(error), (e, i)) => (error :: e, i)
                    case (Right(result), (e, i)) =>
                        (e, result :: i)
                }
            val processedLines = addCommentLine(validatedOrder.pricingMethod, lines)
            val amountToBill = BillingAmount.sumPrices(processedLines.map(line => getLinePrice(line)))

            amountToBill match {
                case Left(value) => Left(value)
                case Right(billAmount) =>
                    val pricedOrder =
                        PricedOrder(
                            validatedOrder.orderId,
                            validatedOrder.customerInfo,
                            validatedOrder.shippingAddress,
                            validatedOrder.billingAddress,
                            billAmount,
                            processedLines,
                            validatedOrder.pricingMethod)
                    Right(pricedOrder)
            }
    }

    def acknowledgeOrder : AcknowledgeOrder = {
        createOrderAcknowledgmentLetter => sendOrderAcknowledgment => pricedOrderWithShipping =>
            val pricedOrder = pricedOrderWithShipping.pricedOrder
            val letter = createOrderAcknowledgmentLetter(pricedOrder)
            val acknowledgment = OrderAcknowledgment(pricedOrder.customerInfo.emailAddress, letter)
            sendOrderAcknowledgment(acknowledgment) match {
                case Sent =>
                    val event = OrderAcknowledgmentSent(pricedOrder.orderId, pricedOrder.customerInfo.emailAddress)
                    Some(event)
                case NotSent => None
            }
    }

    def createBillingEvent(placeOrder: PricedOrder) : Option[BillableOrderPlaced] =
        if(placeOrder.amountToBill > 0)
            Some(BillableOrderPlaced(placeOrder.orderId, placeOrder.billingAddress, placeOrder.amountToBill))
        else None

    def createEvents : CreateEvents = {
        pricedOrder => orderAcknowledgmentSent =>
        val orderPlacedEvents = List(OrderPlacedEvent(pricedOrder))
        val acknowledgmentEvents = listOfOption(orderAcknowledgmentSent.map(order => AcknowledgmentSentEvent(order)))
        val billingEvents = listOfOption(createBillingEvent(pricedOrder).map(bill => BillableOrderPlacedEvent(bill)))
        for {
            result <- List(orderPlacedEvents, acknowledgmentEvents, billingEvents).flatten
        } yield result
    }

    def placeOrder(checkProductExists: CheckProductCodeExists, checkAddressExists: CheckAddressExists,
        getProductPrice: GetPricingFunction, calculateShippingCost: CalculateShippingCost,
        createOrderAcknowledgmentLetter: CreateOrderAcknowledgmentLetter,
        sendOrderAcknowledgment: SendOrderAcknowledgment): PlaceOrder = {
            unvalidatedOrder =>
                validateOrder(checkProductExists) (checkAddressExists) (unvalidatedOrder) match {
                    case Left(validationError) => Left(Validation(validationError))
                    case Right(validatedOrder) =>
                        priceOrder(getProductPrice)(validatedOrder) match {
                            case Left(pricingError) => Left(Pricing(pricingError))
                            case Right(pricedOrder) =>
                                val pricedOrderWithShipping = freeVipShipping(addShippingInfoToOrder()(calculateShippingCost)(pricedOrder))
                                val acknowledgementOption =
                                    acknowledgeOrder(createOrderAcknowledgmentLetter)(sendOrderAcknowledgment)(pricedOrderWithShipping)
                                val events = createEvents(pricedOrder) (acknowledgementOption)
                                Right(events)
                        }
                }
        }

    def validateOrder: ValidateOrder =
        checkProductCodeExists => checkAddressExists => unvalidatedOrder =>
            for {
                orderId <- toOrderId(unvalidatedOrder.orderId)
                customerInfo <- toCustomerInfo(unvalidatedOrder.customerInfo)
                checkedShippingAddress <- toCheckedAddress(checkAddressExists, unvalidatedOrder.shippingAddress)
                checkedBillingAddress <- toCheckedAddress(checkAddressExists, unvalidatedOrder.billingAddress)
                shippingAddress <- toAddress(checkedShippingAddress)
                billingAddress <- toAddress(checkedBillingAddress)
                eitherLines = unvalidatedOrder.lines.map(unvalidatedOrderLine => toValidatedOrderLine(checkProductCodeExists, unvalidatedOrderLine))
                lines = eitherLines collect { case Right(validatedOrderLine) => validatedOrderLine }
                pricingMethod = PricingModule.createPricingMethod(unvalidatedOrder.promotionCode)
                validatedOrder = ValidatedOrder(orderId, customerInfo, shippingAddress, billingAddress, lines, pricingMethod)
            } yield validatedOrder

    def calculateShippingCost: CalculateShippingCost = {
        priceOrder =>
            val shippingAddress = priceOrder.shippingAddress
            if(shippingAddress.country == "US") shippingAddress.state match {
                case "CA" | "OR" | "AZ" | "NV" => Price.unsafeCreate(5.0)
                case _ => Price.unsafeCreate(10.0)
            }
            else Price.unsafeCreate(20.0)
    }

    def addShippingInfoToOrder() : AddShippingInfoToOrder = {
        calculateShippingCost => pricedOrder =>
            val shippingCost = calculateShippingCost(pricedOrder)
            val shippingInfo = ShippingInfo(Fedex24, shippingCost)
            PricedOrderWithShippingMethod(shippingInfo, pricedOrder)
    }
}
