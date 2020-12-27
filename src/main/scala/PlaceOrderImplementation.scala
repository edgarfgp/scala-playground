import CompoundTypes.{Address, CustomerInfo, PersonalName}
import PlaceOrderApi._
import PlaceOrderImplementation.AddressValidationError.{AddressNotFound, InvalidFormat}
import PlaceOrderImplementation.SendResult.{NotSent, Sent}
import PublicTypes.PlaceOrderError.{Pricing, Validation}
import PublicTypes.PlaceOrderEvent.{AcknowledgmentSentEvent, BillableOrderPlacedEvent, OrderPlacedEvent}
import PublicTypes._
import SimpleTypes._

object PlaceOrderImplementation {

    // ======================================================
    // This file contains the final implementation for the PlaceOrder workflow
    //
    // This represents the code in chapter 10, "Working with Errors"
    //
    // There are two parts:
    // * the first section contains the (type-only) definitions for each step
    // * the second section contains the implementations for each step
    //   and the implementation of the overall workflow
    // ======================================================


    // ======================================================
    // Section 1 : Define each step in the workflow using types
    // ======================================================

    // ---------------------------
    // Validation step
    // ---------------------------

    // Product validation


    type CheckProductCodeExists = ProductCode => Boolean

    // Address validation
    sealed trait AddressValidationError
    object AddressValidationError {
        final object InvalidFormat extends  AddressValidationError
        final object AddressNotFound extends  AddressValidationError
    }

    type CheckedAddress = UnvalidatedAddress

    // ---------------------------
    // Validated Order
    // ---------------------------

    final case class ValidatedOrderLine(
        orderLineId : OrderLineId,
        productCode : ProductCode,
        quantity : OrderQuantity)

    final case class ValidatedOrder(
        orderId : OrderId,
        customerInfo : CustomerInfo,
        shippingAddress : Address,
        billingAddress : Address,
        lines : List[ValidatedOrderLine])

    // ---------------------------
    // Pricing step
    // ---------------------------

    type GetProductPrice = ProductCode => Price

    // priced state is defined Domain.WorkflowTypes


    // ---------------------------
    // Send OrderAcknowledgment
    // ---------------------------

    type HtmlString = String

    final case class OrderAcknowledgment(
        emailAddress : EmailAddress,
        letter : HtmlString)

    type CreateOrderAcknowledgmentLetter =
        PricedOrder => HtmlString


    /// Send the order acknowledgement to the customer
    /// Note that this does NOT generate an Result-type error (at least not in this workflow)
    /// because on failure we will continue anyway.
    /// On success, we will generate a OrderAcknowledgmentSent event,
    /// but on failure we won't.

    sealed trait SendResult
    object SendResult {
        final object Sent extends SendResult
        final object NotSent extends SendResult
    }

    type SendOrderAcknowledgment = OrderAcknowledgment => SendResult


    // ---------------------------
    // Create events
    // ---------------------------

    type CreateEvents =
        PricedOrder// input
        => Option[OrderAcknowledgmentSent]// input (event from previous step)
        => List[PlaceOrderEvent]// output

    // ======================================================
    // Section 2 : Implementation
    // ======================================================

    // ---------------------------
    // ValidateOrder step
    // ---------------------------

    def toCustomerInfo(unvalidatedCustomerInfo : UnvalidatedCustomerInfo) : Either[ValidationError, CustomerInfo] = {
        for {
            firstName <- String50.create("FirstName", unvalidatedCustomerInfo.firstName)
            lastName <- String50.create("LastName", unvalidatedCustomerInfo.lastName)
            emailAddress <- EmailAddress.create("EmailAddress", unvalidatedCustomerInfo.emailAddress)
            customerInfo = CustomerInfo(PersonalName(firstName,lastName), emailAddress)
        } yield  customerInfo
    }

    def toAddress(checkedAddress: CheckedAddress) : Either[ValidationError, Address] = {
        for{
            addressLine1 <- String50.create("AddressLine1", checkedAddress.addressLine1)
            addressLine2 <- String50.createOption("AddressLine2", checkedAddress.addressLine2)
            addressLine3 <- String50.createOption("AddressLine3", checkedAddress.addressLine3)
            addressLine4 <- String50.createOption("AddressLine4", checkedAddress.addressLine4)
            city <- String50.create("City", checkedAddress.city)
            zipCode <- ZipCode.create("ZipCode", checkedAddress.zipCode)
        } yield Address(addressLine1, addressLine2, addressLine3, addressLine4, city, zipCode)
    }

    type CheckAddressExists =
        UnvalidatedAddress => Either[AddressValidationError, CheckedAddress]


    /// Call the checkAddressExists and convert the error to a ValidationError
    def  toCheckedAddress(checkAddressExists: CheckAddressExists, unvalidatedAddress: UnvalidatedAddress) : Either[ValidationError, CheckedAddress] = {
        checkAddressExists(unvalidatedAddress) match {
            case Left(addressValidationError) =>
                addressValidationError match {
                    case InvalidFormat => Left("Address has bad format")
                    case AddressNotFound =>  Left("Address not found")
                }
            case Right(checkAddress) => Right(checkAddress)
        }
    }

    def toOrderId(orderId: String): Either[ValidationError, OrderId] = {
        OrderId.create("OrderId", orderId)
    }

    /// Helper function for validateOrder
    def toOrderLineId(orderLineId: String): Either[ValidationError, OrderLineId] = {
        OrderLineId.create("OrderLineId", orderLineId)
    }

    /// Helper function for validateOrder
    def toProductCode(checkProductCodeExists: CheckProductCodeExists, productCode: String): Either[ValidationError, ProductCode] = {
        def checkProduct(productCode: ProductCode) : Either[ValidationError, ProductCode] = {
            if (checkProductCodeExists(productCode)) {
                Right(productCode)
            }else{
                val msg = s"Invalid $productCode"
                Left(msg)
            }
        }

        ProductCode.create("ProductCode", productCode).flatMap(code => checkProduct(code))
    }

    /// Helper function for validateOrder
    def toOrderQuantity(productCode: ProductCode, quantity: BigDecimal) : Either[ValidationError, OrderQuantity] = {
        OrderQuantity.create("OrderQuantity", productCode, quantity)
    }

    /// Helper function for validateOrder
    def toValidatedOrderLine(checkProductCodeExists: CheckProductCodeExists, unvalidatedOrderLine: UnvalidatedOrderLine) : Either[ValidationError, ValidatedOrderLine] = {
        for {
            orderLineId <- toOrderLineId(unvalidatedOrderLine.orderLineId)
            productCode <- toProductCode(checkProductCodeExists, unvalidatedOrderLine.productCode)
            quantity <- toOrderQuantity(productCode, unvalidatedOrderLine.quantity)
        } yield ValidatedOrderLine(orderLineId, productCode, quantity)
    }

    type ValidateOrder =
        CheckProductCodeExists  // dependency
            => CheckAddressExists  // dependency
            => UnvalidatedOrder    // input
            => Either[ValidationError, ValidatedOrder] // output

    def validateOrder : ValidateOrder = {
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
                validatedOrder = ValidatedOrder(orderId, customerInfo, shippingAddress, billingAddress, lines)
            } yield validatedOrder
    }

    // ---------------------------
    // PriceOrder step
    // -------

    def toPricedOrderLine(getProductPrice: GetProductPrice, validatedOrderLine: ValidatedOrderLine) : Either[PricingError, PricedOrderLine] ={
        val qty = OrderQuantity.value(validatedOrderLine.quantity)
        val price = getProductPrice(validatedOrderLine.productCode)
        val linePrice = Price.multiply(qty, price)
        linePrice match {
            case Left(value) => Left(value)
            case Right(value) => Right(PricedOrderLine(validatedOrderLine.orderLineId,validatedOrderLine.productCode, validatedOrderLine.quantity, value))
        }
    }

    type PriceOrder =
        GetProductPrice     // dependency
            => ValidatedOrder  // input
            => Either[PricingError, PricedOrder]  // output

    def priceOrder: PriceOrder = {
        getProductPrice => validatedOrder =>
        val lines = validatedOrder.lines.map(line => toPricedOrderLine(getProductPrice, line)) collect { case Right(validatedOrderLine) =>  validatedOrderLine}
        val amountToBill = BillingAmount.sumPrices(lines.map(line => line.linePrice))
        amountToBill match {
            case Left(value) => Left(value)
            case Right(billAmount) =>
                val pricedOrder = PricedOrder(validatedOrder.orderId, validatedOrder.customerInfo, validatedOrder.shippingAddress, validatedOrder.billingAddress, billAmount, lines)
                Right(pricedOrder)
        }
    }

    type AcknowledgeOrder =
        CreateOrderAcknowledgmentLetter  // dependency
            => SendOrderAcknowledgment      // dependency
            => PricedOrder                  // input
            => Option[OrderAcknowledgmentSent] // output

    // ---------------------------
    // AcknowledgeOrder step
    // ---------------------------
    def acknowledgeOrder : AcknowledgeOrder = {
        createOrderAcknowledgmentLetter => sendOrderAcknowledgment => pricedOrder =>
        val letter = createOrderAcknowledgmentLetter(pricedOrder)
        val acknowledgment = OrderAcknowledgment(pricedOrder.customerInfo.emailAddress, letter)

        // if the acknowledgement was successfully sent,
        // return the corresponding event, else return None
        sendOrderAcknowledgment(acknowledgment) match {
            case Sent =>
                val event = OrderAcknowledgmentSent(pricedOrder.orderId, pricedOrder.customerInfo.emailAddress)
                Some(event)
            case NotSent => None
        }
    }

    // ---------------------------
    // Create events
    // ---------------------------

    def createOrderPlacedEvent(placedOrder: PricedOrder) : OrderPlaced = {
        placedOrder
    }

    def createBillingEvent(placeOrder: PricedOrder) : Option[BillableOrderPlaced] = {
        val billingAmount = placeOrder.amountToBill
        if(billingAmount > 0) {
            Some(BillableOrderPlaced(placeOrder.orderId, placeOrder.billingAddress, placeOrder.amountToBill))
        }else{
            None
        }
    }

    /// helper to convert an Option into a List
    def listOfOption[A](option: Option[A]) : List[A] = {
        option match {
            case Some(value) => List(value)
            case None =>List()
        }
    }

    def createEvents(pricedOrder: PricedOrder, orderAcknowledgmentSent: Option[OrderAcknowledgmentSent]): List[PlaceOrderEvent] = {
        val orderPlacedEvents = List(OrderPlacedEvent(createOrderPlacedEvent(pricedOrder)))
        val acknowledgmentEvents = listOfOption(orderAcknowledgmentSent.map(order => AcknowledgmentSentEvent(order)))
        val billingEvents = listOfOption(createBillingEvent(pricedOrder).map(bill => BillableOrderPlacedEvent(bill)))
        for {
            result <- List(orderPlacedEvents, acknowledgmentEvents, billingEvents).flatten
        } yield result
    }


    // ---------------------------
    // overall workflow
    // ---------------------------
    def placeOrder: PlaceOrder = {
        unvalidatedOrder =>
            validateOrder(checkProductExists : CheckProductCodeExists) (checkAddressExists : CheckAddressExists) (unvalidatedOrder : UnvalidatedOrder) match {
                case Left(validationError) => Left(Validation(validationError))
                case Right(validatedOrder) =>
                    priceOrder(getProductPrice : GetProductPrice) (validatedOrder: ValidatedOrder) match {
                        case Left(pricingError) => Left(Pricing(pricingError))
                        case Right(pricedOrder) =>
                            val acknowledgementOption = acknowledgeOrder(createOrderAcknowledgmentLetter : CreateOrderAcknowledgmentLetter)(sendOrderAcknowledgment: SendOrderAcknowledgment) (pricedOrder: PricedOrder)
                            val events = createEvents(pricedOrder,acknowledgementOption)
                            Right(events)
                    }
            }
        }
}
