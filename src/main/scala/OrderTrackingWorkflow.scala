import ValidationStep.{validatedOrder, _}
import Domain._
import AcknowledgmentStep._
import Extensions._

object OrderTrackingWorkflow {
    // Bounded context : Ordering - Taking
    
    // Data order =
        // CustomerInfo
        // ShippingAddress
        // Billing Address
        // List of Order lines
        // Amount to Bill


    // Data OrderLine=
        // Product
        // Quantity
        // Price


    // Data CustomerInfo = ??
    // Data BillingAddress = ??

    // Context : Order Taking

    // Data WidgetCode = string starting with W then 4 digits
    // Data GizmoCode =string starting with G then 3 digits
    // Data ProductCode = WidgetCode or GizmoCode

    // Data Order Quantity = UnitQuantity OR KilogramQuantity
    // Data UnitQuantity= integer between 1 and 1000
    // Data KilogramQuantity = decimal between 0.05 and 100.0


    // Life Cycle of an Order

    // Data: UnvalidatedOrder =
        // UnvalidatedCustomerInfo
        // UnvalidatedShippingAddress
        // UnvalidatedBillingAddress
        // List of UnvalidatedOrderLines

    // Data : UnvalidatedOrderLines =
        // UnvalidatedProductCode
        // UnvalidatedOrderQuantity

    // Data: ValidatedOrder =
        // ValidatedCustomerInfo
        // ValidatedShippingAddress
        // ValidatedBillingAddress
        // List of ValidatedOrderLines

    // Data : UnvalidatedOrderLines =
        // ValidatedProductCode
        // ValidatedOrderQuantity


    // Data: PricedOrder =
        // ValidatedCustomerInfo
        // ValidatedShippingAddress
        // ValidatedBillingAddress
        // list of PricedOrderLine
        // AmountToBill

    // Data PricedOrderLine =
        // ValidatedOrderLine
        // LinePrice

    // Data: PlacedOrderAcknowledgment =
        // PricedOrder
        // AcknowledgmentLetter

    // Workflow : Placed Order
    // Input: Order form
    // Output :
        // Order Placed event (put on a pile to send to send to the other teams)
        // Or InvalidOrder (put on appropriate pile)
    // Step 1
        // do ValidatedOrder
        // If order is Invalid then
            // add InvalidOrder to the pile
            // stop
    // Step 2
    // Price Order

    // Step 3
    // Do AcknowledgmentToCustomer

    // Step 4
    // Returns OrderPlaced event if no errors

    def placeOrderWorkflow(unvalidatedOrder: UnvalidatedOrder) : Unit = {
        val orderValidated = validatedOrder(unvalidatedOrder)
        val pricedOrder = priceOrder(orderValidated)
        val acknowledgmentOption = acknowledgmentOrder(pricedOrder)
        val events = createEvents(pricedOrder, acknowledgmentOption)
        println(events)
    }


    // SubStep: validatedOrder
    // input: UnvalidatedOrder
    // output: ValidatedOrder or ValidationError
    // dependencies: Check Product code exists , check Address exists

    // Validate the customer name
    // Check that the shipping and the billing address exists

    // For each line:
        // checks the product code syntax
        // Check the product code exists in the Product catalog

    // If everything is Ok the
        // Returns ValidatedOrder
    // else
        // returns ValidationError

    // SubStep PriceOrder
    // input : ValidatedOrder
    // dependencies : Get product price


    // for each line :
        // Get the price for the product
        // Set the price for the line

    // Set the amount of the bill ( = sum of the line prices)


    // SubStep: Send AcknowledgmentToCustomer
    // input : Priced Order
    // Output : None
    // Create acknowledgment letter and send it
    // and priced oder to the customer


    // Data: BillableOrderPlaced =
        // OrderId
        // BillingAddress
        // AmountToBill

    // Workflow Categorize Inbound Mail
    // input: Envelop contents
    // output: Quote from(put on appropriate pile)
    // Order from(put on appropriate pile)

//    val anOrderAtyInUnits = OrderQuantity.UnitQty(123)
//    val anOrderQtyInKg = Kilos(2.5)
//
//    def printQuantity(aOrderQty: OrderQuantity): Unit = aOrderQty match {
//        case OrderQuantity.UnitQty(quantity) =>
//            println(quantity)
//        case Kilos(kg) =>
//            println(kg)
//    }
//
//    printQuantity(anOrderAtyInUnits)
//    printQuantity(anOrderQtyInKg)

    //type ValidationResponse[T] = Future[Either[T,  List[ValidationError]]]
    //type ValidateOrder = UnvalidatedOrder => ValidationResponse[ValidateOrder]

//    val widgetCode = Widget("W1234")
//    val widgetCode2 = Widget("W1434")
//
//    println(widgetCode == widgetCode2)
//
//    val invoice = Paid(PaidInvoice(id = "12345"))
//
//    def printInvoice(invoice: Invoice): Unit = invoice match {
//        case Paid(paidInvoice) =>
//            println(s"The paid invoiceId is ${paidInvoice.id}")
//        case Unpaid(unpaidInvoice) =>
//            println(s"The paid invoiceId is ${unpaidInvoice.id}")
//    }
//
//    printInvoice(invoice)
//
//    val result = 123.validateOrderQuantity
//    val kilosResult = BigDecimal(2.5).validateOrderQuantityKilos
//
//    def showQty[T](qty: Either[T, String]): Unit = {
//        qty match {
//            case Right(error) => println(error)
//            case Left(qty) => println(qty)
//        }
//    }
//
//    showQty(result)
//    showQty(kilosResult)

}
