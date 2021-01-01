import InternalTypes._
import PlaceOrderDTO._
import PublicTypes._
import SimpleTypes._
import io.circe.generic.auto._
import io.circe.parser._

object PlaceOrderApi {

    final case class HttpRequest(action : String, uri : String, body : JsonString)

    final case class HttpResponse(httpStatusCode : Integer, body : JsonString)

    def checkProductExists: CheckProductCodeExists = PlaceOrderImplementation.checkProductExists

    def checkAddressExists: CheckAddressExists = PlaceOrderImplementation.checkAddressExists

    def getProductPrice: GetProductPrice = PlaceOrderImplementation.getProductPrice

    def createOrderAcknowledgmentLetter : CreateOrderAcknowledgmentLetter = PlaceOrderImplementation.createOrderAcknowledgmentLetter

    def sendOrderAcknowledgment: SendOrderAcknowledgment = PlaceOrderImplementation.sendOrderAcknowledgment

    def calculateShippingCost : CalculateShippingCost = PlaceOrderImplementation.calculateShippingCost

    def getPromotionPrices(promotionCode: PromotionCode): TryGetProductPrice = {
            def halfPricePromotion : TryGetProductPrice = {
                productCode =>
                    if(ProductCode.value(productCode) == "ONSALE") Some(Price.unsafeCreate(5.0))
                    else None
            }

            def quarterPricePromotion : TryGetProductPrice = {
                productCode =>
                    if(ProductCode.value(productCode) == "ONSALE") Some(Price.unsafeCreate(2.5))
                    else None
            }

            def noPromotion : TryGetProductPrice = productCode => None

            promotionCode match {
                case "HALF" => halfPricePromotion
                case "QUARTER" => quarterPricePromotion
                case _ => noPromotion
            }
    }

    def getStandardPrices: GetProductPrice =
        productCode => Price.unsafeCreate(10.0)

    def getPricingFunction : GetPricingFunction =
        PricingModule.getPricingFunction(_ => getStandardPrices, productCode => getPromotionPrices(productCode))

    private def runWorkflow =
        PlaceOrderImplementation.placeOrder(
            checkProductExists,
            checkAddressExists,
            getPricingFunction,
            calculateShippingCost,
            createOrderAcknowledgmentLetter,
            sendOrderAcknowledgment)

    def placeOrderApi(request: HttpRequest): HttpResponse = {
        val orderFormJson = request.body
        val unvalidatedOrder =
            decode[OrderFormDto](orderFormJson) match {
                case Left(errorMessage) => throw new Exception(errorMessage)
                case Right(orderFrom) =>
                    OrderFormDto.toUnvalidatedOrder(orderFrom)
            }

        val workflowResult = runWorkflow(unvalidatedOrder)
        processWorkflowResult(workflowResult)
    }
    private def processWorkflowResult(result : Either[PlaceOrderError, List[PlaceOrderEvent]]): HttpResponse = result match {
        case Right(events) =>
            val placeOrderEvents = events.map(e => PlaceOrderEventDto.fromDomain(e))
            HttpResponse(200, placeOrderEvents.mkString("\n"))
        case Left(error) =>
            val responseError = PlaceOrderErrorDto.fromDomain(error)
            HttpResponse(401, responseError.message)
    }
}
