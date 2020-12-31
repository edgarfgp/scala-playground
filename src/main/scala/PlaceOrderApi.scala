import InternalTypes.{CalculateShippingCost, CheckAddressExists, CheckProductCodeExists, CreateOrderAcknowledgmentLetter, GetProductPrice, SendOrderAcknowledgment}
import PlaceOrderDTO.OrderFormDto.toUnvalidatedOrder
import PlaceOrderDTO.{OrderFormDto, PlaceOrderErrorDto, PlaceOrderEventDto}
import PlaceOrderImplementation._
import PublicTypes.{PlaceOrderError, PlaceOrderEvent}
import SimpleTypes.JsonString
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

    private def runWorkflow =
        PlaceOrderImplementation.placeOrder(
            checkProductExists,
            checkAddressExists,
            getProductPrice,
            calculateShippingCost,
            createOrderAcknowledgmentLetter,
            sendOrderAcknowledgment)

    def placeOrderApi(request: HttpRequest): HttpResponse = {
        val orderFormJson = request.body
        val unvalidatedOrder =
            decode[OrderFormDto](orderFormJson) match {
                case Left(errorMessage) => throw new Exception(errorMessage)
                case Right(orderFrom) =>
                    toUnvalidatedOrder(orderFrom)
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
