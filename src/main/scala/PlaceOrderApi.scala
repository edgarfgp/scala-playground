import PlaceOrderDTO.{OrderFormDto, PlaceOrderErrorDto, PlaceOrderEventDto}
import PlaceOrderImplementation.SendResult.Sent
import PlaceOrderImplementation._
import PublicTypes.{PlaceOrderError, PlaceOrderEvent}
import io.circe.generic.auto._
import io.circe.parser._

object PlaceOrderApi {
    type JsonString = String

    final case class HttpRequest(action : String, uri : String, body : JsonString)

    final case class HttpResponse(httpStatusCode : Integer, body : JsonString)

    type PlaceOrderApi = HttpRequest => HttpResponse

    def checkProductExists : CheckProductCodeExists = _ => true

    def checkAddressExists : CheckAddressExists = unvalidatedAddress => Right(unvalidatedAddress)

    def getProductPrice: PlaceOrderImplementation.GetProductPrice = _ => Price.unsafeCreate(1000000)

    def createOrderAcknowledgmentLetter : CreateOrderAcknowledgmentLetter = _ =>  "some text"

    def sendOrderAcknowledgment: SendOrderAcknowledgment = _ => Sent

    def workflowResultToHttpResponse(result : Either[PlaceOrderError, List[PlaceOrderEvent]]): HttpResponse = result match {
            case Right(events) =>
                val placeOrderEvents = events.map(e => PlaceOrderEventDto.fromDomain(e))
                HttpResponse(200, placeOrderEvents.mkString("\n"))
            case Left(error) =>
                val responseError = PlaceOrderErrorDto.fromDomain(error)
                HttpResponse(401, responseError.message)
        }

    def placeOrderApi : PlaceOrderApi = {
        request =>
            val orderFormJson = request.body
            val unvalidatedOrder =
                decode[OrderFormDto](orderFormJson) match {
                    case Left(errorMessage) => throw new Exception(errorMessage)
                    case Right(orderFrom) =>
                        OrderFormDto.toUnvalidatedOrder(orderFrom)
                }

            val workflow =
                placeOrder
                checkProductExists
                checkAddressExists
                getProductPrice
                createOrderAcknowledgmentLetter
                sendOrderAcknowledgment

            workflowResultToHttpResponse(workflow(unvalidatedOrder))
    }
}
