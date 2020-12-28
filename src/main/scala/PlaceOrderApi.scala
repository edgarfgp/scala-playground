import PlaceOrderDTO.{OrderFormDto, PlaceOrderErrorDto, PlaceOrderEventDto}
import PlaceOrderImplementation.SendResult.Sent
import PlaceOrderImplementation._
import PublicTypes.{PlaceOrderError, PlaceOrderEvent}
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.io.Source

object PlaceOrderApi {
    // ======================================================
    // This file contains the JSON API interface to the PlaceOrder workflow
    //
    // 1) The HttpRequest is turned into a DTO, which is then turned into a Domain object
    // 2) The main workflow function is called
    // 3) The output is turned into a DTO which is turned into a HttpResponse
    // ======================================================

    type JsonString = String

    /// Very simplified version!
    final case class HttpRequest(
        action : String,
        uri : String,
        body : JsonString)

    /// Very simplified version!
    final case class HttpResponse(
        httpStatusCode : Integer,
        body : JsonString)

    /// An API takes a HttpRequest as input and returns a async response
    type PlaceOrderApi = HttpRequest => HttpResponse

    // =============================
    // Implementation
    // =============================

    // setup dummy dependencies
    def checkProductExists : CheckProductCodeExists = {
        productCode => true
    }

    def checkAddressExists : CheckAddressExists = {
        unvalidatedAddress => Right(unvalidatedAddress)
    }

    def getProductPrice: PlaceOrderImplementation.GetProductPrice = {
        productCode => Price.unsafeCreate(1000000)
    }

    def createOrderAcknowledgmentLetter : CreateOrderAcknowledgmentLetter = {
        val letterTest = "some text"
        priceOrder => letterTest
    }

    def sendOrderAcknowledgment: SendOrderAcknowledgment =
        orderAcknowledgement => Sent

    // -------------------------------
    // workflow
    // -------------------------------
    def workflowResultToHttpResponse(result : Either[PlaceOrderError, List[PlaceOrderEvent]]) : HttpResponse = {
        result match {
            case Right(events) =>
                val placeOrderEvents = events.map(e => PlaceOrderEventDto.fromDomain(e))
                HttpResponse(200, placeOrderEvents.mkString("\n"))
            case Left(error) =>
                val responseError = PlaceOrderErrorDto.fromDomain(error)
                HttpResponse(401, responseError.message)
        }
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

            workflowResultToHttpResponse(workflow.apply(unvalidatedOrder))
    }
}
