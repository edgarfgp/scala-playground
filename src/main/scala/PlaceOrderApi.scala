import PlaceOrderDTO.{PlaceOrderErrorDto, PlaceOrderEventDto}
import PlaceOrderImplementation._
import PlaceOrderImplementation.SendResult.Sent
import PublicTypes.{PlaceOrderError, PlaceOrderEvent}

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
            case Left(error) =>
                val dto = PlaceOrderErrorDto.fromDomain(error)
                // FIXME Add Deserialization here
                //let json = JsonConvert.SerializeObject(dtos)
                val json = dto.message
                val response = HttpResponse(401, "")
                response
            case Right(events) =>
                val result = events.map(e => PlaceOrderEventDto.fromDomain(e)).toArray
                // FIXME Add Deserialization here
                //let json = JsonConvert.SerializeObject(dto )
                val response = HttpResponse(200, "")
                response
        }
    }

    def placeOrderApi : PlaceOrderApi = {
        request =>
            // following the approach in "A Complete Serialization Pipeline" in chapter 11

            // start with a string
            //let orderFormJson = request.Body
            //let orderForm = JsonConvert.DeserializeObject<OrderFormDto>(orderFormJson)
            // convert to domain object
            //let unvalidatedOrder = orderForm |> OrderFormDto.toUnvalidatedOrder

            // setup the dependencies. See "Injecting Dependencies" in chapter 9
            //let workflow =
                //Implementation.placeOrder
            checkProductExists // dependency
            checkAddressExists // dependency
            getProductPrice    // dependency
            createOrderAcknowledgmentLetter  // dependency
            sendOrderAcknowledgment // dependency

            // now we are in the pure domain
            //let asyncResult = workflow unvalidatedOrder

            // now convert from the pure domain back to a HttpResponse
            //asyncResult
            //|> Async.map (workflowResultToHttpResponse)
            ???

    }
}
