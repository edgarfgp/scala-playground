import CompoundTypes._
import InternalTypes._
import PublicTypes.PlaceOrderError._
import PublicTypes.PlaceOrderEvent._
import PublicTypes._
import SimpleTypes.PricedOrderLine._
import SimpleTypes._

object PlaceOrderDTO {

    object Utils {

        def defaultIfNone[A](defaultValue: A, option: Option[A]) : A = option match {
                case Some(value) => value
                case None => defaultValue
            }

        def listOfOption[A](option: Option[A]) : List[A] = option match {
            case Some(value) => List(value)
            case None =>List()
        }
    }

    final case class CustomerInfoDto(firstName : String, lastName : String, emailAddress : String, vipStatus: String)

    object CustomerInfoDto {

        def toUnvalidatedCustomerInfo (dto: CustomerInfoDto) : UnvalidatedCustomerInfo =
            UnvalidatedCustomerInfo(dto.firstName, dto.lastName, dto.emailAddress, dto.vipStatus)

        def toCustomerInfo (dto:CustomerInfoDto) :Either[String, CustomerInfo] =
            for {
                first <- String50.create("firstName", dto.firstName)
                last <- String50.create("lastName", dto.lastName)
                email <- EmailAddress.create("emailAddress", dto.emailAddress)
                name = PersonalName(first,last)
                vipStatus <- VipStatus.create("vipStatus", dto.vipStatus)
                info = CustomerInfo(name, email, vipStatus)
            } yield info

        def fromCustomerInfo (domainObj: CustomerInfo) :CustomerInfoDto = {
            val vipStatus = VipStatus.value(domainObj.vipStatus)
            CustomerInfoDto(domainObj.name.firstName, domainObj.name.lastName, domainObj.emailAddress, vipStatus)
        }
    }

    final case class AddressDto(
        addressLine1 : String,
        addressLine2 : String,
        addressLine3 : String,
        addressLine4 : String,
        city : String,
        zipCode : String,
        country: String,
        state: UsStateCode)

    object AddressDto {

        def toUnvalidatedAddress(dto: AddressDto): UnvalidatedAddress =
            UnvalidatedAddress(dto.addressLine1,
                dto.addressLine2,
                dto.addressLine3,
                dto.addressLine4,
                dto.city,
                dto.zipCode,
                dto.country,
                dto.state)

        def toAddress(dto: AddressDto) :Either[String, Address] =
            for {
                addressLine1 <- String50.create("addressLine1", dto.addressLine1)
                addressLine2 <- String50.createOption("addressLine2", dto.addressLine2)
                addressLine3 <- String50.createOption("addressLine3", dto.addressLine3)
                addressLine4 <- String50.createOption("addressLine4", dto.addressLine4)
                city <- String50.create("city", dto.city)
                zipCode <- ZipCode.create("zipCode", dto.zipCode)
                country <-  String50.create("country", dto.country)
                state <- UsStateCode.create("state", dto.state)
                address = Address(addressLine1, addressLine2, addressLine3, addressLine4, city, zipCode, country, state)
            } yield address

        def fromAddress(domainObj: Address) : AddressDto = {
            val addressLine2 =  Utils.defaultIfNone(null, domainObj.addressLine2)
            val addressLine3 =  Utils.defaultIfNone(null, domainObj.addressLine3)
            val addressLine4 =  Utils.defaultIfNone(null, domainObj.addressLine4)
            AddressDto(domainObj.addressLine1,
                addressLine2,
                addressLine3,
                addressLine4,
                domainObj.city,
                domainObj.zipCode,
                domainObj.country,
                domainObj.state)
        }
    }

    final case class OrderFormLineDto(orderLineId : String, productCode : String, quantity : BigDecimal)

    object OrderLineDto {
        def toUnvalidatedOrderLine(dto: OrderFormLineDto) : UnvalidatedOrderLine =
            UnvalidatedOrderLine(dto.orderLineId, dto.productCode, dto.quantity)
    }

    final case class PricedOrderLineDto(
        orderLineId : String,
        productCode : String,
        quantity : BigDecimal,
        linePrice : BigDecimal,
        comment : String)

    object PricedOrderLineDto {
        def fromDomain(domainObj: PricedOrderLine): PricedOrderLineDto =
            domainObj match {
                case ProductLine(productLine) =>
                    val productCode = ProductCode.value(productLine.productCode)
                    val quantity = OrderQuantity.value(productLine.quantity)
                    PricedOrderLineDto(productLine.orderLineId, productCode, quantity, productLine.linePrice, "")
                case CommentLine(comment) =>
                    PricedOrderLineDto(null, null, 0.0, 0.0, comment)
            }

    }

    final case class OrderFormDto(
        orderId : String,
        customerInfo : CustomerInfoDto,
        shippingAddress : AddressDto,
        billingAddress : AddressDto,
        lines : List[OrderFormLineDto],
        promotionCode: String)

    object OrderFormDto {

        def toUnvalidatedOrder(dto: OrderFormDto) : UnvalidatedOrder = {
            val customerInfo = CustomerInfoDto.toUnvalidatedCustomerInfo(dto.customerInfo)
            val shippingAddress = AddressDto.toUnvalidatedAddress(dto.shippingAddress)
            val billingAddress = AddressDto.toUnvalidatedAddress(dto.billingAddress)
            val lines = dto.lines.map(line => OrderLineDto.toUnvalidatedOrderLine(line))
            val promotionCode = dto.promotionCode
            UnvalidatedOrder(dto.orderId, customerInfo,shippingAddress, billingAddress, lines, promotionCode)
        }

    }

    final case class OrderPlacedDto(
        orderId : String,
        customerInfo : CustomerInfoDto,
        shippingAddress : AddressDto,
        billingAddress : AddressDto,
        amountToBill : BigDecimal,
        lines : List[PricedOrderLineDto])

    object OrderPlacedDto {

        def fromDomain(domainObj: PricedOrder) : OrderPlacedDto = {
            val customerInfo = CustomerInfoDto.fromCustomerInfo(domainObj.customerInfo)
            val shippingAddress = AddressDto.fromAddress(domainObj.shippingAddress)
            val billingAddress = AddressDto.fromAddress(domainObj.billingAddress)
            val lines = domainObj.lines.map(line => PricedOrderLineDto.fromDomain(line))
            OrderPlacedDto(domainObj.orderId, customerInfo, shippingAddress, billingAddress, domainObj.amountToBill, lines)
        }
    }

    final case class BillableOrderPlacedDto(
        orderId : String,
        billingAddress: AddressDto,
        amountToBill : BigDecimal)

    object BillableOrderPlacedDto {

        def fromDomain(domainObj: BillableOrderPlaced) : BillableOrderPlacedDto = {
            val billingAddress = AddressDto.fromAddress(domainObj.billingAddress)
            BillableOrderPlacedDto(domainObj.orderId, billingAddress, domainObj.amountToBill)
        }
    }

    final case class OrderAcknowledgmentSentDto(orderId : String, emailAddress : String)

    object OrderAcknowledgmentSentDto {

        def fromDomain(domainObj:OrderAcknowledgmentSent) : OrderAcknowledgmentSentDto =
            OrderAcknowledgmentSentDto(domainObj.orderId, domainObj.emailAddress)
    }

    object PlaceOrderEventDto {

        def fromDomain(domainObj: PlaceOrderEvent) : PlaceOrderEventDto = domainObj match {
                case OrderPlacedEvent(orderPlaced) =>
                    val value = OrderPlacedDto.fromDomain(orderPlaced)
                    val key = "OrderPlacedEvent"
                    Map(key-> value)
                case BillableOrderPlacedEvent(billableOrderPlaced) =>
                    val value = BillableOrderPlacedDto.fromDomain(billableOrderPlaced)
                    val key = "BillableOrderPlacedEvent"
                    Map(key -> value)
                case AcknowledgmentSentEvent(orderAcknowledgmentSent) =>
                    val value = OrderAcknowledgmentSentDto.fromDomain(orderAcknowledgmentSent)
                    val key = "AcknowledgmentSentEvent"
                    Map(key -> value)
            }
    }

    final case class PlaceOrderErrorDto(code : String, message : String)

    object PlaceOrderErrorDto {

        def fromDomain(domainObj: PlaceOrderError) : PlaceOrderErrorDto = domainObj match {
                case Validation(validationError) =>
                    PlaceOrderErrorDto("ValidationError", validationError)
                case Pricing(pricingError) =>
                    PlaceOrderErrorDto("PricingError", pricingError)
                case RemoteService(remoteServiceError) =>
                    val message = s"${remoteServiceError.service.name} ${remoteServiceError.exception.getMessage}"
                    PlaceOrderErrorDto("RemoteServiceError", message)
        }
    }
}