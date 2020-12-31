import CompoundTypes.{Address, CustomerInfo, PersonalName}
import PublicTypes.PlaceOrderError.{Pricing, RemoteService, Validation}
import PublicTypes.PlaceOrderEvent.{AcknowledgmentSentEvent, BillableOrderPlacedEvent, OrderPlacedEvent}
import PublicTypes._
import SimpleTypes.{OrderQuantity, ProductCode}

object PlaceOrderDTO {

    object Utils {

        def defaultIfNone[A](defaultValue: A, option: Option[A]) : A = option match {
                case Some(value) => value
                case None => defaultValue
            }
    }

    final case class CustomerInfoDto(firstName : String, lastName : String, emailAddress : String)

    object CustomerInfoDto {
        def toUnvalidatedCustomerInfo (dto: CustomerInfoDto) : UnvalidatedCustomerInfo =
            UnvalidatedCustomerInfo(dto.firstName, dto.lastName, dto.emailAddress)

        def toCustomerInfo (dto:CustomerInfoDto) :Either[String, CustomerInfo] =
            for {
                first <- String50.create("FirstName", dto.firstName)
                last <- String50.create("LastName", dto.lastName)
                email <- EmailAddress.create("EmailAddress", dto.emailAddress)
                name = PersonalName(first,last)
                info = CustomerInfo(name, email)
            } yield info

        def fromCustomerInfo (domainObj: CustomerInfo) :CustomerInfoDto =
            CustomerInfoDto(domainObj.name.firstName, domainObj.name.lastName, domainObj.emailAddress)
    }

    final case class AddressDto(
        addressLine1 : String,
        addressLine2 : String,
        addressLine3 : String,
        addressLine4 : String,
        city : String,
        zipCode : String)

    object AddressDto {

        def toUnvalidatedAddress(dto: AddressDto): UnvalidatedAddress =
            UnvalidatedAddress(dto.addressLine1, dto.addressLine2, dto.addressLine3, dto.addressLine4, dto.city, dto.zipCode)

        def toAddress(dto: AddressDto) :Either[String, Address] =
            for {
                addressLine1 <- String50.create("AddressLine1", dto.addressLine1)
                addressLine2 <- String50.createOption("AddressLine2", dto.addressLine2)
                addressLine3 <- String50.createOption("AddressLine3", dto.addressLine3)
                addressLine4 <- String50.createOption("AddressLine4", dto.addressLine4)
                city <- String50.create("City", dto.city)
                zipCode <- ZipCode.create("ZipCode", dto.zipCode)
                address = Address(addressLine1, addressLine2, addressLine3, addressLine4, city, zipCode)
            } yield address

        def fromAddress(domainObj: Address) : AddressDto = {
            val addressLine2 =  Utils.defaultIfNone(null, domainObj.addressLine2)
            val addressLine3 =  Utils.defaultIfNone(null, domainObj.addressLine3)
            val addressLine4 =  Utils.defaultIfNone(null, domainObj.addressLine4)
            AddressDto(domainObj.addressLine1, addressLine2, addressLine3, addressLine4, domainObj.city, domainObj.zipCode)
        }
    }

    final case class OrderFormLineDto(orderLineId : String, productCode : String, quantity : BigDecimal)

    object OrderLineDto {
        def toUnvalidatedOrderLine(dto: OrderFormLineDto) : UnvalidatedOrderLine = {
            UnvalidatedOrderLine(dto.orderLineId, dto.productCode, dto.quantity)
        }
    }

    final case class PricedOrderLineDto(
        orderLineId : String,
        productCode : String,
        quantity : BigDecimal,
        linePrice : BigDecimal)

    object PricedOrderLineDto {
        def fromDomain(domainObj: PricedOrderLine): PricedOrderLineDto = {
            val productCode = ProductCode.value(domainObj.productCode)
            val quantity = OrderQuantity.value(domainObj.quantity)
            PricedOrderLineDto(domainObj.orderLineId, productCode, quantity, domainObj.linePrice)
        }
    }

    final case class OrderFormDto(
        orderId : String,
        customerInfo : CustomerInfoDto,
        shippingAddress : AddressDto,
        billingAddress : AddressDto,
        lines : List[OrderFormLineDto])

    object OrderFormDto {
        def toUnvalidatedOrder(dto: OrderFormDto) : UnvalidatedOrder = {
            val customerInfo = CustomerInfoDto.toUnvalidatedCustomerInfo(dto.customerInfo)
            val shippingAddress = AddressDto.toUnvalidatedAddress(dto.shippingAddress)
            val billingAddress = AddressDto.toUnvalidatedAddress(dto.billingAddress)
            val lines = dto.lines.map(line => OrderLineDto.toUnvalidatedOrderLine(line))
            UnvalidatedOrder(dto.orderId, customerInfo,shippingAddress, billingAddress, lines)
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

    type PlaceOrderEventDto = Map[String, Object]

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
