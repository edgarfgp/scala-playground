import CompoundTypes.{Address, CustomerInfo, PersonalName}
import PublicTypes.PlaceOrderError.{Pricing, RemoteService, Validation}
import PublicTypes.PlaceOrderEvent.{AcknowledgmentSentEvent, BillableOrderPlacedEvent, OrderPlacedEvent}
import PublicTypes._
import SimpleTypes.{OrderQuantity, ProductCode}
import io.circe.Decoder
import io.circe.generic.semiauto.deriveDecoder

object PlaceOrderDTO {

    object Utils {
        // Helper function to get the value from an Option, and if None, use the defaultValue
        /// Note that the defaultValue is the first parameter, unlike the similar `defaultArg`
        def defaultIfNone[A](defaultValue: A, option: Option[A]) : A = {
            option match {
                case Some(value) => value
                case None => defaultValue
            }
        }
    }


    //===============================================
    // DTO for CustomerInfo
    //===============================================

    final case class CustomerInfoDto(firstName : String, lastName : String, emailAddress : String)

    /// Functions for converting between the DTO and corresponding domain object
    object CustomerInfoDto {
        /// Convert the DTO into a UnvalidatedCustomerInfo object.
        /// This always succeeds because there is no validation.
        /// Used when importing an OrderForm from the outside world into the domain.
        def toUnvalidatedCustomerInfo (dto: CustomerInfoDto) : UnvalidatedCustomerInfo = {
            val domainObj = UnvalidatedCustomerInfo(dto.firstName, dto.lastName, dto.emailAddress)
            domainObj
        }

        /// Convert the DTO into a CustomerInfo object
        /// Used when importing from the outside world into the domain, eg loading from a database
        def toCustomerInfo (dto:CustomerInfoDto) :Either[String, CustomerInfo] = {
            for {
                first <- String50.create("FirstName", dto.firstName)
                last <- String50.create("LastName", dto.lastName)
                email <- EmailAddress.create("EmailAddress", dto.emailAddress)
                name = PersonalName(first,last)
                info = CustomerInfo(name, email)
            } yield info
        }

        /// Convert a CustomerInfo object into the corresponding DTO.
        /// Used when exporting from the domain to the outside world.
        def fromCustomerInfo (domainObj: CustomerInfo) :CustomerInfoDto = {
            CustomerInfoDto(domainObj.name.firstName, domainObj.name.lastName, domainObj.emailAddress)
        }
    }

    //===============================================
    // DTO for Address
    //===============================================

    final case class AddressDto(
        addressLine1 : String,
        addressLine2 : String,
        addressLine3 : String,
        addressLine4 : String,
        city : String,
        zipCode : String)

    object AddressDto {
        /// Convert the DTO into a UnvalidatedAddress
        /// This always succeeds because there is no validation.
        /// Used when importing an OrderForm from the outside world into the domain.
        def toUnvalidatedAddress(dto: AddressDto): UnvalidatedAddress = {
            UnvalidatedAddress(dto.addressLine1, dto.addressLine2, dto.addressLine3, dto.addressLine4, dto.city, dto.zipCode)
        }

        /// Convert the DTO into a Address object
        /// Used when importing from the outside world into the domain, eg loading from a database.
        def toAddress(dto: AddressDto) :Either[String, Address] = {
            for {
                addressLine1 <- String50.create("AddressLine1", dto.addressLine1)
                addressLine2 <- String50.createOption("AddressLine2", dto.addressLine2)
                addressLine3 <- String50.createOption("AddressLine3", dto.addressLine3)
                addressLine4 <- String50.createOption("AddressLine4", dto.addressLine4)
                city <- String50.create("City", dto.city)
                zipCode <- ZipCode.create("ZipCode", dto.zipCode)
                address = Address(addressLine1, addressLine2, addressLine3, addressLine4, city, zipCode)
            } yield address
        }

        /// Convert a Address object into the corresponding DTO.
        /// Used when exporting from the domain to the outside world.
        def fromAddress(domainObj: Address) : AddressDto = {
            val addressLine2 =  Utils.defaultIfNone(null, domainObj.addressLine2)
            val addressLine3 =  Utils.defaultIfNone(null, domainObj.addressLine3)
            val addressLine4 =  Utils.defaultIfNone(null, domainObj.addressLine4)
            AddressDto(domainObj.addressLine1, addressLine2, addressLine3, addressLine4, domainObj.city, domainObj.zipCode)
        }
    }

    //===============================================
    // DTOs for OrderLines
    //===============================================

    /// From the order form used as input
    final case class OrderFormLineDto(
        orderLineId : String,
        productCode : String,
        quantity : BigDecimal)


    object OrderLineDto {
        /// Convert the OrderFormLine into a UnvalidatedOrderLine
        /// This always succeeds because there is no validation.
        /// Used when importing an OrderForm from the outside world into the domain.
        def toUnvalidatedOrderLine(dto: OrderFormLineDto) : UnvalidatedOrderLine = {
            UnvalidatedOrderLine(dto.orderLineId, dto.productCode, dto.quantity)
        }
    }


    //===============================================
    // DTOs for PricedOrderLines
    //===============================================

    /// Used in the output of the workflow
    final case class PricedOrderLineDto(
        orderLineId : String,
        productCode : String,
        quantity : BigDecimal,
        linePrice : BigDecimal)

    object PricedOrderLineDto {
        /// Convert a PricedOrderLine object into the corresponding DTO.
        /// Used when exporting from the domain to the outside world.
        def fromDomain(domainObj: PricedOrderLine): PricedOrderLineDto = {
            val productCode = ProductCode.value(domainObj.productCode)
            val quantity = OrderQuantity.value(domainObj.quantity)
            PricedOrderLineDto(domainObj.orderLineId, productCode, quantity, domainObj.linePrice)
        }
    }

    //===============================================
    // DTO for OrderForm
    //===============================================

    final case class OrderFormDto(
        orderId : String,
        customerInfo : CustomerInfoDto,
        shippingAddress : AddressDto,
        billingAddress : AddressDto,
        lines : List[OrderFormLineDto])

    object OrderFormDto {
        /// Convert the OrderForm into a UnvalidatedOrder
        /// This always succeeds because there is no validation.
        def toUnvalidatedOrder(dto: OrderFormDto) : UnvalidatedOrder = {
            val customerInfo = CustomerInfoDto.toUnvalidatedCustomerInfo(dto.customerInfo)
            val shippingAddress = AddressDto.toUnvalidatedAddress(dto.shippingAddress)
            val billingAddress = AddressDto.toUnvalidatedAddress(dto.billingAddress)
            val lines = dto.lines.map(line => OrderLineDto.toUnvalidatedOrderLine(line))
            UnvalidatedOrder(dto.orderId, customerInfo,shippingAddress, billingAddress, lines)
        }
    }

    //===============================================
    // DTO for OrderPlaced event
    //===============================================


    /// Event to send to shipping context
    final case class OrderPlacedDto(
        orderId : String,
        customerInfo : CustomerInfoDto,
        shippingAddress : AddressDto,
        billingAddress : AddressDto,
        amountToBill : BigDecimal,
        lines : List[PricedOrderLineDto])

    object OrderPlacedDto {
        /// Convert a OrderPlaced object into the corresponding DTO.
        /// Used when exporting from the domain to the outside world.
        def fromDomain(domainObj: PricedOrder) : OrderPlacedDto = {
            val customerInfo = CustomerInfoDto.fromCustomerInfo(domainObj.customerInfo)
            val shippingAddress = AddressDto.fromAddress(domainObj.shippingAddress)
            val billingAddress = AddressDto.fromAddress(domainObj.billingAddress)
            val lines = domainObj.lines.map(line => PricedOrderLineDto.fromDomain(line))
            OrderPlacedDto(domainObj.orderId, customerInfo, shippingAddress, billingAddress, domainObj.amountToBill, lines)
        }
    }

    //===============================================
    // DTO for BillableOrderPlaced event
    //===============================================

    /// Event to send to billing context
    final case class BillableOrderPlacedDto(
        orderId : String,
        billingAddress: AddressDto,
        amountToBill : BigDecimal)

    object BillableOrderPlacedDto {
        /// Convert a BillableOrderPlaced object into the corresponding DTO.
        /// Used when exporting from the domain to the outside world.
        def fromDomain(domainObj: BillableOrderPlaced) : BillableOrderPlacedDto = {
            val billingAddress = AddressDto.fromAddress(domainObj.billingAddress)
            BillableOrderPlacedDto(domainObj.orderId, billingAddress, domainObj.amountToBill)
        }
    }

    //===============================================
    // DTO for OrderAcknowledgmentSent event
    //===============================================

    /// Event to send to other bounded contexts
    final case class OrderAcknowledgmentSentDto(orderId : String, emailAddress : String)

    object OrderAcknowledgmentSentDto {
        /// Convert a OrderAcknowledgmentSent object into the corresponding DTO.
        /// Used when exporting from the domain to the outside world.
        def fromDomain(domainObj:OrderAcknowledgmentSent) : OrderAcknowledgmentSentDto = {
            OrderAcknowledgmentSentDto(domainObj.orderId, domainObj.emailAddress)
        }
    }

    //===============================================
    // DTO for PlaceOrderEvent
    //===============================================

    /// Use a dictionary representation of a PlaceOrderEvent, suitable for JSON
    /// See "Serializing Records and Choice Types Using Maps" in chapter 11
    type PlaceOrderEventDto = Map[String, Object]

    object PlaceOrderEventDto {
        /// Convert a PlaceOrderEvent into the corresponding DTO.
        /// Used when exporting from the domain to the outside world.
        def fromDomain(domainObj: PlaceOrderEvent) : PlaceOrderEventDto = {
            domainObj match {
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
    }


    //===============================================
    // DTO for PlaceOrderError
    //===============================================

    final case class PlaceOrderErrorDto(code : String, message : String)

    object PlaceOrderErrorDto {
        def fromDomain(domainObj: PlaceOrderError) : PlaceOrderErrorDto = {
            domainObj match {
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

}
