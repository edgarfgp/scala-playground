
import Domain.AddressValidationError.{AddressNotFound, InvalidFormat}
import Domain.OrderQuantity.{Kilos, UnitQty}
import Domain.ProductCode.{Gizmo, Widget}
import Domain._

package object Extensions {

    implicit class ValidateProductCode(private val productCode: ProductCode) extends AnyVal {
        def validateProductCode : Either[ProductCode, String] = {
            productCode match {
                case Widget(widgetCode) =>
                    if(widgetCode.isEmpty) {
                        Right("UnitQuantity can not be empty")
                    }else{
                        Left(productCode)
                    }
                case Gizmo(gizmoCode) =>
                    if(gizmoCode.isEmpty) {
                        Right("UnitQuantity can not be empty")
                    }else{
                        Left(productCode)
                    }
            }
        }
    }
    implicit class ValidateOrderQuantityUnits(private val quantity: Int) extends AnyVal {
        def validateOrderQuantity : Either[Int, String] = {
            if(quantity < 1) {
                Right("UnitQuantity can not be negative")
            }else if(quantity > 100){
                Right("UnitQuantity can not be greater that 100")
            }else{
                Left(quantity)
            }
        }
    }

    implicit class OrderQuantityUnits(private val orderQuantity: OrderQuantity) extends AnyVal {
        def value: OrderQuantity = orderQuantity match {
                case UnitQty(unitQty) => UnitQty(unitQty)
                case Kilos(kg) => Kilos(kg)
        }
    }

    implicit class ValidateOrderQuantityKilos(private val quantity: BigDecimal) extends AnyVal {
        def validateOrderQuantityKilos : Either[BigDecimal, String] = {
            if(quantity < 10) {
                Right("Kilos can not less that 10")
            }else if(quantity > 100){
                Right("Kilos can not be greater that 100")
            }else{
                Left(quantity)
            }
        }
    }

    implicit class ValidateBillingAmount(private val billingAmount: BigDecimal) extends AnyVal {
        def validateBillingAmount : Either[BigDecimal, String] = {
            if(billingAmount == 0) {
                Right("BillingAmount can not  be zero")
            }else{
                Left(billingAmount)
            }
        }
    }

    implicit class ValidateCustomerInfo(private val unvalidatedCustomerInfo: UnvalidatedCustomerInfo) extends AnyVal {
        def validateCustomerInfo : Either[ValidatedCustomerInfo, String] = {
            if(unvalidatedCustomerInfo.firstName.length > 50) {
                Right("Name can not be longer that 50")
            }else{
                Left(ValidatedCustomerInfo(PersonalName(unvalidatedCustomerInfo.firstName, None,
                    unvalidatedCustomerInfo.lastName), unvalidatedCustomerInfo.emailAddress))
            }
        }
    }

    implicit class ValidateAddress(private val unvalidatedAddress : UnvalidatedAddress) extends AnyVal {
        def validateAddress : Either[ValidatedAddress, AddressValidationError] = {
            if(unvalidatedAddress.addressLine1.length > 50) {
                Right(InvalidFormat("Invalid format for address"))
            }else if(unvalidatedAddress.addressLine1.isEmpty){
                Right(AddressNotFound("Address not found"))
            }
            else{
                Left(ValidatedAddress(unvalidatedAddress.addressLine1,
                    unvalidatedAddress.addressLine2, unvalidatedAddress.addressLine3, unvalidatedAddress.addressLine4,
                    unvalidatedAddress.city, unvalidatedAddress.zipCode))
            }
        }
    }


    final implicit class ValidateEmailAddress(private val emailAddress: String) extends AnyVal {
        def validateEmailAddress : Either[String, String] = {
            if(!emailAddress.contains("@")) {
                Right("Email address does not contains @")
            }else{
                Left(emailAddress)
            }
        }
    }

    implicit class ValidateOrderId(val str: String) extends AnyVal {
        def validateOrderId : Either[String, String] = {
            if (str.isEmpty) {
                Right("Invalid OrderId")
            }else{
                Left(str)
            }
        }
    }

    implicit class ValidateOrderLineId(val orderLineId: String) extends AnyVal {
        def validateOrderLineId : Either[String, String] = {
            if (orderLineId.isEmpty) {
                Right("Invalid OrderLineId")
            }else{
                Left(orderLineId)
            }
        }
    }

    def listToOption[T](option: Option[T]) : List[T] = {
        option match {
            case Some(value) => List(value)
            case None => List()
        }
    }

    def eitherToValue[T](either: Either[T, String]) : T  ={
        either match {
            case Left(value) => value
            case Right(errorMessage) =>  throw new Exception(errorMessage)
        }
    }

    def optionToValue[T](option: Option[T]) : T = {
        option match {
            case Some(value) => value
            case None => throw new Exception("Can not extract value from an option")
        }
    }
}