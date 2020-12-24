import Domain.OrderQuantity.{Kilos, UnitQty}
import Domain.PlaceOrderError.{AddressInvalidFormat, AddressNotFound, OrderValidation}
import Domain.ProductCode.{Gizmo, Widget}
import Domain._

package object Extensions {

    implicit class ValidateProductCode(private val productCode: ProductCode) extends AnyVal {
        def validateProductCode: Either[String, ProductCode] = {
            productCode match {
                case Widget(widgetCode) =>
                    if (widgetCode.isEmpty) {
                        Left("UnitQuantity can not be empty")
                    } else {
                        Right(productCode)
                    }
                case Gizmo(gizmoCode) =>
                    if (gizmoCode.isEmpty) {
                        Left("UnitQuantity can not be empty")
                    } else {
                        Right(productCode)
                    }
            }
        }
    }

    implicit class ValidateOrderQuantityUnits(private val quantity: Int) extends AnyVal {
        def validateOrderQuantity: Either[String, Int] = {
            if (quantity < 1) {
                Left("UnitQuantity can not be negative")
            } else if (quantity > 100) {
                Left("UnitQuantity can not be greater that 100")
            } else {
                Right(quantity)
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
        def validateOrderQuantityKilos: Either[String, BigDecimal] = {
            if (quantity < 10) {
                Left("Kilos can not less that 10")
            } else if (quantity > 100) {
                Left("Kilos can not be greater that 100")
            } else {
                Right(quantity)
            }
        }
    }

    implicit class ValidateBillingAmount(private val billingAmount: BigDecimal) extends AnyVal {
        def validateBillingAmount: Either[String, BigDecimal] = {
            if (billingAmount == 0) {
                Left("BillingAmount can not  be zero")
            } else {
                Right(billingAmount)
            }
        }
    }

    implicit class ValidateCustomerInfo(private val unvalidatedCustomerInfo: CustomerInfo) extends AnyVal {
        def validateCustomerInfo: Either[PlaceOrderError, CustomerInfo] = {
            if (unvalidatedCustomerInfo.name.firstName.length > 50) {
                Left(OrderValidation(ValidationError(unvalidatedCustomerInfo.name.firstName, "Name can not be longer that 50")))
            } else {
                Right(CustomerInfo(PersonalName(unvalidatedCustomerInfo.name.firstName, None,
                    unvalidatedCustomerInfo.name.lastName), unvalidatedCustomerInfo.emailAddress))
            }
        }
    }

    implicit class ValidateAddress(private val unvalidatedAddress: Address) extends AnyVal {
        def validateAddress: Either[PlaceOrderError, Address] = {
            if (unvalidatedAddress.addressLine1.length > 50) {
                Left(AddressInvalidFormat(ValidationError(unvalidatedAddress.addressLine1, "Invalid format for address")))
            } else if (unvalidatedAddress.addressLine2.isEmpty) {
                Left(AddressNotFound(ValidationError(unvalidatedAddress.addressLine2, "Address not found")))
            }
            else {
                Right(Address(unvalidatedAddress.addressLine1,
                    unvalidatedAddress.addressLine2, unvalidatedAddress.addressLine3, unvalidatedAddress.addressLine4,
                    unvalidatedAddress.city, unvalidatedAddress.zipCode))
            }
        }
    }


    final implicit class ValidateEmailAddress(private val emailAddress: String) extends AnyVal {
        def validateEmailAddress: Either[String, String] = {
            if (!emailAddress.contains("@")) {
                Left("Email address does not contains @")
            } else {
                Right(emailAddress)
            }
        }
    }

    implicit class ValidateOrderId(val orderId: String) extends AnyVal {
        def validateOrderId: Either[PlaceOrderError, String] = {
            if (orderId.isEmpty) {
                Left(OrderValidation(ValidationError(orderId, "Invalid orderId")))
            } else {
                Right(orderId)
            }
        }
    }

    implicit class ValidateOrderLineId(val orderLineId: String) extends AnyVal {
        def validateOrderLineId: Either[PlaceOrderError, String] = {
            if (orderLineId.isEmpty) {
                Left(OrderValidation(ValidationError(orderLineId, "Invalid OrderLineId")))
            } else {
                Right(orderLineId)
            }
        }
    }

    def listToOption[T](option: Option[T]): List[T] = {
        option match {
            case Some(value) => List(value)
            case None => List()
        }
    }

    def eitherToValue[A, B](either: Either[A, B]) : B = {
        either.right.get
    }

    def optionToValue[T](option: Option[T]): T = {
        option match {
            case Some(value) => value
            case None => throw new Exception("Can not extract value from an option")
        }
    }
}