import SimpleTypes.OrderQuantity.{Kilogram, Unit}
import SimpleTypes.ProductCode.{Gizmo, Widget}
import SimpleTypes.{KilogramQuantity, UnitQuantity, _}

import scala.util.matching.Regex

object SimpleTypes {

    // ===============================
    // Simple types and constrained types related to the OrderTaking domain.
    //
    // E.g. Single case discriminated unions (aka wrappers), enums, etc
    // ===============================

    /// Constrained to be 50 chars or less, not null
    type String50 = String

    /// An email address
    type EmailAddress = String

    /// A zip code
    type ZipCode = String

    /// An Id for Orders. Constrained to be a non-empty string < 10 chars
    type OrderId = String

    /// An Id for OrderLines. Constrained to be a non-empty string < 10 chars
    type OrderLineId = String

    /// The codes for Widgets start with a "W" and then four digits
    type WidgetCode = String

    /// The codes for Gizmos start with a "G" and then three digits.
    type GizmoCode = String

    /// A ProductCode is either a Widget or a Gizmo
    sealed trait ProductCode

    object ProductCode {

        final case class Widget(code : WidgetCode) extends ProductCode

        final case class Gizmo(code: GizmoCode) extends ProductCode

        /// Return the string value inside a ProductCode
        def value(productCode : ProductCode) : String = {
            productCode match {
                case Widget(code) => code
                case Gizmo(code) => code
            }
        }

        /// Create an ProductCode from a string
        /// Return Error if input is null, empty, or not matching pattern
        def create(fieldName: String, code: String) : Either[String, ProductCode] = {
            if(code.isEmpty){
                val message = s"$fieldName must not be null or empty"
                Left(message)
            }else if (code.startsWith("W")) {
                WidgetCode.create(fieldName, code) match {
                    case Left(value) => Left(value)
                    case Right(value) => Right(Widget(value))
                }
            }else if (code.startsWith("G")) {
                WidgetCode.create(fieldName, code) match {
                    case Left(value) => Left(value)
                    case Right(value) => Right(Gizmo(value))
                }
            }else {
                val message = s"$fieldName: Format not recognized"
                Left(message)
            }
        }
    }

    /// Constrained to be a integer between 1 and 1000
    type UnitQuantity = Int

    /// Constrained to be a decimal between 0.05 and 100.00
    type KilogramQuantity = BigDecimal

    sealed trait OrderQuantity

    /// A Quantity is either a Unit or a Kilogram
    object OrderQuantity {

        final case class Unit(code : UnitQuantity) extends OrderQuantity

        final case class Kilogram(code : KilogramQuantity) extends OrderQuantity

        /// Return the value inside a OrderQuantity
        def value(orderQuantity: OrderQuantity) : BigDecimal = {
            orderQuantity match {
                case Unit(code) => code
                case Kilogram(code) => code
            }
        }

        /// Create a OrderQuantity from a productCode and quantity
        def create(fieldName: String, productCode: ProductCode, quantity: BigDecimal): Either[String, OrderQuantity] = {
            productCode match {
                case Widget(_) =>
                    UnitQuantity.create(fieldName, quantity.toInt) // convert float to int
                    match {
                        case Left(value) => Left(value)
                        case Right(value) => Right(Unit(value)) // lift to OrderQuantity type
                    }
                case Gizmo(_) =>
                    KilogramQuantity.create(fieldName, quantity) match {
                        case Left(value) => Left(value)
                        case Right(value) => Right(Kilogram(value)) // lift to OrderQuantity type
                    }
            }
        }

    }

    /// Constrained to be a decimal between 0.0 and 1000.00
    type Price = BigDecimal

    /// Constrained to be a decimal between 0.0 and 10000.00
    type BillingAmount = BigDecimal

    /// Represents a PDF attachment
    final case class PdfAttachment(name : String, bytes: Array[Byte])

    type PromotionCode = String

}

// ===============================
// Reusable constructors and getters for constrained types
// ===============================

/// Useful functions for constrained types
object ConstrainedType {

    /// Create a constrained string
    /// Return Error if input is null, empty, or length > maxLen
    def createString(fieldName: String, maxLen: Int , str: String): Either[String, String] = {
        if(str.isEmpty) {
            val message = s"$fieldName must not be null or empty"
            Left(message)
        }else if(str.length > maxLen){
            val message = s"$fieldName must not be more that $maxLen chars"
            Left(message)
        } else {
            Right(str)
        }
    }

    /// Create a optional constrained string
    /// Return None if input is null, empty.
    /// Return error if length > maxLen
    /// Return Some if the input is valid
    def createStringOption(fieldName: String, maxLen: Int, str: String): Either[String, Option[String]]= {
            if(str.isEmpty) {
                Right(None)
            }else if(str.length > maxLen) {
                val message = s"$fieldName must not be more that $maxLen chars"
                Left(message)
            }else {
                Right(Some(str))
            }
    }

    /// Create a constrained integer using the constructor provided
    /// Return Error if input is less than minVal or more than maxVal
    def createInt(fieldName: String, minVal : Int, maxVal : Int, i: Int) : Either[String, Int] = {
        if(i < minVal){
            val message = s"$fieldName: Must not be less than $minVal"
            Left(message)
        }else if (i > maxVal){
            val message = s"$fieldName: Must not be greater than $maxVal"
            Left(message)
        }else {
            Right(i)
        }
    }

    /// Create a constrained decimal using the constructor provided
    /// Return Error if input is less than minVal or more than maxVal
    def createDecimal(fieldName: String, minVal : BigDecimal, maxVal : BigDecimal, i: BigDecimal) : Either[String, BigDecimal] = {
        if(i < minVal){
            val message = s"$fieldName: Must not be less than $minVal"
            Left(message)
        }else if (i > maxVal){
            val message = s"$fieldName: Must not be greater than $maxVal"
            Left(message)
        }else {
            Right(i)
        }
    }

    /// Create a constrained string using the constructor provided
    /// Return Error if input is null. empty, or does not match the regex pattern
    def createLike(fieldName: String, pattern: String, str: String) : Either[String, String] = {

        if(str.isEmpty){
            val message = s"$fieldName must not be null or empty"
            Left(message)
        }else{
            val regex : Regex = pattern.r
            if (regex.matches(str)) {
                Right(str)
            } else {
                val message = s"$fieldName $str must match the pattern"
                Left(message)
            }
        }
    }
}

object String50 {

    /// Create an String50 from a string
    /// Return Error if input is null, empty, or length > 50
    def create(fieldName: String, str: String): Either[String, String50] = {
        ConstrainedType.createString(fieldName, 50, str)
    }

    /// Create an String50 from a string
    /// Return None if input is null, empty.
    /// Return error if length > maxLen
    /// Return Some if the input is valid
    def createOption(fieldName: String, str: String): Either[String, Option[String50]] = {
        ConstrainedType.createStringOption(fieldName, 50, str)
    }

}

object EmailAddress {
    /// Create an EmailAddress from a string
    /// Return Error if input is null, empty, or doesn't have an "@" in it
    def create(fieldName: String, str: String) : Either[String, EmailAddress] = {
        ConstrainedType.createLike(fieldName, ".+@.+", str)
    }
}

object ZipCode {
    /// Create a ZipCode from a string
    /// Return Error if input is null, empty, or doesn't have 5 digits
    def create(fieldName: String, str: String) : Either[String, ZipCode]= {
        ConstrainedType.createLike(fieldName, """\d{5}""", str)
    }
}

object OrderId {

    /// Create an String50 from a string
    /// Return Error if input is null, empty, or length > 50
    def create(fieldName: String, str: String): Either[String, OrderId] = {
        ConstrainedType.createString(fieldName, 50, str)
    }
}

object OrderLineId {

    /// Create an String50 from a string
    /// Return Error if input is null, empty, or length > 50
    def create(fieldName: String, str: String): Either[String, OrderLineId] = {
        ConstrainedType.createString(fieldName, 50, str)
    }
}

object WidgetCode {
    /// Create a ZipCode from a string
    /// Return Error if input is null, empty, or doesn't have 5 digits
    def create(fieldName: String, code: String) : Either[String, WidgetCode]= {
        ConstrainedType.createLike(fieldName, """W\d{4}""", code)
    }
}

object GizmoCode {
    /// Create a ZipCode from a string
    /// Return Error if input is null, empty, or doesn't have 5 digits
    def create(fieldName: String, code: String) : Either[String, GizmoCode]= {
        ConstrainedType.createLike(fieldName, """G\d{3}""", code)
    }
}

object UnitQuantity {

    /// Create a UnitQuantity from a int
    /// Return Error if input is not an integer between 1 and 1000
    def create(fieldName: String, v: Int) : Either[String, UnitQuantity] = {
        ConstrainedType.createInt(fieldName,1, 1000, v)
    }
}


object KilogramQuantity {

    /// Create a UnitQuantity from a int
    /// Return Error if input is not an integer between 1 and 1000
    def create(fieldName: String, v: BigDecimal) : Either[String, KilogramQuantity] = {
        ConstrainedType.createDecimal(fieldName,0.5, 100, v)
    }
}

object Price {
    /// Create a Price from a decimal.
    /// Return Error if input is not a decimal between 0.0 and 1000.00
    def create(v: BigDecimal): Either[String, Price]= {
        ConstrainedType.createDecimal("Price", 0.0, 1000, v)
    }

    /// Create a Price from a decimal.
    /// Throw an exception if out of bounds. This should only be used if you know the value is valid.
    def unsafeCreate(v: BigDecimal): Price = {
        create(v).fold(error => throw new Exception(error), value => value)
    }

    /// Multiply a Price by a decimal qty.
    /// Return Error if new price is out of bounds.
    def multiply(qty: BigDecimal, p: Price): Either[String, Price] =
        create (qty * p)

}


object BillingAmount {
    // Create a BillingAmount from a decimal.
    /// Return Error if input is not a decimal between 0.0 and 10000.00
    def create(v: BigDecimal): Either[String, BillingAmount] = {
        ConstrainedType.createDecimal("BillingAmount", 0.0, 10000, v)
    }

    /// Sum a list of prices to make a billing amount
    /// Return Error if total is out of bounds
    def sumPrices(prices: List[Price]): Either[String, BillingAmount] = {
        val total = prices.sum
        create(total)
    }
}