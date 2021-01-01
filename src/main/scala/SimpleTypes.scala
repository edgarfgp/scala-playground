import CompoundTypes.PricedOrderProductLine
import PublicTypes.UnvalidatedAddress
import SimpleTypes.ProductCode.{Gizmo, Widget}
import SimpleTypes.{KilogramQuantity, UnitQuantity, _}

object SimpleTypes {

    type String50 = String

    type JsonString = String

    type HtmlString = String

    type CheckedAddress = UnvalidatedAddress

    type EmailAddress = String

    type ZipCode = String

    type OrderId = String

    type OrderLineId = String

    type WidgetCode = String

    type GizmoCode = String

    type UsStateCode = String

    type PromotionCode = String

    type CommentLine = String

    sealed trait PricedOrderLine
    object PricedOrderLine {

        final case class ProductLine(productLine : PricedOrderProductLine) extends PricedOrderLine

        final case class CommentLine(commentLine: String) extends PricedOrderLine
    }

    sealed trait VipStatus
    object VipStatus {

        final object Normal extends VipStatus

        final object Vip extends VipStatus

        def value(status: VipStatus): String = status match {
            case Normal => "Normal"
            case Vip =>"VIP"
        }

        def create(fieldName: String, str: String) : Either[String, VipStatus] = str match {
            case "normal" | "Normal" => Right(Normal)
            case "vip" | "VIP" | "Vip" => Right(Vip)
            case _ => Left(s"$fieldName: Must be one of 'Normal', 'VIP'")
        }
    }

    sealed trait ProductCode

    object ProductCode {

        final case class Widget(code: WidgetCode) extends ProductCode

        final case class Gizmo(code: GizmoCode) extends ProductCode

        def value(productCode: ProductCode): String = {
            productCode match {
                case Widget(code) => code
                case Gizmo(code) => code
            }
        }

        def create(fieldName: String, code: String): Either[String, ProductCode] = {
            if (code.isEmpty) Left(s"$fieldName must not be null or empty")
            else if (code.startsWith("W"))
                WidgetCode.create(fieldName, code) match {
                    case Left(value) => Left(value)
                    case Right(value) => Right(Widget(value))
                }
            else if (code.startsWith("G"))
                WidgetCode.create(fieldName, code) match {
                    case Left(value) => Left(value)
                    case Right(value) => Right(Gizmo(value))
                }
            else {
                Left(s"$fieldName: Format not recognized")
            }
        }
    }

    sealed trait PricingMethod
    object PricingMethod {

        final object Standard extends PricingMethod

        final case class Promotion(promotionCode: PromotionCode) extends PricingMethod
    }

    type UnitQuantity = Int

    type KilogramQuantity = BigDecimal

    sealed trait OrderQuantity

    object OrderQuantity {

        final case class Unit(code : UnitQuantity) extends OrderQuantity

        final case class Kilogram(code : KilogramQuantity) extends OrderQuantity

        def value(orderQuantity: OrderQuantity) : BigDecimal = orderQuantity match {
                case Unit(code) => code
                case Kilogram(code) => code
        }

        def create(fieldName: String, productCode: ProductCode, quantity: BigDecimal): Either[String, OrderQuantity] = productCode match {
            case Widget(_) =>
                UnitQuantity.create(fieldName, quantity.toInt)
                match {
                    case Left(value) => Left(value)
                    case Right(value) => Right(Unit(value))
                }
            case Gizmo(_) =>
                KilogramQuantity.create(fieldName, quantity) match {
                    case Left(value) => Left(value)
                    case Right(value) => Right(Kilogram(value))
                }
        }
    }

    type Price = BigDecimal

    type BillingAmount = BigDecimal

    final case class PdfAttachment(name : String, bytes: Array[Byte])
}

object ConstrainedType {

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

    def createStringOption(fieldName: String, maxLen: Int, str: String): Either[String, Option[String]] =
            if(str.isEmpty) Right(None)
            else if(str.length > maxLen) Left(s"$fieldName must not be more that $maxLen chars")
            else Right(Some(str))

    def createInt(fieldName: String, minVal : Int, maxVal : Int, i: Int) : Either[String, Int] = {
        if(i < minVal)
            Left(s"$fieldName: Must not be less than $minVal")
        else if (i > maxVal)
            Left(s"$fieldName: Must not be greater than $maxVal")
        else Right(i)
    }

    def createDecimal(fieldName: String, minVal : BigDecimal, maxVal : BigDecimal, i: BigDecimal) : Either[String, BigDecimal] = {
        if(i < minVal) Left(s"$fieldName: Must not be less than $minVal")
        else if (i > maxVal)
            Left(s"$fieldName: Must not be greater than $maxVal")
        else Right(i)
    }

    def createLike(fieldName: String, pattern: String, str: String) : Either[String, String] = {
        if(str.isEmpty) Left(s"$fieldName must not be null or empty")
        else
            if (pattern.r.matches(str)) { Right(str)}
            else {
                Left(s"$fieldName $str must match the pattern")
            }

    }
}

object String50 {

    def create(fieldName: String, str: String): Either[String, String50] = ConstrainedType.createString(fieldName, 50, str)

    def createOption(fieldName: String, str: String): Either[String, Option[String50]] = ConstrainedType.createStringOption(fieldName, 50, str)

}

object EmailAddress {
    def create(fieldName: String, str: String) : Either[String, EmailAddress] = ConstrainedType.createLike(fieldName, ".+@.+", str)
}

object ZipCode {
    def create(fieldName: String, str: String) : Either[String, ZipCode]= ConstrainedType.createLike(fieldName, """\d{5}""", str)
}

object OrderId {

    def create(fieldName: String, str: String): Either[String, OrderId] = ConstrainedType.createString(fieldName, 50, str)
}

object OrderLineId {

    def create(fieldName: String, str: String): Either[String, OrderLineId] = ConstrainedType.createString(fieldName, 50, str)
}

object WidgetCode {

    def create(fieldName: String, code: String) : Either[String, WidgetCode]= ConstrainedType.createLike(fieldName, """W\d{4}""", code)
}

object GizmoCode {

    def create(fieldName: String, code: String) : Either[String, GizmoCode]= ConstrainedType.createLike(fieldName, """G\d{3}""", code)
}

object UnitQuantity {

    def create(fieldName: String, v: Int) : Either[String, UnitQuantity] = ConstrainedType.createInt(fieldName,1, 1000, v)
}

object KilogramQuantity {

    def create(fieldName: String, v: BigDecimal): Either[String, KilogramQuantity] = ConstrainedType.createDecimal(fieldName, 0.5, 100, v)
}

object Price {

    def create(v: BigDecimal): Either[String, Price]= ConstrainedType.createDecimal("Price", 0.0, 1000, v)

    def unsafeCreate(v: BigDecimal): Price = create(v).fold(error => throw new Exception(error), value => value)

    def multiply(qty: BigDecimal, p: Price): Either[String, Price] = create (qty * p)
}

object BillingAmount {

    def create(v: BigDecimal): Either[String, BillingAmount] = ConstrainedType.createDecimal("BillingAmount", 0.0, 10000, v)

    def sumPrices(prices: List[Price]): Either[String, BillingAmount] = create(prices.sum)
}

object UsStateCode {

    def create(fieldName: String, str: String) = {
        val pattern = "^(A[KLRZ]|C[AOT]|D[CE]|FL|GA|HI|I[ADLN]|K[SY]|LA|M[ADEINOST]|N[CDEHJMVY]|O[HKR]|P[AR]|RI|S[CD]|T[NX]|UT|V[AIT]|W[AIVY])$"
        ConstrainedType.createLike(fieldName, pattern, str)
    }
}