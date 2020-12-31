import SimpleTypes._

object CompoundTypes {

    final case class PersonalName(firstName : String50, lastName : String50)

    final case class CustomerInfo(name : PersonalName, emailAddress : EmailAddress, vipStatus: VipStatus)

    final case class Address(
        addressLine1 : String50,
        addressLine2 : Option[String50],
        addressLine3 : Option[String50],
        addressLine4 : Option[String50],
        city : String50,
        zipCode : ZipCode,
        country: String,
        state: UsStateCode)

    final case class ValidatedOrderLine(
        orderLineId : OrderLineId,
        productCode : ProductCode,
        quantity : OrderQuantity)

    final case class ValidatedOrder(
        orderId : OrderId,
        customerInfo : CustomerInfo,
        shippingAddress : Address,
        billingAddress : Address,
        lines : List[ValidatedOrderLine])

    final case class OrderAcknowledgment(
        emailAddress : EmailAddress,
        letter : HtmlString)
}
