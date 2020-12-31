import SimpleTypes._

object CompoundTypes {

    final case class PersonalName(firstName : String50, lastName : String50) extends Product
    final case class CustomerInfo(name : PersonalName, emailAddress : EmailAddress)

    final case class Address(
        addressLine1 : String50,
        addressLine2 : Option[String50],
        addressLine3 : Option[String50],
        addressLine4 : Option[String50],
        city : String50,
        zipCode : ZipCode)

}
