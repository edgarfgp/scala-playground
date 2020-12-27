import SimpleTypes._

package object CompoundTypes {

    // ==================================
    // Common compound types used throughout the OrderTaking domain
    //
    // Includes: customers, addresses, etc.
    // Plus common errors.
    //
    // ==================================

    // ==================================
    // Customer-related types
    // ==================================

    final case class PersonalName(firstName : String50, lastName : String50) extends Product
    final case class CustomerInfo(name : PersonalName, emailAddress : EmailAddress)

//    // ==================================
//    // Address-related
//    // ==================================
//
    final case class Address(
        addressLine1 : String50,
        addressLine2 : Option[String50],
        addressLine3 : Option[String50],
        addressLine4 : Option[String50],
        city : String50,
        zipCode : ZipCode)

//    // ==================================
//    // Product-related types
//    // ==================================
//
//    // Note that the definition of a Product is in a different bounded
//    // context, and in this context, products are only represented by a ProductCode
//    // (see the SimpleTypes module).


}
