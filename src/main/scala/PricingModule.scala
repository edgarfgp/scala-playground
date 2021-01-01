import InternalTypes._
import SimpleTypes.PricingMethod._
import SimpleTypes._

object PricingModule {

    def createPricingMethod(promotionCode: PromotionCode): PricingMethod = {
        if(promotionCode.isBlank) Standard
        else Promotion(promotionCode)
    }


    def getPricingFunction(standardPrices: GetStandardPrices, promoPrices: GetPromotionPrices) : GetPricingFunction = {
        pricingMethod =>
            def getStandardPrice : GetProductPrice = standardPrices(())

            def getPromotionPrice(promotionCode: PromotionCode) : GetProductPrice = {
                productCode =>
                    val getPromotionPrice = promoPrices(promotionCode)

                    getPromotionPrice(productCode) match {
                        case Some(price) => price
                        case None => getStandardPrice(productCode)
                    }
            }

            pricingMethod match {
                case PricingMethod.Standard => getStandardPrice
                case Promotion(promotionCode) => getPromotionPrice(promotionCode)
            }

    }

}
