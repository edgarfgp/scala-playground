import PlaceOrderApi._

import scala.io.Source

object Main {

    def main(args: Array[String]): Unit = {
        val filename = "src/main/scala/place-order.json"
        val source = Source.fromFile(filename)
        val responseBody = try source.mkString finally source.close()
        placeOrderApi(HttpRequest("GET", "https://myorders.com/placeOrders", responseBody))
    }
}