import PlaceOrderApi.HttpRequest
import scala.io.Source

object Main {

    def main(args: Array[String]): Unit = {
        val filename = "src/main/scala/place-order.json"
        val source = Source.fromFile(filename)
        val responseBody = try source.mkString finally source.close()
        val result = PlaceOrderApi.placeOrderApi.apply(HttpRequest("GET", "https://myorders.com/placeOrders", responseBody))
        println(result)
    }
}