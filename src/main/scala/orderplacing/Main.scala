package orderplacing

import orderplacing.PlaceOrderApi._
import scala.io.Source

object Main {

    def main(args: Array[String]): Unit = {
        val fileName = "place-order.json"
        val path = new java.io.File(s"src/main/scala/orderplacing/$fileName")

        if(path.exists()){
            val source = Source.fromFile(path)
            val responseBody = try source.mkString finally source.close()
            PlaceOrderApi.placeOrderApi(HttpRequest("GET", "<Add Url>", responseBody))
        }else {
            println(s" $path does not exists.")
        }
    }
}