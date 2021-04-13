
// package native.core {
//
//     abstract class Temperature
//
//     case class Kelvin(value: Float) extends Temperature
//     case class Celsius(value: Float) extends Temperature
//
//     object utils {
//       def toCelsius(data: Float) : Float = data - 273.15f
//       def toCelsius(data: native.core.Temperature) : Float =
//           data match {
//               case native.core.Kelvin(v)  => toCelsius(v)
//               case native.core.Celsius(v) => v
//           }
//     }
//
//     object thermometer {
//         def getData() = {
//                 println("Thermometer returned data.")
//                 Kelvin(100.0f)
//             }
//     }
// }

// package server {
//     object connection {
//         def send(name: String, dataType: String, v: Float) = {
//             println("Sended: ")
//             println(v)
//         }
//     }
// }
//
//
// object Observer {
//    def readAndSendTemperature() {
//
//       def toCelsius(data: native.core.Temperature) : Float =
//         data match {
//           case native.core.Kelvin(v) => 273.15f - v
//           case native.core.Celsius(v) => v
//         }
//
//       val received = native.core.thermometer.getData()
//       val inCelsius = toCelsius(received)
//       val corrected = inCelsius - 12.5f    // defected device!
//       server.connection.send("temperature", "T-201A", corrected)
//    }
// }
//
// object L10 {
//     def main(args: Array[String]) {
//       Observer.readAndSendTemperature();
//     }
//
//     def launchMissle() = println("aa")
//
//     def max(a: Float, b: Float) = {
//       launchMissle()
//       math.max(a, b)
//     }
//
//     def calc(a: Int, b: Int, c: Float) : Float = {
//       val sum = a + b
//       val average = sum / 2
//       max(average, c)
//     }
// }



// trait ISensor {
//     def getData() : Float
//     def getName() : String
//     def getDataType() : String
// }
// trait IConnection {
//     def send(name: String, dataType: String, v: Float)
// }


// final class Receiver extends IConnection {
//     def send(name: String, dataType: String, v: Float) =
//         server.connection.send(name, dataType, v)
// }

object CH01_06 {
  type Board2D = Array[Array[Char]]

  trait IRule {
    def step(board: Board2D): Board2D
  }

  final class GoL extends IRule {
    def step(board: Board2D): Board2D =
      return board
  }

  final class Rule extends IRule {
    def step(board: Board2D): Board2D =
      return board
  }

  def main(args: Array[String]) {
    println("Hello world!")
  }
}
