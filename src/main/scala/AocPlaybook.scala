import File.readFile
import cats.Foldable
import cats.implicits._

object AocPlaybook extends App {

  //day 1
  println("DAY 1")

  val depthReport = readFile[Int]("depthReport")(_.toInt)

  def countIncreases(records: List[Int], windowSize: Int = 1): Int = {
    records.sliding(windowSize).toList.headOption match {
      case Some(firstWindow) if records.length >= windowSize =>
        Foldable[List]
          .foldLeft(records.sliding(windowSize).toList, (0, firstWindow.combineAll)) {
            case ((count, previousWindow), currentWindow) =>
              if (previousWindow < currentWindow.combineAll) (count + 1, currentWindow.combineAll)
              else (count, currentWindow.combineAll)
          }
          ._1
      case None => 0
    }
  }

  println(s"Number of increases in depth is ${countIncreases(depthReport)}")
  println(s"Number of window sum increases in depth is ${countIncreases(depthReport, 3)}")

  // day2
  println("DAY 2")

  val horizontalStart = 0
  val depthStart      = 0

  case class Step(direction: Direction, value: Int)

  sealed trait Direction
  case object Up      extends Direction
  case object Down    extends Direction
  case object Forward extends Direction

  def decodeDepth(in: String): Step = {
    val p               = in.split(" ").toList
    val directionString = p.head
    val value           = p.tail.head.toInt
    directionString match {
      case "forward" => Step(Forward, value)
      case "up"      => Step(Up, value)
      case "down"    => Step(Down, value)
    }
  }

  val course = readFile("course")(decodeDepth)

  val result_part1 = course
    .foldLeft((horizontalStart, depthStart))((hd, step) => {
      val (horizontalLocation, depthLocation) = hd
      step.direction match {
        case Up      => (horizontalLocation, depthLocation - step.value)
        case Down    => (horizontalLocation, depthLocation + step.value)
        case Forward => (horizontalLocation + step.value, depthLocation)
      }
    })

  println(s"The sum of the location is: ${result_part1._1 * result_part1._2}")

  val aimStart = 0

  val result_part2 = course
    .foldLeft((horizontalStart, depthStart, aimStart))((hda, step) => {
      val (horizontalLocation, depthLocation, aim) = hda
      step.direction match {
        case Up      => (horizontalLocation, depthLocation, aim - step.value)
        case Down    => (horizontalLocation, depthLocation, aim + step.value)
        case Forward => (horizontalLocation + step.value, depthLocation + (step.value * aim), aim)
      }
    })

  println(s"The sum of the location based on aim is: ${result_part2._1 * result_part2._2}")

}
