import File.readFile
import cats.Foldable
import cats.implicits._

object AocPlaybook extends App {

  //day 1
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
}
