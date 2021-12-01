import scala.io.Source

object AocPlaybook extends App {

  //day 1
  val depthReport = readFile[Int]("depthReport")(_.toInt)

  val increases = depthReport
    .foldLeft((0, depthReport.head))((countPrevious, depth) => {
      val (count, previous) = countPrevious
      if (previous < depth) (count + 1, depth)
      else (count, depth)
    })
    ._1

  println(s"Number of increases in depth is $increases")

  val windowIncreases = depthReport
    .foldLeft(((0, depthReport.head + depthReport(1) + depthReport(2)), (depthReport(1), depthReport(2))))(
      (countPreviousSumPreviousTwo, depth) => {
        val ((count, previousSum), (twoBack, onBack)) = countPreviousSumPreviousTwo
        val currentWindowSum                          = depth + onBack + twoBack
        if (previousSum < currentWindowSum) ((count + 1, currentWindowSum), (onBack, depth))
        else ((count, currentWindowSum), (onBack, depth))
      }
    )
    ._1
    ._1

  println(s"Number of window sum increases in depth is $windowIncreases")

  def readFile[A](fileName: String)(encoder: String => A): List[A] = {
    val file = getClass.getResource(fileName).getFile
    Source.fromFile(file).getLines().toList.map(encoder)
  }
}
