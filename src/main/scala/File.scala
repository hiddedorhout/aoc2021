import scala.io.Source

object File {
  def readFile[A](fileName: String)(encoder: String => A): List[A] = {
    val file   = getClass.getResource(fileName).getFile
    val source = Source.fromFile(file)
    try {
      source.getLines().toList.map(encoder)
    } finally {
      source.close()
    }
  }
}
