package core

import scala.io.Source

object InputReader {

  def read(filePath: String): String = {
    val resource = Source.fromResource(filePath)
//    resource.getLines()
    resource.mkString

  }

}
