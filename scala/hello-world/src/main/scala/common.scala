import scala.util.Using
import scala.io.Source
object common {
  def readFile(fname: String): Array[String] = {
    Using(Source.fromFile(fname)) { source =>
      source.getLines().toArray
    }.get
  }
}
