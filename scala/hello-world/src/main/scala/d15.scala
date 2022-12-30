import scala.collection.mutable.HashSet
object d15 extends App {
  case class Line(sensor: (Int, Int), beacon: (Int, Int))
  def parseLine(in: String): Line = {
    val r =
      """Sensor at x=([-\d]+), y=([-\d]+): closest beacon is at x=([-\d]+), y=([-\d]+)""".r
    in match {
      case r(sx, sy, bx, by) => Line((sx.toInt, sy.toInt), (bx.toInt, by.toInt))
    }
  }

  def p1(lines: Array[Line], loi: Int): Int = {
    val beacons =
      HashSet.from(lines.filter(_.beacon._2 == loi).map(_.beacon._1))
    // Will this scale? /shrug
    var unpossible = HashSet[Int]()
    for (line <- lines) {
      val r = ((line.beacon._1 - line.sensor._1).abs +
        (line.beacon._2 - line.sensor._2).abs)
      val y_dist = (loi - line.sensor._2).abs
      val er = r - y_dist
      // println(line.sensor + ", " + er)
      for (x <- (line.sensor._1 - er to (line.sensor._1 + er))) {
        if (!beacons.contains(x))
          unpossible.add(x)
      }
    }
    // println(unpossible.toArray.sorted.mkString(","))
    unpossible.size
  }
  val input = common.readFile(args(0))
  val lines = input.map(parseLine)
  val loi = args(1).toInt
  println(p1(lines, loi))
}
