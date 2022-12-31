import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
object d15 extends App {
  case class Line(sensor: (Int, Int), beacon: (Int, Int))
  def parseLine(in: String): Line = {
    val r =
      """Sensor at x=([-\d]+), y=([-\d]+): closest beacon is at x=([-\d]+), y=([-\d]+)""".r
    in match {
      case r(sx, sy, bx, by) => Line((sx.toInt, sy.toInt), (bx.toInt, by.toInt))
    }
  }

  /*
  def mergeRanges(
      ranges: Array[Array[Int]],
      newRange: (Int, Int)
  ): Array[Array[Int]] = {
    var o: ArrayBuffer[Array[Int]] = ArrayBuffer()
    var maybeNewRange: Option[Array[Int]] = None
    var added = false
    def addTo() = {
      if (maybeNewRange.isDefined) {
        o.append(maybeNewRange.get)
      } else {
        o.append(Array(newRange._1, newRange._2))
      }
    }
    for (range <- ranges) {
      // Nonoverlapping, before
      if (range(1) < newRange._1 - 1) {
        o.append(range)
      } else if (range(0) <= newRange._1 || newRange._2 <= range(1)) {
        // Some amount of overlap
        if (maybeNewRange.isEmpty) {
          maybeNewRange = Some(
            Array(
              Math.min(newRange._1, range(0)),
              Math.max(newRange._2, range(1))
            )
          )
        } else {
          maybeNewRange.get(0) = Math.min(range(0), maybeNewRange.get(0))
          maybeNewRange.get(1) = Math.max(range(1), maybeNewRange.get(1))
        }
      } else if (range(0) > newRange._2 + 1) {
        // Overlapping after
        if (!added) {
          addTo()
        }
        added = true
        o.append(range)
      }
    }
    if (!added) {
      addTo()
    }
    o.toArray
  }
   */

  def getRange(line: Line, loi: Int): (Int, Int) = {
    val r = ((line.beacon._1 - line.sensor._1).abs +
      (line.beacon._2 - line.sensor._2).abs)
    val y_dist = (loi - line.sensor._2).abs
    val er = r - y_dist // Can be negative
    // println(line.sensor + ", " + er)
    (line.sensor._1 - er, line.sensor._1 + er)
  }

  def p1(lines: Array[Line], loi: Int): Int = {
    val beacons =
      HashSet.from(lines.filter(_.beacon._2 == loi).map(_.beacon._1))
    // Will this scale? /shrug
    // var ranges: Array[Array[Int]] = Array()
    var unpossible = HashSet[Int]()
    for (line <- lines) {
      val rn = getRange(line, loi)
      // println(line.sensor + ", " + er)
      for (x <- (rn._1 to rn._2)) {
        if (!beacons.contains(x))
          unpossible.add(x)
      }
    }
    // println(unpossible.toArray.sorted.mkString(","))
    unpossible.size
  }

  def p2(lines: Array[Line], siz: Int): Long = {
    def p2o(lines: Array[Line], siz: Int): (Long, Long) = {
      for (r <- 0 to siz) {
        var c = 0;
        while (c <= siz) {
          val last_c = c
          for (line <- lines) {
            val rn = getRange(line, r)
            if (rn._1 <= rn._2 && c >= rn._1 && c <= rn._2) {
              c = rn._2 + 1
            }
          }
          if (c == last_c) {
            return (c, r)
          }
        }
      }
      throw new RuntimeException
    }
    val (r0, c0) = p2o(lines, siz)
    println((r0, c0))
    r0 * 4000000 + c0
  }
  val input = common.readFile(args(0))
  val lines = input.map(parseLine)
  val loi = args(1).toInt
  println(p1(lines, loi))
  val siz = args(2).toInt
  println(p2(lines, siz))
}
