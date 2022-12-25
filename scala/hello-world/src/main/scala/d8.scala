import scala.util.Using
import scala.io.Source
object d8 extends App {
  def parseInput(inp: Array[String]): Array[Array[Int]] = {
    inp.map(_.map(_.asDigit).toArray)
  }

  def isVisible(a: Array[Array[Int]], w: Int, h: Int, r: Int, c: Int): Int = {
    val v = a(r)(c)
    if (!(0 until r).exists(r0 => a(r0)(c) >= v)) {
      return 1;
    }
    if (!(r + 1 until w).exists(r0 => a(r0)(c) >= v)) {
      return 1;
    }
    if (!(0 until c).exists(c0 => a(r)(c0) >= v)) {
      return 1;
    }
    if (!(c + 1 until h).exists(c0 => a(r)(c0) >= v)) {
      return 1;
    }
    0
  }

  def p1(input: Array[Array[Int]]): Int = {
    val w = input.length
    val h = input(0).length
    (0 until w)
      .map(r => {
        (0 until h).map { c =>
          val v = input(r)(c)
          val v0 = isVisible(input, w, h, r, c)
          // println(s"v: $v, r: $r, c: $c, $v0")
          v0
        }.sum
      })
      .sum
  }
  val filename = args(0);
  val input_str = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  val input: Array[Array[Int]] = parseInput(input_str)
  println(p1(input))
}
