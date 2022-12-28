import scala.collection.mutable.ArrayBuffer
object d13 extends App {
  sealed abstract class Packet extends Ordered[Packet] {
    override def compare(that: Packet): Int = {
      (this, that) match {
        case (I(l), I(r)) =>
          if (l < r) -1 else if (l > r) 1 else 0
        case (l: L, r: I) => l.compare(new L(Array(r)))
        case (l: I, r: L) => new L(Array(l)).compare(r)
        case (L(l), L(r)) => {
          var idx = 0;
          while (idx < l.size && idx < r.size) {
            l(idx).compare(r(idx)) match {
              case 1  => return 1
              case -1 => return -1
              case _  =>
            }
            idx += 1
          }
          if (l.size < r.size) -1
          else if (l.size > r.size) 1
          else 0
        }
      }
    }
  }

  case class L(a: Array[Packet]) extends Packet {
    override def toString() = {
      "L(" + a.mkString(",") + ")"
    }
  };
  case class I(v: Int) extends Packet {}

  def parse(s: String): (Packet, Int) = {
    if (s(0) == '[') {
      parseList(s)
    } else {
      var ii = 0;
      while (ii < s.size && s(ii).isDigit) { ii += 1 }
      (new I(s.slice(0, ii).toInt), ii)
    }
  }

  def parseList(s: String): (L, Int) = {
    assert(s(0) == '[')
    var idx = 1
    var inner = ArrayBuffer[Packet]()
    while (idx < s.size && s(idx) != ']') {
      val (p, next) = parse(s.slice(idx, s.length))
      inner.append(p)
      idx += next
      if (idx < s.size && s(idx) == ',') idx += 1
    }
    (new L(inner.toArray), idx + 1)
  }

  def parsePairs(inp: Array[String]): Array[(L, L)] = {
    var aidx = 0;
    var a: ArrayBuffer[(L, L)] = ArrayBuffer()
    while (aidx < inp.length) {
      val l = parseList(inp(aidx))._1
      aidx += 1
      val r = parseList(inp(aidx))._1
      aidx += 2
      a.append((l, r))
    }
    a.toArray
  }

  def p1(pairs: Array[(L, L)]): Int = {
    pairs.zipWithIndex.map { case ((l, r), idx) =>
      if (l < r) idx + 1 else 0
    }.sum
  }

  def p2(pairs: Array[(L, L)]): Int = {
    val divider_keys = Array(
      new L(Array(new L(Array(new I(2))))),
      new L(Array(new L(Array(new I(6)))))
    )
    val all_lists = pairs.flatMap { case (l, r) => Array(l, r) } ++ divider_keys
    val sorted = all_lists.sorted
    // sorted.map(println)
    sorted.zipWithIndex
      .map { case (l: L, idx) =>
        if (divider_keys.exists(_.compare(l) == 0)) idx + 1 else 1
      }
      .fold(1) { case (x, y) => x * y }
  }

  val input = common.readFile(args(0))
  val pairs = parsePairs(input)
  // pairs.map(println)
  println(p1(pairs))
  println(p2(pairs))
}
