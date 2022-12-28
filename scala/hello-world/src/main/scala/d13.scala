import scala.collection.mutable.ArrayBuffer
object d13 extends App {
  sealed abstract class Packet;

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

  def inOrder(pair: (Packet, Packet)): Option[Boolean] = {
    pair match {
      case (I(l), I(r)) =>
        if (l < r) Some(true) else if (l > r) Some(false) else None
      case (l: L, r: I) => inOrder((l, new L(Array(r))))
      case (l: I, r: L) => inOrder((new L(Array(l)), r))
      case (L(l), L(r)) => {
        var idx = 0;
        while (idx < l.size && idx < r.size) {
          inOrder((l(idx), r(idx))) match {
            case Some(v) => return Some(v)
            case None    =>
          }
          idx += 1
        }
        if (l.size < r.size) Some(true)
        else if (l.size > r.size) Some(false)
        else None
      }
    }
  }

  def p1(pairs: Array[(L, L)]): Int = {
    pairs.zipWithIndex.map { case (pair, idx) =>
      if (inOrder(pair).get) idx + 1 else 0
    }.sum
  }

  def p2(pairs: Array[(L, L)]): Int = {
    val divider_keys = Array(
      new L(Array(new L(Array(new I(2))))),
      new L(Array(new L(Array(new I(6)))))
    )
    val all_lists = pairs.flatMap { case (l, r) => Array(l, r) } ++ divider_keys
    val sorted = all_lists.sortWith { case (l, r) =>
      inOrder((l, r)).get
    }
    // sorted.map(println)
    sorted.zipWithIndex
      .map { case (l: L, idx) =>
        if (divider_keys.exists(a => inOrder((a, l)).isEmpty)) idx + 1 else 1
      }
      .fold(1) { case (x, y) => x * y }
  }

  val input = common.readFile(args(0))
  val pairs = parsePairs(input)
  // pairs.map(println)
  println(p1(pairs))
  println(p2(pairs))
}
