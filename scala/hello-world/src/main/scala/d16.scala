import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
object d16 extends App {
  case class Valve(rate: Int, tunnels: List[String])
  var in = common.readFile(args(0))
  def parse_input(in: Array[String]): HashMap[String, Valve] = {
    val r =
      """Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r
    var h = HashMap[String, Valve]()
    in.foreach(_ match {
      case r(name, flo, tunnels) =>
        h(name) = Valve(flo.toInt, tunnels.split(", ").toList)
    })
    h
  }
  var valve_map = parse_input(in)
  case class Valve2(rate: Int, tunnels: HashMap[String, Int])
  def bfs(
      start: String,
      m: HashMap[String, List[String]]
  ): List[(String, Int)] = {
    var visited = HashSet[String](start)
    var o = ArrayBuffer[(String, Int)]()
    var q = Queue[(String, Int)]()
    m(start).foreach(s => q.append((s, 1)))
    while (!q.isEmpty) {
      val (s, d) = q.dequeue()
      if (!visited.contains(s)) {
        m(s).foreach(s0 => q.append((s0, d + 1)))
        visited += s
        o.append((s, d))
      }
    }
    o.toList
  }
  def find_paths(inp: HashMap[String, Valve]): HashMap[String, Valve2] = {
    val relavent_valves: Set[String] =
      inp.filter { case (_, Valve(r, _)) => r > 0 }.map(_._1).toSet ++ Set("AA")
    val tmap = inp.map { case (s0, Valve(_, t0)) => (s0, t0) }
    inp.filter { case (s, _) => relavent_valves.contains(s) }.map {
      case (s, Valve(r, _)) => {
        var tunnel_map: HashMap[String, Int] = HashMap()
        bfs(s, tmap).foreach { case (a, b) =>
          if (relavent_valves.contains(a)) tunnel_map(a) = b
        }
        (s, Valve2(r, tunnel_map))
      }
    }
  }
  val paths = find_paths(valve_map)
  // println(paths)

  // map of (turned, curr) -> (score, last_minute, acc0)
  case class Acc(total_p: Int, v: String, m: Int)
  case class ValveSeq(tot: Int, valves: List[Acc])

  def get_total_p(a: List[Acc]): Int = { a.map(_.total_p).sum }
  def so(
      inp: HashMap[String, Valve2],
      num_valves: Int,
      minutes: Int,
      turned: Set[String],
      acc: ValveSeq,
      curr: String,
      memo: HashMap[(Set[String], String), (ValveSeq, Int, ValveSeq)]
  ): ValveSeq = {
    val v: () => ValveSeq = () => {
      // obervation: for a given (turned, curr), we want to have turned the
      // valves as early as possible
      // println(s"$minutes $turned $acc $curr")
      if (minutes <= 0 || turned.size == num_valves) acc
      else {
        val neighbors = inp(curr).tunnels
        val rate = inp(curr).rate
        ({
          if (!turned.contains(curr) && rate != 0) {
            val pressure = rate * (minutes - 1)
            List(
              so(
                inp,
                num_valves,
                minutes - 1,
                turned + curr,
                ValveSeq(
                  acc.tot + pressure,
                  acc.valves :+ Acc(pressure, curr, minutes)
                ),
                curr,
                memo
              )
            )
          } else Nil
        } ++ neighbors.filter { case (ns, _) => !turned.contains(ns) }.map {
          case (ns, nd) =>
            so(inp, num_valves, minutes - nd, turned, acc, ns, memo)
        }).maxBy(_.tot)
      }
    }
    memo.get((turned, curr)) match {
      case Some((sc, l_minute, acc0))
          if (l_minute >= minutes && acc0.tot >= acc.tot) =>
        sc
      case _ => {
        val vv = v()
        memo((turned, curr)) = (vv, minutes, acc)
        vv
      }
    }
  }

  /*
  // Map of (turned, curr) -> (score, last_minute, acc0)
  var memo2 = HashMap[(Set[String], Set[String]), (Int, Int, Int)]()

  def so2(
      inp: HashMap[String, Valve],
      path: HashMap[String, Valve2],
      num_valves: Int,
      minutes: Int,
      turned: Set[String],
      acc: Int,
      curr: Set[String]
  ): Int = {
    val v = () => {
      // println(s"$minutes $turned $acc $curr")
      if (minutes <= 0 || turned.size == num_valves) acc
      else {
        var curr0 = curr.toList
        if (curr0.size == 1) {
          curr0 = curr0 ++ curr0
        }
        val (curr1, curr2) = (curr0(0), curr0(1))
        val (rate1, rate2) = (inp(curr1).rate, inp(curr2).rate)
        val pressure1 = rate1 * (minutes - 1)
        val pressure2 = rate2 * (minutes - 1)
        val neighbors1 = inp(curr1).tunnels
        val neighbors2 = inp(curr2).tunnels
        // Both go to neighbor
        val all_neighbors = {
          for (n1 <- neighbors1; n2 <- neighbors2)
            yield Set(n1, n2)
        }
        val l: List[Int] = {
          // Turn both
          if (
            !turned.contains(curr2) && rate2 != 0 && !turned.contains(
              curr1
            ) && rate1 != 0 && curr1 != curr2
          ) {
            List(
              so2(
                inp,
                path,
                num_valves,
                minutes - 1,
                turned + curr1 + curr2,
                acc + pressure2 + pressure1,
                curr
              )
            )
          } else Nil
        } ++ {
          // Turn 1
          if (!turned.contains(curr1) && rate1 != 0) {
            neighbors2
              .map { case (ns, nd) =>
                so2(
                  inp,
                  path,
                  num_valves,
                  minutes - nd,
                  turned + curr1,
                  acc + pressure1,
                  Set(curr1, ns)
                )
              }
          } else Nil
        } ++ {
          // Turn 2
          if (!turned.contains(curr2) && rate2 != 0) {
            neighbors1
              .map { case (ns, nd) =>
                so2(
                  inp,
                  path,
                  num_valves,
                  minutes - nd,
                  turned + curr2,
                  acc + pressure2,
                  Set(curr2, ns)
                )
              }
          } else Nil
        } ++ all_neighbors.map(
          so2(inp, path, num_valves, minutes - 1, turned, acc, _)
        )
        l.max
      }
    }
    memo2.get((turned, curr)) match {
      case Some((sc, l_minute, acc0)) if (l_minute >= minutes && acc0 >= acc) =>
        sc
      case _ => {
        val vv = v()
        memo2((turned, curr)) = (vv, minutes, acc)
        if (memo2.size % 1000 == 0) {
          println("Memo size ", memo2.size)
        }
        vv
      }
    }
    // v()
  }
   */

  val num_valves: (HashMap[String, Valve] => Int) =
    (v: HashMap[String, Valve]) =>
      v.filter { case (_, Valve(r, _)) =>
        r > 0
      }.size
  def p1(valve_map: HashMap[String, Valve]): Int = {
    var memo = HashMap[(Set[String], String), (ValveSeq, Int, ValveSeq)]()
    val v =
      so(
        paths,
        num_valves(valve_map),
        30,
        Set(),
        ValveSeq(0, List()),
        "AA",
        memo
      )
    // println(memo.filter { case ((ts, _), _) =>
    //   ts == Set("MH", "QW", "ZU", "NT", "KF", "FF", "XY", "NQ")
    // })
    println(memo.size)
    println(v)
    v.tot
  }

  def merge_valve_seqs(a: ValveSeq, b: ValveSeq): Int = {
    var turned = HashSet[String]()
    var acc = 0
    def add_to(x: Acc) = {
      if (!turned.contains(x.v)) {
        acc += x.total_p
      }
      turned += x.v
    }
    var a_idx = 0
    var b_idx = 0
    while (a_idx < a.valves.size || b_idx < b.valves.size) {
      if (a_idx < a.valves.size && b_idx < b.valves.size) {
        if (a.valves(a_idx).m > b.valves(b_idx).m) {
          // A first
          add_to(a.valves(a_idx))
          a_idx += 1
        } else {
          add_to(b.valves(b_idx))
          b_idx += 1
        }
      } else if (a_idx < a.valves.size) {
        add_to(a.valves(a_idx))
        a_idx += 1
      } else if (b_idx < b.valves.size) {
        add_to(b.valves(b_idx))
        b_idx += 1
      }
    }
    acc
  }

  def p2(valve_map: HashMap[String, Valve]): Int = {
    var memo = HashMap[(Set[String], String), (ValveSeq, Int, ValveSeq)]()
    val v =
      so(
        paths,
        num_valves(valve_map),
        26,
        Set(),
        ValveSeq(0, List()),
        "AA",
        memo
      )
    // println(memo.filter { case ((t, _), _) => t == Set("JJ", "BB", "CC") })
    // println(memo.filter { case ((t, _), _) => t == Set("DD", "HH", "EE") })
    println(memo.size)
    var m = 0
    var md: Option[(List[Acc], List[Acc])] = None
    var idx = 0L
    val expected = memo.size.toLong * memo.size.toLong
    for (
      seq1 <- memo;
      seq2 <- memo
    ) {
      val s1 = seq1._2._3
      val s2 = seq2._2._3
      val m0 = merge_valve_seqs(s1, s2)
      if (m0 > m) {
        m = m0
        md = Some((s1.valves, s2.valves))
      }
      idx += 1
      if (idx % 1000000 == 0) {
        val pct = idx * 100f / expected
        println(s"At $pct%")
      }
    }
    println(md)
    // so2(valve_map, paths, num_valves(valve_map), 26, Set(), 0, Set("AA"))
    m
  }
  // println(p1(valve_map))
  println(p2(valve_map))
}
