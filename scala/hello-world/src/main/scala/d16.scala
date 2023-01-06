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

  object Question extends Enumeration {
    type Question = Value

    val One, Two = Value
  }
  import Question._

  def get_total_p(a: List[Acc]): Int = { a.map(_.total_p).sum }
  def so(
      q: Question,
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
      if (minutes <= 0 || turned.size == num_valves) {
        if (q == One)
          acc
        else {
          // val useful_paths: (String) => Boolean = s => {
          //   !turned.contains(s) || s == "AA"
          // }
          // val paths_2: HashMap[String, Valve2] =
          //   inp
          //     .filter { case (s, _) => useful_paths(s) }
          //     .map {
          //       case (s, vs) => {
          //         var tunnel_map: HashMap[String, Int] = HashMap()
          //         vs.tunnels.filter { case (s, _) => useful_paths(s) }.foreach {
          //           case (s, d) => tunnel_map(s) = d
          //         }
          //         (s, Valve2(vs.rate, tunnel_map))
          //       }
          //     }
          // val num_valves2 = paths_2.size - 1 // ignore AA
          var memo2 =
            HashMap[(Set[String], String), (ValveSeq, Int, ValveSeq)]()
          val acc2 = so(
            One,
            inp,
            num_valves,
            26,
            turned,
            ValveSeq(0, List()),
            "AA",
            memo2
          )
          ValveSeq(acc.tot + acc2.tot, acc.valves ++ acc2.valves)
        }
      } else {
        val neighbors = inp(curr).tunnels
        val rate = inp(curr).rate
        ({
          if (!turned.contains(curr) && rate != 0) {
            val pressure = rate * (minutes - 1)
            List(
              so(
                q,
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
            so(q, inp, num_valves, minutes - nd, turned, acc, ns, memo)
        }).maxBy(_.tot)
      }
    }
    memo.get((turned, curr)) match {
      case Some((sc, l_minute, acc0))
          if (l_minute >= minutes && acc0.tot >= acc.tot) => {
        sc
      }
      case _ => {
        val vv = v()
        // if (!vv.valves.map(_.v).startsWith(acc.valves.map(_.v))) {
        //   println(vv)
        //   println(acc)
        //   throw new RuntimeException("Weird!")
        // }
        memo((turned, curr)) = (vv, minutes, acc)
        vv
      }
    }
  }

  val num_valves: (HashMap[String, Valve] => Int) =
    (v: HashMap[String, Valve]) =>
      v.filter { case (_, Valve(r, _)) =>
        r > 0
      }.size
  def p1(valve_map: HashMap[String, Valve]): Int = {
    var memo = HashMap[(Set[String], String), (ValveSeq, Int, ValveSeq)]()
    val v =
      so(
        One,
        paths,
        num_valves(valve_map),
        30,
        Set(),
        ValveSeq(0, List()),
        "AA",
        memo
      )
    // memo.foreach(println)
    // println(memo.size)
    // println(v)
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
        Two,
        paths,
        num_valves(valve_map),
        26,
        Set(),
        ValveSeq(0, List()),
        "AA",
        memo
      )
    v.tot
  }
  println(p1(valve_map))
  println(p2(valve_map))
}
