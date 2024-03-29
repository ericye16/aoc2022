import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Queue
import scala.collection.BitSet
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

  case class Acc(total_p: Int, v: String, m: Int)
  case class ValveSeq(tot: Int, valves: List[Acc])

  class Memo {
    var best: Int = 0
    var m = HashMap[(Set[String], String), (ValveSeq, Int, ValveSeq)]()
  }

  def upper_bound(
      minutes: Int,
      inp: HashMap[String, Valve2],
      turned: Set[String],
      acc: ValveSeq,
      curr: String
  ): Int = {
    val v = {
      inp(curr).tunnels
        .filter { case (n, _) => !turned.contains(n) }
        .map { case (n, d) =>
          inp(n).rate * Math.max(minutes - d, 0)
        }
        .sum
    }

    // println(s"$minutes $inp $turned $acc $curr $v")
    v + acc.tot + (
      if (!turned.contains(curr)) inp(curr).rate * (minutes - 1) else 0
    )
  }

  def so(
      inp: HashMap[String, Valve2],
      num_valves: Int,
      minutes: Int,
      turned: Set[String],
      acc: ValveSeq,
      curr: String,
      memo: Memo
  ): ValveSeq = {
    val v: () => ValveSeq = () => {
      // obervation: for a given (turned, curr), we want to have turned the
      // valves as early as possible
      // println(s"$minutes $turned $acc $curr")
      if (minutes <= 0 || turned.size == num_valves) {
        acc

      } else {
        val upb = upper_bound(minutes, inp, turned, acc, curr)
        if (upb < memo.best) {
          return ValveSeq(0, List())
        }
        val neighbors = inp(curr).tunnels
        val rate = inp(curr).rate
        var valveseqs = ArrayBuffer[ValveSeq]()
        if (!turned.contains(curr) && rate != 0) {
          val pressure = rate * (minutes - 1)
          valveseqs.append(
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
        } else {
          neighbors.filter { case (ns, _) => !turned.contains(ns) }.foreach {
            case (ns, nd) =>
              valveseqs.append(
                so(inp, num_valves, minutes - nd, turned, acc, ns, memo)
              )
          }
        }
        valveseqs.maxBy(_.tot)
      }
    }
    memo.m.get((turned, curr)) match {
      case Some((sc, l_minute, acc0))
          if (l_minute >= minutes && acc0.tot >= acc.tot) => {
        sc
      }
      case _ => {
        val vv = v()
        memo.m((turned, curr)) = (vv, minutes, acc)
        if (vv.tot > memo.best) {
          memo.best = vv.tot
          // println("Best is ", memo.best)
        }
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
    var memo = new Memo()
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
    // memo.foreach(println)
    // println(memo.size)
    // println(v)
    v.tot
  }

  def so2(
      minutes: Int,
      inp: HashMap[String, Valve2],
      num_valves: Int,
      turned: Set[String],
      acc: ValveSeq,
      curr: String
  ): List[ValveSeq] = {
    // println(s"$minutes turned: $turned acc: $acc, $curr")
    if (minutes <= 0 || turned.size == num_valves) List(acc)
    else {
      // stop here case
      so2(0, inp, num_valves, turned, acc, curr) ++
        (if (!turned.contains(curr) && inp(curr).rate > 0) {
           // Turn here case
           val pressure = inp(curr).rate * (minutes - 1)
           so2(
             minutes - 1,
             inp,
             num_valves,
             turned + curr,
             ValveSeq(
               acc.tot + pressure,
               acc.valves :+ Acc(pressure, curr, minutes)
             ),
             curr
           )
         } else
           (inp(curr).tunnels
             .filter {
               case (v, _) => (!turned.contains(v) && inp(v).rate > 0)
             }
             .map { case (v, d) =>
               so2(minutes - d, inp, num_valves, turned, acc, v)
             })
             .flatten)
    }
  }

  def no_conflict(a: ValveSeq, b: ValveSeq): Boolean = {
    val s = a.valves.map(_.v).toSet
    b.valves.map(_.v).forall(!s.contains(_))
  }

  def p2(valve_map: HashMap[String, Valve]): Int = {
    val all_paths =
      so2(26, paths, num_valves(valve_map), Set(), ValveSeq(0, List()), "AA")
        .sortBy(-_.tot)
        .toArray
    val turned: Array[Set[String]] =
      all_paths.map(x => x.valves.map(_.v).toSet)
    var max = 0
    for (i <- 0 until all_paths.size) {
      var j = i + 1
      val a = all_paths(i)
      while (j < all_paths.size && all_paths(j).tot + a.tot > max) {
        if ((turned(i).intersect(turned(j))).isEmpty) {
          val b = all_paths(j)
          val m = a.tot + b.tot
          if (m > max) max = m
        }
        j += 1
      }
    }
    max
  }
  println(p1(valve_map))
  println(p2(valve_map))
}
