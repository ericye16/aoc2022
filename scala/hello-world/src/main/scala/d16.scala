import scala.collection.mutable.HashMap
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
  println(valve_map)

  // map of (turned, curr) -> (score, last_minute)
  var memo = HashMap[(Int, Set[String], Int, String), Int]()

  def so(
      inp: HashMap[String, Valve],
      num_valves: Int,
      minutes: Int,
      turned: Set[String],
      acc: Int,
      curr: String
  ): Int = {
    val v = () => {
      // obervation: for a given (turned, curr), we want to have turned the
      // valves as early as possible
      // println(s"$minutes $turned $acc $curr")
      if (minutes == 0 || turned.size == num_valves) acc
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
                acc + pressure,
                curr
              )
            )
          } else Nil
        } ++ neighbors.map(
          so(inp, num_valves, minutes - 1, turned, acc, _)
        )).max
      }
    }
    memo.getOrElseUpdate((minutes, turned, acc, curr), v())
    // v()
  }

  var memo2 = HashMap[(Int, Set[String], Int, Set[String]), Int]()

  def so2(
      inp: HashMap[String, Valve],
      num_valves: Int,
      minutes: Int,
      turned: Set[String],
      acc: Int,
      curr: Set[String]
  ): Int = {
    val v = () => {
      // println(s"$minutes $turned $acc $curr")
      if (minutes == 0 || turned.size == num_valves) acc
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
              .map(x =>
                so2(
                  inp,
                  num_valves,
                  minutes - 1,
                  turned + curr1,
                  acc + pressure1,
                  Set(curr1, x)
                )
              )
          } else Nil
        } ++ {
          // Turn 2
          if (!turned.contains(curr2) && rate2 != 0) {
            neighbors1
              .map(x =>
                so2(
                  inp,
                  num_valves,
                  minutes - 1,
                  turned + curr2,
                  acc + pressure2,
                  Set(curr2, x)
                )
              )
          } else Nil
        } ++ all_neighbors.map(
          so2(inp, num_valves, minutes - 1, turned, acc, _)
        )
        l.max
      }
    }
    memo2.getOrElseUpdate((minutes, turned, acc, curr), v())
    // v()
  }

  val num_valves: (HashMap[String, Valve] => Int) =
    (v: HashMap[String, Valve]) =>
      v.filter { case (_, Valve(r, _)) =>
        r > 0
      }.size
  def p1(valve_map: HashMap[String, Valve]): Int = {
    so(valve_map, num_valves(valve_map), 30, Set(), 0, "AA")
  }
  def p2(valve_map: HashMap[String, Valve]): Int = {
    so2(valve_map, num_valves(valve_map), 26, Set(), 0, Set("AA"))
  }
  // println(p1(valve_map))
  println(p2(valve_map))
}
