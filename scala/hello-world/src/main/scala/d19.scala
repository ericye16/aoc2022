import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer
object d19 extends App {
  case class Blueprint(
      id: Int,
      ore_ore: Int,
      clay_ore: Int,
      obsidian_ore: Int,
      obsidian_clay: Int,
      geode_ore: Int,
      geode_obsidian: Int
  )
  def parseLine(s: String): Blueprint = {
    val r =
      """Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.""".r
    s match {
      case r(bid, oo, co, obo, oc, go, gob) =>
        Blueprint(
          bid.toInt,
          oo.toInt,
          co.toInt,
          obo.toInt,
          oc.toInt,
          go.toInt,
          gob.toInt
        )
    }
  }

  case class State(
      ores: Int,
      clays: Int,
      obsidians: Int,
      geodes: Int,
      ore_robots: Int,
      clay_robots: Int,
      obsidian_robots: Int,
      geode_robots: Int
  )

  def step_minutes(state: State, minutes: Int = 1): State = {
    State(
      ores = state.ores + state.ore_robots * minutes,
      clays = state.clays + state.clay_robots * minutes,
      obsidians = state.obsidians + state.obsidian_robots * minutes,
      geodes = state.geodes + state.geode_robots * minutes,
      ore_robots = state.ore_robots,
      clay_robots = state.clay_robots,
      obsidian_robots = state.obsidian_robots,
      geode_robots = state.geode_robots
    )
  }

  val skip = true
  val remember_acc = false

  class Memo {
    var best: Int = 0
    var map: HashMap[(Int, State), (List[(Int, State)], Int)] = HashMap()
  }

  var so_count = 0
  def so(
      b: Blueprint,
      minutes_left: Int,
      acc: List[(Int, State)],
      s: State,
      memo: Memo
  ): (List[(Int, State)], Int) = {
    val v: () => (List[(Int, State)], Int) = () => {
      so_count += 1
      val next_acc = if (remember_acc) acc :+ ((minutes_left, s)) else List()
      if (minutes_left == 0) {
        // println(s"Final state $s")
        (next_acc, s.geodes)
      } else {
        // Upper bound: 2 minutes left, build one robot, create one geode
        // Upper bound: 3 minutes left, build one robot, build one robot (create one geode), create 2 geodes
        val upper_bound =
          (minutes_left * s.geode_robots + s.geodes +
            (minutes_left * (minutes_left - 1)) / 2)
        if (upper_bound < memo.best) {
          return (next_acc, 0)
        }
        val mm = 24 - minutes_left
        // println(s"$mm $s")
        val next_s = step_minutes(s)
        var next_states = ArrayBuffer[(List[(Int, State)], Int)]()
        // Ore robot
        if (s.ores >= b.ore_ore) {
          next_states.append(
            so(
              b,
              minutes_left - 1,
              next_acc,
              next_s.copy(
                ores = next_s.ores - b.ore_ore,
                ore_robots = next_s.ore_robots + 1
              ),
              memo
            )
          )
        }
        // Clay robot
        if (s.ores >= b.clay_ore) {
          next_states.append(
            so(
              b,
              minutes_left - 1,
              next_acc,
              next_s.copy(
                ores = next_s.ores - b.clay_ore,
                clay_robots = next_s.clay_robots + 1
              ),
              memo
            )
          )
        }
        // Obsidian robot
        if (s.ores >= b.obsidian_ore && s.clays >= b.obsidian_clay) {
          next_states.append(
            so(
              b,
              minutes_left - 1,
              next_acc,
              next_s.copy(
                ores = next_s.ores - b.obsidian_ore,
                clays = next_s.clays - b.obsidian_clay,
                obsidian_robots = next_s.obsidian_robots + 1
              ),
              memo
            )
          )
        }
        // Geode robot
        // Always do this if possible.
        if (s.ores >= b.geode_ore && s.obsidians >= b.geode_obsidian) {
          next_states.append(
            so(
              b,
              minutes_left - 1,
              next_acc,
              next_s.copy(
                ores = next_s.ores - b.geode_ore,
                obsidians = next_s.obsidians - b.geode_obsidian,
                geode_robots = next_s.geode_robots + 1
              ),
              memo
            )
          )
        } else {
          next_states.append {
            val ore_rate = s.ore_robots
            val clay_rate = s.clay_robots
            val obsidian_rate = s.obsidian_robots

            def next_robot(
                resources: Int,
                resource_rate: Int,
                resources_needed: Int
            ): Int = {
              if (resource_rate == 0) Int.MaxValue
              else {
                var more_resources_needed =
                  (resources_needed - resources) % resources_needed
                if (more_resources_needed <= 0)
                  more_resources_needed += resources_needed
                if (more_resources_needed < 0) {
                  println(s"$resources_needed, $resources")
                  throw new RuntimeException("fdjskl")
                }
                val v = Math
                  .ceil(
                    more_resources_needed.toDouble / resource_rate.toDouble
                  )
                  .toInt
                // if (v != 1) {
                //   println(
                //     s"r: $resources, $resource_rate, $resources_needed, $v"
                //   )
                // }
                v
              }
            }

            // The minus ones are necessary because reasons...
            val next_ore_robot = next_robot(s.ores, ore_rate, b.ore_ore)
            val next_clay_robot = next_robot(s.ores, ore_rate, b.clay_ore)

            val next_obsidian_robot_ore =
              next_robot(s.ores, ore_rate, b.obsidian_ore)
            val next_obsidian_robot_clay =
              next_robot(s.clays, clay_rate, b.obsidian_clay)
            val next_obsidian_robot =
              if (s.ores >= b.obsidian_ore && s.clays >= b.obsidian_clay) {
                Math.max(next_obsidian_robot_clay, next_obsidian_robot_ore)
              } else {
                Math.min(next_obsidian_robot_clay, next_obsidian_robot_ore)
              }

            val next_geode_robot_ore = next_robot(s.ores, ore_rate, b.geode_ore)
            val next_geode_robot_obsidian =
              next_robot(s.obsidians, obsidian_rate, b.geode_obsidian)
            val next_geode_robot =
              if (s.ores >= b.geode_ore && s.obsidians >= b.geode_obsidian) {
                Math.max(next_geode_robot_obsidian, next_geode_robot_ore)
              } else {
                Math.min(next_geode_robot_obsidian, next_geode_robot_ore)
              }

            val next_steps = if (skip) {
              math.max(
                List(
                  next_ore_robot,
                  next_clay_robot,
                  next_geode_robot,
                  next_obsidian_robot,
                  minutes_left
                ).min.toInt,
                1
              )
            } else 1
            // if (next_steps != 1) {
            //   println(s"$minutes_left $s")
            //   println(
            //     s"Skipping forward $next_steps since $next_ore_robot $next_clay_robot $next_obsidian_robot $next_geode_robot $minutes_left"
            //   )
            // }

            val next_state_skip = step_minutes(s, next_steps)

            so(
              b,
              minutes_left - next_steps,
              next_acc,
              next_state_skip,
              memo
            )
          }
        }
        next_states.maxBy { case (_, sc) => sc }
      }
    }
    memo.map.get((minutes_left, s)) match {
      case Some(v) => v
      case None => {
        val vv = v()
        memo.map((minutes_left, s)) = vv
        if (vv._2 > memo.best) {
          memo.best = vv._2
        }
        vv
      }
    }
  }

  def findBest(b: Blueprint, minutes: Int): Int = {
    var memo = new Memo()
    so_count = 0
    val v = so(
      b,
      minutes,
      List(),
      State(
        ores = 0,
        clays = 0,
        obsidians = 0,
        geodes = 0,
        ore_robots = 1,
        clay_robots = 0,
        obsidian_robots = 0,
        geode_robots = 0
      ),
      memo
    )
    // println(memo)
    val formatter = java.text.NumberFormat.getInstance
    val fso_count = formatter.format(so_count)
    println(s"So count: $fso_count")
    println(s"On blueprint $b, found $v")
    v._2
  }

  def p1(inp: Seq[Blueprint]): Int = {
    inp.map(b => b.id * findBest(b, 24)).sum
  }

  def p2(inp: Seq[Blueprint]): Int = {
    inp.filter(_.id <= 3).map(b => findBest(b, 32)).fold(1)(_ * _)
  }

  val inp = common.readFile(args(0)).map(parseLine)
  println(p1(inp))
  println(p2(inp))
}
