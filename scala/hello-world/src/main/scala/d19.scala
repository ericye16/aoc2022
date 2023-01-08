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

  def step_one_minute(state: State): State = {
    State(
      ores = state.ores + state.ore_robots,
      clays = state.clays + state.clay_robots,
      obsidians = state.obsidians + state.obsidian_robots,
      geodes = state.geodes + state.geode_robots,
      ore_robots = state.ore_robots,
      clay_robots = state.clay_robots,
      obsidian_robots = state.obsidian_robots,
      geode_robots = state.geode_robots
    )
  }

  def so(
      b: Blueprint,
      minutes_left: Int,
      s: State,
      memo: HashMap[(Int, State), Int]
  ): Int = {
    val v: () => Int = () => {
      if (minutes_left == 0) s.geodes
      else {
        val next_s = step_one_minute(s)
        var next_states = ArrayBuffer[Int]()
        // Ore robot
        if (s.ores >= b.ore_ore) {
          next_states.append(
            so(
              b,
              minutes_left - 1,
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
            so(b, minutes_left - 1, next_s, memo)
          }
        }
        next_states.max
      }
    }
    memo.getOrElseUpdate((minutes_left, s), v())
  }

  def findBest(b: Blueprint, minutes: Int): Int = {
    var memo = HashMap[(Int, State), Int]()
    val v = so(
      b,
      minutes,
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
    println(s"On blueprint $b, found $v")
    v
  }

  def p1(inp: Seq[Blueprint]): Int = {
    inp.map(b => b.id * findBest(b, 24)).sum
  }

  def p2(inp: Seq[Blueprint]): Int = {
    inp.filter(_.id <= 3).map(b => findBest(b, 32)).fold(1)(_ * _)
  }

  val inp = common.readFile(args(0)).map(parseLine)
  println(p1(inp))
  // println(p2(inp))
}
