import scala.util.Using
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object d17 extends App {
  def printArray(arr: Array[Array[Char]], highest_rock: Int) = {
    for (h <- Math.min(highest_rock + 2, arr.length) - 1 to 0 by -1) {
      println(arr(h).mkString)
    };
  }

  class Rock(val rock_shape: Array[(Int, Int)], val height: Int);

  def create_rock(rock_shape: String): Rock = {
    val lines = rock_shape.split("""\n""")
    var rock_coords: ArrayBuffer[(Int, Int)] = ArrayBuffer();
    val height = lines.length
    for (h <- height until 0 by -1) {
      val h_idx = height - h
      val row = lines(h_idx)
      for (r <- 0 until row.size) {
        if (row(r) == '#') {
          rock_coords.addOne((h - 1, r));
        }
      };
    };
    new Rock(rock_coords.toArray, height)
  }

  val rocks = List(
    create_rock("""####"""),
    create_rock(""".#.
###
.#."""),
    create_rock("""..#
..#
### """),
    create_rock("""#
#
#
#"""),
    create_rock("""##
##""")
  )

  def moveRock(rock: Rock, offset: (Int, Int)): Array[(Int, Int)] = {
    rock.rock_shape.map { case (a, b) => (a + offset._1, b + offset._2) }
  }

  def p1(input: String, num_rocks: Long): Long = {
    val theight = 1000000

    var arr = Array.ofDim[Char](theight, 7);
    for (w <- 0 until 7; r <- 0 until theight) {
      arr(r)(w) = '.'
    };
    val state_interval = 100

    def dropRock(
        which_rock: Int,
        dir_idx: Int,
        arr: Array[Array[Char]],
        highest_rock: Int
    ): (Int, Int) = {
      val rock = rocks(which_rock)
      var rock_origin = (highest_rock + 3, 2)
      var dropped = false;
      var ho = highest_rock
      var dir0 = dir_idx
      var dropped_levels = 0
      while (!dropped) {
        // Sideways
        val direction = if (input(dir0) == '<') {
          -1
        } else if (input(dir0) == '>') {
          1
        } else {
          throw new RuntimeException("Not allowed dir")
        };
        dir0 += 1;
        dir0 %= input.length()
        val orig_after_sideways = (rock_origin._1, rock_origin._2 + direction)
        val after_sideways =
          moveRock(rock, orig_after_sideways)
        if (
          after_sideways.forall {
            case (h, c) => {
              // c >= 0 && c < 7 && arrHeights(c) < h
              val a = c >= 0 && c < 7 && arr(h)(c) == '.'
              a
            }
          }
        ) {
          rock_origin = orig_after_sideways
        }
        // println("s " + rock_origin)
        // Down
        val orig_after_down = (rock_origin._1 - 1, rock_origin._2)
        val after_down = moveRock(rock, orig_after_down)
        if (
          after_down.exists {
            case (h, c) => {
              val b = h < 0 || arr(h)(c) == '#'
              b
            }
          }
        ) {
          // Stop the rock the prior step
          moveRock(rock, rock_origin).foreach {
            case (h, c) => {
              arr(h)(c) = '#'
            }
          }
          dropped = true;
          // Took me a while to find this issue
          ho = Math.max(rock_origin._1 + rock.height, ho)
          // printArray(arr, highest_rock + 2)
          // println(highest_rock)
          // println("--------")
        } else {
          rock_origin = orig_after_down;
          dropped_levels += 1
          if (dropped_levels > state_interval) {
            throw new RuntimeException("not holding enough state!")
          }
          // println("d " + rock_origin)
        }
      }
      (ho, dir0)
    }
    // Modulo "window"
    var dir_idx = 0;
    var rock_idx = 0L
    // Map of (which rock, dir_idx, state) => (final height, rock_idx)
    var hmap = HashMap[(Int, Int, List[List[Char]]), (Int, Long)]()
    var highest_rock = 0
    var numrocks0 = num_rocks
    var add_height = 0L
    var cycled = false
    while (rock_idx < numrocks0) {
      val which_rock: Int = (rock_idx % rocks.length).toInt;
      val s =
        arr
          .slice(highest_rock - state_interval, highest_rock + 1)
          .map(_.toList)
          .toList
      val after_drop = dropRock(which_rock, dir_idx, arr, highest_rock)
      highest_rock = after_drop._1
      dir_idx = after_drop._2
      if (!cycled) {
        hmap.get((which_rock, dir_idx, s)) match {
          case None =>
            hmap((which_rock, dir_idx, s)) = (highest_rock, rock_idx)
          case Some((fheight, old_rock_idx)) => {
            val cycle_height = highest_rock - fheight
            val cycle_rocks = rock_idx - old_rock_idx
            val remaining_rocks = numrocks0 - rock_idx
            val cycles_left = remaining_rocks / cycle_rocks
            add_height = cycle_height * cycles_left
            println(s"Cycle detected: $cycle_height height, $cycle_rocks rocks")
            val jump_ahead_rocks = (cycles_left * cycle_rocks)
            numrocks0 -= jump_ahead_rocks
            println(
              s"Jumping ahead $jump_ahead_rocks rocks by adding $add_height"
            )
            cycled = true
          }
        }
      }
      rock_idx += 1
    }
    // printArray(arr, highest_rock + 2)
    highest_rock + add_height
  }
  val filename = args(0);
  val num_rocks = args(1).toLong
  val input = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  for (line <- input) {
    println(p1(line, num_rocks))
  }
}
