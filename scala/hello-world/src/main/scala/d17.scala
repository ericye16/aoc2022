import scala.util.Using
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

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

  def p1(input: String, num_rocks: Int): Int = {
    var arrHeights = Array.ofDim[Int](7);
    var arr = Array.ofDim[Char](5000, 7);
    for (w <- 0 until 7; r <- 0 until 5000) {
      arr(r)(w) = '.'
    };
    for (w <- 0 until 7) {
      arrHeights(w) = -1
    }
    var highest_rock = 0
    var dir_idx = 0;
    for (rock_idx: Int <- 0 until num_rocks) {
      val which_rock: Int = rock_idx % rocks.length;
      val rock = rocks(which_rock)
      var rock_origin = (highest_rock + 3, 2)
      var dropped = false;
      while (!dropped) {
        // Sideways
        val direction = if (input(dir_idx) == '<') {
          -1
        } else if (input(dir_idx) == '>') {
          1
        } else {
          throw new RuntimeException("Not allowed dir")
        };
        dir_idx += 1;
        dir_idx %= input.length()
        val orig_after_sideways = (rock_origin._1, rock_origin._2 + direction)
        val after_sideways =
          moveRock(rock, orig_after_sideways)
        if (
          after_sideways.forall {
            case (h, c) => {
              // c >= 0 && c < 7 && arrHeights(c) < h
              val a = c >= 0 && c < 7 && arr(h)(c) == '.'
              val b = c >= 0 && c < 7 && arrHeights(c) < h
              if (a != b) {
                println((h, c))
                printArray(arr, h + 10)
                println(arrHeights.mkString(","))
                throw new RuntimeException("fjdskl")
              }
              c >= 0 && c < 7 && arr(h)(c) == '.'
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
              // if (arrHeights(c) > h) {
              //   throw new RuntimeException("fdjskl")
              // }
              val a = h < 0 || arrHeights(c) >= h
              val b = h < 0 || arr(h)(c) == '#'
              if (a != b) {
                throw new RuntimeException("fdsfdsf 2")
              }
              a
            }
          }
        ) {
          // Stop the rock the prior step
          moveRock(rock, rock_origin).foreach {
            case (h, c) => {
              arrHeights(c) = Math.max(arrHeights(c), h)
              arr(h)(c) = '#'
            }
          }
          dropped = true;
          // Took me a while to find this issue
          highest_rock = Math.max(rock_origin._1 + rock.height, highest_rock)
          // printArray(arr, highest_rock + 2)
          println(arrHeights.mkString(" "))
          // println(highest_rock)
          // println("--------")
        } else {
          rock_origin = orig_after_down;
          // println("d " + rock_origin)
        }
      }
    }
    // printArray(arr, highest_rock + 2)
    highest_rock
  }
  val filename = args(0);
  val num_rocks = args(1).toInt
  val input = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  for (line <- input) {
    println(p1(line, num_rocks))
  }
}
