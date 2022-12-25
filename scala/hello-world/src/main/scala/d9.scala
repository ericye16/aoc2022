import scala.util.Using
import scala.io.Source
import scala.collection.mutable

object d9 extends App {
  object Direction extends Enumeration {
    type Direction = Value
    val Left, Right, Up, Down = Value
  }
  import Direction._
  case class Move(dir: Direction, num: Int)

  def parseMove(s: String): Move = {
    val re = """(L|R|D|U) (\d+)""".r
    s match {
      case re(dir, num) => {
        val d2: Direction = dir match {
          case "L" => Left
          case "R" => Right
          case "D" => Down
          case "U" => Up
        }
        val n2: Int = num.toInt;
        Move(d2, n2)
      }
    }
  }

  def p(num_tails: Int): (Array[Move]) => Int = { input_moves =>
    val num_knots = num_tails + 1
    var tail_positions: mutable.Set[(Int, Int)] = mutable.Set()
    var knots: mutable.ArrayBuffer[Array[Int]] = mutable.ArrayBuffer()
    for (_ <- 0 until num_knots) {
      knots += Array(0, 0)
    }
    for (move <- input_moves) {
      val dir = move.dir;
      for (_ <- 0 until move.num) {
        if (dir == Up) knots(0)(0) += 1
        else if (dir == Down) knots(0)(0) -= 1
        else if (dir == Left) knots(0)(1) -= 1
        else if (dir == Right) knots(0)(1) += 1
        for (k <- 1 until num_knots) {
          var tail_delta =
            (knots(k - 1)(0) - knots(k)(0), knots(k - 1)(1) - knots(k)(1))
          if (tail_delta._1.abs > 1 || tail_delta._2.abs > 1) {
            knots(k)(0) += math.signum(tail_delta._1)
            knots(k)(1) += math.signum(tail_delta._2)
          }

        }
        tail_positions += ((knots(num_knots - 1)(0), knots(num_knots - 1)(1)))
      }
    }
    tail_positions.size
  }

  def p1 = p(1)

  def p2 = p(9)

  val filename = args(0);
  val input_str = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  val input_moves = input_str.map(parseMove)
  println(p1(input_moves))
  println(p2(input_moves))
}
