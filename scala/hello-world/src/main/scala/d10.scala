import scala.util.Using
import scala.io.Source
object d10 extends App {
  sealed abstract class Instruction
  case class AddX(n: Int) extends Instruction
  case class Noop() extends Instruction

  def parseInstr(s: String): Instruction = {
    val addx_pattern = """addx ([-\d]+)""".r
    s match {
      case "noop"          => Noop()
      case addx_pattern(n) => AddX(n.toInt)
    }
  }

  def p1(instructions: Array[Instruction]): Int = {
    var x = 1;
    var clock = 0;
    var signal_strength = 0;
    for (instr <- instructions) {
      val prev_clock = clock
      val prev_x = x;
      instr match {
        case Noop() => clock += 1;
        case AddX(n) => {
          clock += 2;
          x += n;
        }
      }
      for (c <- prev_clock + 1 to clock; if (c - 20) % 40 == 0) {
        signal_strength += prev_x * c
      }
    }
    signal_strength
  }

  def p2(instructions: Array[Instruction]): Array[Array[Char]] = {
    var buf = Array.ofDim[Char](6, 40)
    for (c <- 0 until 40; r <- 0 until 6) {
      buf(r)(c) = '.'
    }
    var instr_idx = 0;
    var clock = 0;
    var x = 1;
    var current_instr: Instruction = Noop()
    var instr_started = clock;
    while (
      instr_idx < instructions.length || current_instr.isInstanceOf[AddX]
    ) {
      // Only update X when the instruction is retired
      current_instr match {
        case AddX(n) if (clock - instr_started == 2) => {
          x += n
          current_instr =
            if (instr_idx < instructions.length) instructions(instr_idx)
            else Noop()
          instr_idx += 1
          instr_started = clock
        }
        case Noop() => {
          current_instr =
            if (instr_idx < instructions.length) instructions(instr_idx)
            else Noop()
          instr_idx += 1
          instr_started = clock
        }
        case AddX(_) =>
      }
      val c = clock % 40;
      val r = (clock / 40) % 6
      // println(s"($r, $c): $x, $current_instr at $clock")
      if (c == x || c == (x - 1) || c == (x + 1)) {
        buf(r)(c) = '#'
      }
      clock += 1
    }
    buf
  }

  val filename = args(0);
  val input_str = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  val instructions = input_str.map(parseInstr)
  println(p1(instructions))
  p2(instructions).map(_.mkString).map(println)
}
