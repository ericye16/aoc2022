import scala.util.Using
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.IndexedSeqView
import scala.util.matching.Regex
object d5 extends App {
  val parseStacks: Array[String] => (Array[List[Char]], Int) =
    (input: Array[String]) => {
      var stacks: ArrayBuffer[List[Char]] = ArrayBuffer();
      var read_stacks = false;
      var line_idx = 0;

      // Literally the worst code I've ever read
      while (!read_stacks) {
        val line = input(line_idx)
        if (line.isEmpty()) {
          read_stacks = true;
        }
        var char_idx = 1;
        var stack_idx = 0;
        while (char_idx < line.length) {
          if (stacks.length <= stack_idx) {
            stacks += List();
          }
          val ch = line(char_idx);
          if (ch.isLetter) {
            stacks(stack_idx) +:= ch;
          }
          char_idx += 4;
          stack_idx += 1;
        }
        line_idx += 1;
      }
      (stacks.map(_.reverse).toArray, line_idx)
    }

  case class Move(num: Int, from: Int, to: Int)
  val parseMoves: Seq[String] => Seq[Move] = (input) => {
    val movePattern: Regex = """move (\d+) from (\d) to (\d)""".r
    for (line <- input) yield {
      val matching = movePattern.findFirstMatchIn(line).get;
      Move(
        matching.group(1).toInt,
        // subtract 1 for 0-indexed crates
        matching.group(2).toInt - 1,
        matching.group(3).toInt - 1
      )
    }
  }

  val input = Using(Source.fromFile("d5/input.txt")) { source =>
    source.getLines().toArray
  }.get

  var (stacks: Array[List[Char]], next_line: Int) = parseStacks(input);
  var stacks2 = stacks.clone;
  for (move <- parseMoves(input.drop(next_line))) {
    // println(move)
    for (_ <- 0 until move.num) {
      stacks(move.to) +:= stacks(move.from).head
      stacks(move.from) = stacks(move.from).tail
    }
    stacks2(move.to) = stacks2(move.from).take(move.num) ::: stacks2(move.to)
    stacks2(move.from) = stacks2(move.from).drop(move.num)
    // println(stacks2.mkString(","))
  }
  println(stacks.map(_.head).mkString);
  println(stacks2.map(_.head).mkString);
}
