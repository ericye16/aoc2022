import scala.util.Using
import scala.io.Source
import scala.collection.mutable.ArrayDeque
object d6 extends App {
  val p = (num_chars: Int, input: String) => {
    var deck: ArrayDeque[Char] = ArrayDeque();
    var idx = 0;
    var found_idx = 0;
    // println(input)
    for (char <- input if found_idx == 0) {
      deck += char;
      if (deck.length > num_chars) {
        deck.removeHead();
      }
      if (deck.length == num_chars) {
        val no_duplicates: Boolean = {
          val s: Set[Char] = deck.toSet;
          s.size == num_chars
        }
        if (no_duplicates) {
          found_idx = idx;
        }
      }
      idx += 1;
    }
    found_idx + 1
  }

  val filename = args(0);
  val input = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  for (line <- input) {
    println(p(4, line))
    println(p(14, line))
  }

}
