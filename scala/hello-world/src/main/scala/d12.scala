import scala.util.Using
import scala.io.Source
import scala.collection.mutable.PriorityQueue
object d12 extends App {

  def height(c: Char): Int = {
    if (c.isLower) {
      c - 'a'
    } else if (c == 'S') {
      0
    } else if (c == 'E') {
      'z' - 'a'
    } else throw new RuntimeException
  }

  def p1(input: Array[Array[Char]]): Int = {
    val s1 = input.size
    val s2 = input(0).size
    val start = (0 until input.size)
      .flatMap(x => (0 until input(0).size).map(y => (x, y)))
      .find { case (x, y) => input(x)(y) == 'S' }
      .get
    val end = (0 until input.size)
      .flatMap(x => (0 until input(0).size).map(y => (x, y)))
      .find { case (x, y) => input(x)(y) == 'E' }
      .get
    val visited = Array.ofDim[Boolean](s1, s2)
    val distances = Array.ofDim[Int](s1, s2)
    for (x <- 0 until s1; y <- 0 until s2) {
      visited(x)(y) = false
      distances(x)(y) = Int.MaxValue
    }
    var tent_distances: PriorityQueue[(Int, (Int, Int))] = PriorityQueue()(
      Ordering.by[(Int, (Int, Int)), Int](_._1).reverse
    )
    tent_distances.addOne((0, (start._1, start._2)))
    while (!visited(end._1)(end._2)) {
      val (dist, (curx, cury)) = tent_distances.dequeue
      if (!visited(curx)(cury)) {
        val curheight = height(input(curx)(cury))
        for ((dx: Int, dy: Int) <- Vector((-1, 0), (1, 0), (0, -1), (0, 1))) {
          val nx = curx + dx;
          val ny = cury + dy;
          if (nx >= 0 && nx < s1 && ny >= 0 && ny < s2) {
            if (!visited(nx)(ny)) {
              val nheight = height(input(nx)(ny))
              if (nheight - curheight <= 1) {
                val ndist = dist + 1
                val odist = distances(nx)(ny)
                if (ndist < odist) {
                  tent_distances.addOne((ndist, (nx, ny)))
                }
              }
            }
          }
        }
        visited(curx)(cury) = true
        distances(curx)(cury) = dist
      }
    };
    distances(end._1)(end._2)
  }

  def p2(input: Array[Array[Char]]): Int = {
    val s1 = input.size
    val s2 = input(0).size
    val start = (0 until input.size)
      .flatMap(x => (0 until input(0).size).map(y => (x, y)))
      .find { case (x, y) => input(x)(y) == 'E' }
      .get
    val visited = Array.ofDim[Boolean](s1, s2)
    val distances = Array.ofDim[Int](s1, s2)
    for (x <- 0 until s1; y <- 0 until s2) {
      visited(x)(y) = false
      distances(x)(y) = Int.MaxValue
    }
    var tent_distances: PriorityQueue[(Int, (Int, Int))] = PriorityQueue()(
      Ordering.by[(Int, (Int, Int)), Int](_._1).reverse
    )
    tent_distances.addOne((0, (start._1, start._2)))
    var found_any_end: Option[(Int, Int)] = None;
    while (found_any_end.isEmpty) {
      val (dist, (curx, cury)) = tent_distances.dequeue
      val curheight = height(input(curx)(cury))
      if (!visited(curx)(cury)) {
        for ((dx: Int, dy: Int) <- Vector((-1, 0), (1, 0), (0, -1), (0, 1))) {
          val nx = curx + dx;
          val ny = cury + dy;
          if (nx >= 0 && nx < s1 && ny >= 0 && ny < s2) {
            if (!visited(nx)(ny)) {
              val nheight = height(input(nx)(ny))
              if (nheight - curheight >= -1) {
                val ndist = dist + 1
                val odist = distances(nx)(ny)
                if (ndist < odist) {
                  tent_distances.addOne((ndist, (nx, ny)))
                }
              }
            }
          }
        }
        visited(curx)(cury) = true
        distances(curx)(cury) = dist
        if (curheight == 0) {
          found_any_end = Some((curx, cury))
        }
      }
    };
    distances(found_any_end.get._1)(found_any_end.get._2)
  }
  val filename = args(0);
  val input_str = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  val input = input_str.map(_.toCharArray())
  println(p1(input))
  println(p2(input))
}
