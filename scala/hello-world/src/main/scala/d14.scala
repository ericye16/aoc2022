object d14 extends App {
  def parseLine(input: String): Array[(Int, Int)] = {
    input.split(" -> ").map { x =>
      val v = x.split(",")
      (v(0).toInt, v(1).toInt)
    }
  }

  def find_maxes(a: Array[Array[(Int, Int)]]): (Int, Int) = {
    val fa = a.flatten[(Int, Int)]
    fa.reduce[(Int, Int)] { case ((x, y), (xr, yr)) =>
      (Math.max(x, xr), Math.max(y, yr))
    }
  }

  def drawRocks(a: Array[Array[Char]], lines: Array[Array[(Int, Int)]]) = {
    for (l <- lines) {
      var idx = 0;
      var prev = l(idx)
      idx += 1
      while (idx < l.size) {
        val next = l(idx)
        val diff = (next._1 - prev._1, next._2 - prev._2)
        val nsteps = Math.max(diff._1.abs, diff._2.abs)
        for (t <- 0 to nsteps) {
          a(prev._1 + t * diff._1 / nsteps)(prev._2 + t * diff._2 / nsteps) =
            '#'
        }
        prev = next
        idx += 1
      }
    }
  }

  def dropRock(a: Array[Array[Char]]): Boolean = {
    val start_point = (500, 0)
    var num_rocks = 0
    var rock_pos = start_point
    var stopped = false
    while (!stopped) {
      if (rock_pos._2 == a(0).size - 1) {
        return true
      } else {
        val down = (rock_pos._1, rock_pos._2 + 1)
        val left = (rock_pos._1 - 1, rock_pos._2 + 1)
        val right = (rock_pos._1 + 1, rock_pos._2 + 1)
        if (a(down._1)(down._2) == '.') {
          rock_pos = down
        } else if (a(left._1)(left._2) == '.') {
          rock_pos = left
        } else if (a(right._1)(right._2) == '.') {
          rock_pos = right
        } else {
          stopped = true
          num_rocks += 1
          a(rock_pos._1)(rock_pos._2) = 'o'
        }
      }
    }
    false
  }

  def p1(in: Array[Array[(Int, Int)]]): Int = {
    val (mx, my) = find_maxes(in)
    val minx = in.flatten[(Int, Int)].foldLeft[Int](Int.MaxValue) {
      case (acc, (xr, yr)) => (Math.min(acc, xr))
    }
    // Don't really want to deal with 0 indexing
    var a = Array.fill[Char](mx + 1, my + 1)('.')
    drawRocks(a, in)

    var num_rocks = 0
    while (!dropRock(a)) {
      num_rocks += 1
      // println(num_rocks)
      // a.slice(minx, a.size).transpose.map(x => println(x.mkString))
    }
    num_rocks
  }

  def p2(in: Array[Array[(Int, Int)]]): Int = {
    val (mx, my) = find_maxes(in)
    val minx = in.flatten[(Int, Int)].foldLeft[Int](Int.MaxValue) {
      case (acc, (xr, yr)) => (Math.min(acc, xr))
    }
    // Don't really want to deal with 0 indexing
    // Add 500 since worst case is a pyramid more or less
    var a = Array.fill[Char](mx + 500 + 1, my + 1 + 2)('.')
    drawRocks(a, in)
    for (x <- 0 until a.size) {
      a(x)(my + 2) = '#'
    }

    var num_rocks = 0
    while (a(500)(0) != 'o') {
      dropRock(a)
      num_rocks += 1
      // println(num_rocks)
      // a.slice(minx, a.size).transpose.map(x => println(x.mkString))
    }
    num_rocks
  }

  val input = common.readFile(args(0))
  val lines = input.map(parseLine)
  println(p1(lines))
  println(p2(lines))
  // lines.map(_.map(println))
}
