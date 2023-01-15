object d20 extends App {

  def p1(inp: Array[Int]): Int = {
    val n = inp.size
    var idx_to_pos = Array.ofDim[Int](n)
    var pos_to_idx = Array.ofDim[Int](n)
    def swap(
        idx_to_pos: Array[Int],
        pos_to_idx: Array[Int],
        posa: Int,
        posb: Int
    ) = {
      val posa0 = math.floorMod(posa, idx_to_pos.size)
      val posb0 = math.floorMod(posb, idx_to_pos.size)
      val idxa = pos_to_idx(posa0)
      val idxb = pos_to_idx(posb0)
      idx_to_pos(idxa) = posb0
      idx_to_pos(idxb) = posa0
      pos_to_idx(posa0) = idxb
      pos_to_idx(posb0) = idxa
    }
    for (idx <- 0 until n) {
      idx_to_pos(idx) = idx
      pos_to_idx(idx) = idx
    }
    var output_array = Array.ofDim[Int](n)
    for (idx <- 0 until n) {
      var v = inp(idx)
      val start_pos = idx_to_pos(idx)
      // "Delete" item from the list
      for (pos <- start_pos + 1 until n) {
        val this_idx = pos_to_idx(pos)
        idx_to_pos(this_idx) = pos - 1
        pos_to_idx(pos - 1) = this_idx
      }
      // Logic here is a little dodgy
      var end_pos = math.floorMod(start_pos + v, n - 1)
      if (end_pos == 0) end_pos = n - 1
      // Add items after
      for (pos <- (n - 2) to end_pos by -1) {
        val this_idx = pos_to_idx(pos)
        idx_to_pos(this_idx) = pos + 1
        pos_to_idx(pos + 1) = this_idx
      }
      idx_to_pos(idx) = end_pos
      pos_to_idx(end_pos) = idx

      for (i <- 0 until n) {
        output_array(i) = inp(pos_to_idx(i))
      }
      println(inp(idx), ": ", output_array.mkString(","))
    }
    var zidx = output_array.indexWhere(_ == 0)
    val (c0, c1, c2) = (
      output_array(math.floorMod(1000 + zidx, n)),
      output_array(math.floorMod(2000 + zidx, n)),
      output_array(math.floorMod(3000 + zidx, n))
    )
    println((c0, c1, c2))
    (c0 + c1 + c2)
  }

  val inp = common.readFile(args(0)).map(_.toInt)
  println(inp.mkString(","))
  println(p1(inp))
}
