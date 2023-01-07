import scala.collection.mutable.HashMap

object d18 extends App {
  case class Cube(x: Int, y: Int, z: Int)
  def parseLine(inp: String): Cube = {
    val r = """(\d+),(\d+),(\d+)""".r
    inp match {
      case r(x, y, z) => Cube(x.toInt, y.toInt, z.toInt)
    }
  }

  case class Coord(x: Int, y: Int, z: Int)
  type Face = Set[Coord]

  def get_faces(cube: Cube): List[Face] = {
    List(
      // Face 1, down
      Set(
        Coord(cube.x, cube.y, cube.z),
        Coord(cube.x + 1, cube.y, cube.z),
        Coord(cube.x + 1, cube.y + 1, cube.z),
        Coord(cube.x, cube.y + 1, cube.z)
      ),
      // Face 2, right
      Set(
        Coord(cube.x + 1, cube.y, cube.z),
        Coord(cube.x + 1, cube.y + 1, cube.z),
        Coord(cube.x + 1, cube.y + 1, cube.z + 1),
        Coord(cube.x + 1, cube.y, cube.z + 1)
      ),
      // Face 3, back
      Set(
        Coord(cube.x, cube.y + 1, cube.z),
        Coord(cube.x + 1, cube.y + 1, cube.z),
        Coord(cube.x + 1, cube.y + 1, cube.z + 1),
        Coord(cube.x, cube.y + 1, cube.z + 1)
      ),
      // Face 4, front
      Set(
        Coord(cube.x, cube.y, cube.z),
        Coord(cube.x + 1, cube.y, cube.z),
        Coord(cube.x + 1, cube.y, cube.z + 1),
        Coord(cube.x, cube.y, cube.z + 1)
      ),
      // Face 5, left
      Set(
        Coord(cube.x, cube.y, cube.z),
        Coord(cube.x, cube.y, cube.z + 1),
        Coord(cube.x, cube.y + 1, cube.z + 1),
        Coord(cube.x, cube.y + 1, cube.z)
      ),
      // Face 6, top
      Set(
        Coord(cube.x, cube.y, cube.z + 1),
        Coord(cube.x + 1, cube.y, cube.z + 1),
        Coord(cube.x + 1, cube.y + 1, cube.z + 1),
        Coord(cube.x, cube.y + 1, cube.z + 1)
      )
    )
  }

  def p1(inp: Array[Cube]): Int = {
    var face_count = HashMap[Face, Int]();
    for (
      cube <- inp;
      face <- get_faces(cube)
    ) {
      face_count(face) = face_count.getOrElse(face, 0) + 1
    }
    face_count.filter { case (_, c) => c == 1 }.size
  }

  def dfs(ffill:  Array[Array[Array[Int]]], cube: Cube):List[Cube]  = {
    
  }

  def p2(inp: Array[Cube]): Int = {
    val max_x = inp.map(_.x).max + 1
    val max_y = inp.map(_.y).max + 1
    val max_z = inp.map(_.z).max + 1
    // 0 is empty, 1 is filled, 2 is interior, 3 is exterior
    var ffill = Array.fill[Int](max_x, max_y, max_z)(0)
    for (cube <- inp) {
      ffill(cube.x)(cube.y)(cube.z) = 1
    }
    for (x <- 0 until max_x; y <- 0 until max_y; z <- 0 until max_z) {
      if (ffill(x)(y)(z) ==  0)  {
        val cubes = dfs(ffill, Cube(x, y, z))
      }
    }
    ???
  }
  val inp = common.readFile(args(0)).map(parseLine)
  println(p1(inp))
}
