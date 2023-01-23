import scala.collection.mutable.HashMap
object d21 extends App {
  sealed abstract class Expr;
  case class Lit(v: Long) extends Expr;
  case class Comb(a: String, b: String, op: Char) extends Expr;
  def parse(inp: String): (String, Expr) = {
    val sp = inp.split(": ")
    val name = sp(0)
    val lit_r = """(\d+)""".r
    val expr_r = """(\w+) ([+*/-]) (\w+)""".r
    (
      name,
      sp(1) match {
        case lit_r(v)         => Lit(v.toLong)
        case expr_r(a, op, b) => Comb(a, b, op(0))
      }
    )
  }

  type ExprTree = HashMap[String, Expr]

  def p1(inp: ExprTree): BigInt = {
    def eval_expr(name: String, tree: ExprTree): BigInt = {
      tree(name) match {
        case Lit(v) => v
        case Comb(a, b, op) => {
          val va = eval_expr(a, tree)
          val vb = eval_expr(b, tree)
          if (op == '+') va + vb
          else if (op == '*') va * vb
          else if (op == '/') va / vb
          else va - vb
        }
      }
    }
    eval_expr("root", inp)
  }

  sealed abstract class SymbTree;
  case class SymbLit(v: Long) extends SymbTree;
  case class Humn() extends SymbTree;

  def p2(inp: ExprTree): Long = {
    val (root_a, root_b) = inp("root") match {
      case Comb(a, b, op) => (a, b)
      case _              => ???
    }
    def eval_expr(name: String, tree: ExprTree): String = {
      if (name == "humn") "humn"
      else {
        tree(name) match {
          case Lit(v) => v.toString
          case Comb(a, b, op) => {
            val va = eval_expr(a, tree)
            val vb = eval_expr(b, tree)
            "(" + va + op + vb + ")"
            // if (op == '+') va + vb
            // else if (op == '*') va * vb
            // else if (op == '/') va / vb
            // else va - vb
          }
        }
      }
    }
    println(eval_expr(root_a, inp))
    println(eval_expr(root_b, inp))
    ???
  }

  val inp = common.readFile(args(0)).map(parse)
  var hm = HashMap[String, Expr]()
  inp.foreach {
    case (name, expr) => {
      hm += (name -> expr)
    }
  }
  println(p1(hm))
  println(p2(hm))
}
