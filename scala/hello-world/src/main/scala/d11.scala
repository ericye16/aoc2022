import scala.util.Using
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayDeque
object d11 extends App {

  sealed abstract class Operand
  case class Literal(n: Int) extends Operand
  case class Old() extends Operand

  object Operator extends Enumeration {
    type Operator = Value

    val Add, Multiply = Value
  }
  import Operator._

  class Monkey(
      var items: ArrayDeque[Long],
      val op_1: Operand,
      val op_2: Operand,
      val op: Operator,
      val test_div: Int,
      val true_monkey: Int,
      val false_monkey: Int
  ) {
    override def toString =
      s"Monkey($items, $op_1, $op, $op_2, $test_div, $true_monkey, $false_monkey)"

    // Deep copy for clone
    override def clone(): Monkey =
      new Monkey(
        items.clone,
        op_1,
        op_2,
        op,
        test_div,
        true_monkey,
        false_monkey
      )
  }

  def parseMonkeys(input: Array[String]): Array[Monkey] = {
    var line_idx = 0;
    var monkeys: ArrayBuffer[Monkey] = ArrayBuffer()
    val monkey_idx_re = """Monkey \d:""".r
    val starting_re = """  Starting items: ([, \d]+)""".r
    val operation_re = """  Operation: new = (\d+|old) (\+|\*) (\d+|old)""".r
    val divisible_re = """  Test: divisible by (\d+)""".r
    val true_re = """    If true: throw to monkey (\d)""".r
    val false_re = """    If false: throw to monkey (\d)""".r
    while (line_idx < input.size) {
      input(line_idx) match {
        case monkey_idx_re() =>
      }
      line_idx += 1
      val items: Array[Long] = input(line_idx) match {
        case starting_re(items_str) => {
          items_str.split(",").map(_.trim).map(_.toLong)
        }
      }
      line_idx += 1
      val (op1: Operand, operand: Operator, op2: Operand) =
        input(line_idx) match {
          case operation_re(op1_0, opo, op2_0) =>
            (
              op1_0 match {
                case "old" => Old()
                case v     => Literal(v.toInt)
              },
              opo match {
                case "+" => Add
                case "*" => Multiply
              },
              op2_0 match {
                case "old" => Old()
                case v     => Literal(v.toInt)
              }
            )
        }
      line_idx += 1
      val divisor = input(line_idx) match {
        case divisible_re(v) => v.toInt
      }
      line_idx += 1
      val true_monkey = input(line_idx) match {
        case true_re(m) => m.toInt
      }
      line_idx += 1
      val false_monkey = input(line_idx) match {
        case false_re(m) => m.toInt
      }
      line_idx += 1
      monkeys += (
        new Monkey(
          items.to(ArrayDeque),
          op1,
          op2,
          operand,
          divisor,
          true_monkey,
          false_monkey
        )
      )
      while (line_idx < input.size && input(line_idx).isEmpty) {
        line_idx += 1
      }
    }
    monkeys.toArray
  }

  def operation(old: Long, op1: Operand, op: Operator, op2: Operand): Long = {
    val o1 = op1 match {
      case Old()      => old
      case Literal(l) => l
    };
    val o2 = op2 match {
      case Old()      => old
      case Literal(l) => l
    }
    op match {
      case Add      => o1 + o2
      case Multiply => o1 * o2
    }
  }

  def p1(monkeys: Array[Monkey]): Int = {
    val num_monkeys = monkeys.size
    var monkeys0 = monkeys
    var num_inspected: Array[Int] = Array.ofDim(num_monkeys)
    for (i <- 0 until num_monkeys) {
      num_inspected(i) = 0
    }
    for (r <- 0 until 20) {
      for (m <- 0 until num_monkeys) {
        while (!monkeys0(m).items.isEmpty) {
          var worry = monkeys0(m).items.removeHead()
          worry =
            operation(worry, monkeys0(m).op_1, monkeys0(m).op, monkeys0(m).op_2)
          worry /= 3
          val next_monkey = if (worry % monkeys0(m).test_div == 0) {
            monkeys0(m).true_monkey
          } else {
            monkeys0(m).false_monkey
          }
          monkeys0(next_monkey).items.append(worry)
          num_inspected(m) += 1
        }
      }
    }
    // println(num_inspected)
    num_inspected.sorted(Ordering.Int.reverse).take(2).reduce((a, b) => a * b)
  }

  def p2(monkeys: Array[Monkey]): Long = {
    val num_monkeys = monkeys.size
    var monkeys0 = monkeys
    var num_inspected: Array[Long] = Array.ofDim(num_monkeys)
    for (i <- 0 until num_monkeys) {
      num_inspected(i) = 0
    }
    val lcm = monkeys0.map(_.test_div).reduce((a, b) => a * b)
    // println(s"lcm: $lcm")
    for (r <- 0 until 10000) {
      for (m <- 0 until num_monkeys) {
        while (!monkeys0(m).items.isEmpty) {
          var worry = monkeys0(m).items.removeHead()
          worry =
            operation(worry, monkeys0(m).op_1, monkeys0(m).op, monkeys0(m).op_2)
          worry %= lcm
          val next_monkey = if (worry % monkeys0(m).test_div == 0) {
            monkeys0(m).true_monkey
          } else {
            monkeys0(m).false_monkey
          }
          monkeys0(next_monkey).items.append(worry)
          num_inspected(m) += 1
        }
      }
      // if (r % 1000 == 0) {
      //   println(num_inspected.mkString(","))
      //   monkeys0.map(println)
      // }
    }
    num_inspected.sorted(Ordering.Long.reverse).take(2).reduce((a, b) => a * b)
  }

  val filename = args(0);
  val input_str = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  val monkeys = parseMonkeys(input_str)
  // monkeys.map(println)
  println(p1(monkeys.map(_.clone)))
  // monkeys.map(println)
  println(p2(monkeys))
}
