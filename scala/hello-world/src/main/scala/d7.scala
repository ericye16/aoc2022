import scala.util.Using
import scala.io.Source
import scala.collection.mutable.Map;

sealed trait FileOrFolder;
case class Folder(
    files: Map[String, FileOrFolder],
    var listed: Boolean = false,
    var size: Option[Int] = None
) extends FileOrFolder;
case class File(size: Int) extends FileOrFolder;

object d7 extends App {

  val maybeGetSize = (input: Folder) =>
    {
      if (input.listed) {
        input.files.values.foldLeft[Option[Int]](Some(0))(
          (acc: Option[Int], file_or_folder: FileOrFolder) => {
            acc match {
              case None => None
              case Some(acc) => {
                file_or_folder match {
                  case File(size)                      => Some(acc + size)
                  case Folder(files, true, Some(size)) => Some(acc + size)
                  case _                               => None
                }
              }
            }
          }
        );
      } else None
    }: Option[Int]

  val getTree = (input: Array[String]) => {
    val cd_regex = """\$ cd (\.\.|/|\w+)""".r;
    val ls_line = "$ ls";
    val ls_output_regex = """(dir|\d+) ([.\w]+)""".r;

    var stack: List[Folder] = List();
    for (line <- input) {
      val cd_match = cd_regex.findFirstMatchIn(line);
      cd_match match {
        case Some(value) => {
          // Before chdir-ing, attempt to sum up the directory size.
          if (!stack.isEmpty)
            stack.head.size = maybeGetSize(stack.head)
          val path = value.group(1)
          if (path == "/") {
            stack = List(Folder(Map()));
          } else if (path == "..") {
            stack = stack.tail;
          } else {
            stack =
              stack.head.files.get(path).get.asInstanceOf[Folder] +: stack;
          }
        }
        case None =>
      }
      if (line == ls_line) {
        stack.head.listed = true;
      }
      val dir_output_match = ls_output_regex.findFirstMatchIn(line);
      dir_output_match match {
        case Some(value) => {
          val dir_or_size = value.group(1);
          val name = value.group(2);
          if (dir_or_size == "dir") {
            stack.head.files(name) = Folder(Map());
          } else {
            stack.head.files(name) = File(dir_or_size.toInt)
          }
        }
        case None =>
      }
    }
    for (dir <- stack) {
      dir.size = maybeGetSize(dir);
    }
    stack.last
  }

  def p1(root: Folder): Int = {
    val this_size = if (root.size.get <= 100000) {
      root.size.get
    } else {
      0
    };
    root.files.values
      .map(x =>
        x match {
          case File(size) => 0
          case f: Folder  => p1(f)
        }
      )
      .sum + this_size
  }

  def flatten_folders(f: FileOrFolder): Seq[Folder] = {
    f match {
      case _: File    => Seq()
      case fo: Folder => fo +: fo.files.values.toSeq.flatMap(flatten_folders)
    }
  }

  def p2(root: Folder): Int = {
    val size_to_shrink = 30000000 - (70000000 - root.size.get);
    val all_folders: Seq[Folder] = flatten_folders(root)
    all_folders
      .sortBy { _.size.get }
      .filter(_.size.get > size_to_shrink)
      .head
      .size
      .get
  }

  val filename = args(0);
  val input = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  val tree = getTree(input)
  println(tree);
  println(p1(tree))
  println(p2(tree))
}
