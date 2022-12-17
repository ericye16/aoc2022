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
                if (file_or_folder.isInstanceOf[File]) {
                  Some(acc + file_or_folder.asInstanceOf[File].size)
                } else {
                  val fol = file_or_folder.asInstanceOf[Folder];
                  if (fol.listed && fol.size.isDefined) {
                    Some(acc + fol.size.get)
                  } else {
                    None
                  }
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
        if (x.isInstanceOf[Folder]) {
          p1(x.asInstanceOf[Folder])
        } else 0
      )
      .sum + this_size
  }

  val filename = args(0);
  val input = Using(Source.fromFile(filename)) { source =>
    source.getLines().toArray
  }.get
  val tree = getTree(input)
  println(tree);
  println(p1(tree))
}
