import scala.util.parsing.combinator.{
  RegexParsers
}
import java.{io=>jio}
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime,MutableDateTime}
import org.joda.time.format.DateTimeFormatter

sealed abstract class Node(name:String, start:DateTime, end:DateTime)
case class S1(name:String, start:DateTime, end:DateTime, children:List[Node])
  extends Node(name,start,end)
case class S2(name:String, start:DateTime, end:DateTime, children:List[S3])
  extends Node(name,start,end)
case class S3(name:String, start:DateTime, end:DateTime)
  extends Node(name,start,end)

trait LogParser extends RegexParsers with DateTimeParsers
{
  val TIME_PARSER =
    DateTimeFormat.forPattern("HH:mm")

  def flatten(nodes:List[Node]) : List[List[Node]] = {
    // want each element in the result to be in reverse order, eg. S3, S2, S1, S1
    def walk1 : Node => List[List[Node]] =
      _ match {
        case s1@S1(_,_,_,_) => walkS1(s1)
        case s2@S2(_,_,_,_) => walkS2(s2)
        // no top-level S3's
      }

    def walkS1(s1:S1) : List[List[Node]] =
      List(s1) :: s1.children.flatMap(walk1).map(_ ++ List(s1))

    def walkS2(s2:S2) : List[List[Node]] =
      List(s2) :: s2.children.map(List(_,s2))
    
    nodes.flatMap(walk1)
  }
  
  def main(args:Array[String]) = {
    val in = """10:27 [some stuff] - the-first-h3:
10:29 [some stuff] - yeah baby
10:29 [some stuff] - come on let's code"""
//    test (in, s3, S3("the-first-h3:",time(10,27),time(10,29)))
  }

//  override val whiteSpace = "\n"r
  override def skipWhitespace = false

  val linechars = ("[^\n]*"r)
  
  val eol:Parser[String] = "\n"

  val timestamp = datetime(TIME_PARSER)

  val prefix = timestamp <~ " [some stuff] - "

  val h1_start_text:Parser[String] = "=>"
  val h1_end_text:Parser[String] = "=<"

  val h2_start_line1:Parser[String] = "=="
  val h2_start_line2:Parser[String] = "[==]"

  val keywords = List(h1_start_text, h1_end_text, h2_start_line1, h2_start_line2)

  // as an example, do h1 in monadic style
  val h1 =   for (t <- prefix;
                  _ <- h1_start_text;
                  title <- linechars;
                  _ <- eol) yield (t,title)
  val h1_end = (prefix <~ h1_end_text) ~ linechars <~ eol ^^
            { case t ~ title => (t, title) }
  val h2 = (prefix <~ h2_start_line1 <~ eol <~
            prefix <~ h2_start_line2) ~ linechars <~ eol ^^
            { case t ~ title => (t,title) }
  val h3 = prefix ~ (".*:"r) <~ eol ^^
            { case t ~ title => (t,title) }
  val genline = prefix <~ (noneOf(regex(".*:"r) :: keywords)) <~ linechars <~ eol
  val s3 = h3 ~ (genline*) ^^
            { case ((start,title)) ~ times =>
              S3(title, start, times.lastOption.getOrElse(start)) }
  val s2 = (h2 <~ (genline*)) ~ (s3*) ^^
            { case ((start,title)) ~ s3s =>
                val end = s3s.lastOption.map(_.end).getOrElse(start)
                S2(title, start, end, s3s)
                }
  lazy val s1_child : Parser[Node] = s1 | s2 // note not s3
  val s1 = ((h1) <~ (genline*)) ~ (s1_child*) ~ h1_end ^^
            { case ((t,title)) ~ s2s ~ ((t_end,title_end)) =>
              if (title_end != title) throw new RuntimeException("Parse error")
              else S1(title, t, t_end, s2s) }
  val all = ((genline*) ~> s1+) <~ (genline*)

  def any[E] (ps:TraversableOnce[Parser[E]]) = ps.reduce(_ | _)
  def noneOf[E](ps:TraversableOnce[Parser[E]]) = not (any(ps))
//  def noneOf = (any _) andThen (not _)


}
