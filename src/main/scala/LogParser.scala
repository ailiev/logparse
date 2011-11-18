import scala.util.parsing.combinator.{
  RegexParsers
}
import java.{io=>jio}
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

sealed trait Node
case class S1(name:String, start:DateTime, end:DateTime, children:List[Node]) extends Node
case class S2(name:String, start:DateTime, end:DateTime, children:List[S3]) extends Node
case class S3(name:String, start:DateTime, end:DateTime) extends Node

trait LogParser extends RegexParsers
{
  val TIME_PARSER =
    DateTimeFormat.forPattern("HH:mm")

  def main(args:Array[String]) = {
    val in = """10:27 [some stuff] - the-first-h3:
10:29 [some stuff] - yeah baby
10:29 [some stuff] - come on let's code"""
//    test (in, s3, S3("the-first-h3:",time(10,27),time(10,29)))
  }

  def time (h:Int,m:Int) = new DateTime(0).withHourOfDay(h).withMinuteOfHour(m)

//  override val whiteSpace = "\n"r
  override def skipWhitespace = false

  val linechars = ("[^\n]*"r)
  
  val eol:Parser[String] = "\n"

  val timestamp =  ("""\d\d:\d\d"""r) ^^
            (TIME_PARSER.parseDateTime(_))

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
  val h1_end = (prefix <~ h1_end_text) ~ linechars ^^
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
  def s1_child : Parser[Node] = s1 | s2
  def s1 = ((h1) <~ (genline*)) ~ (s1_child*) ~ h1_end ^^
            { case ((t,title)) ~ s2s ~ ((t_end,title_end)) =>
              if (title_end != title) throw new RuntimeException("Parse error")
              else S1(title, t, t_end, s2s) }
  val all = ((genline*) ~> s1+) <~ (genline*)

  def noneOf[E](ps:List[Parser[E]]) = not (ps.reduce(_ | _))
}
