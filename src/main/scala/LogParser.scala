import scala.util.parsing.combinator.{
  RegexParsers
}
import java.{io=>jio}
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

case class LogNode(time:DateTime)
case class Target(t2:DateTime, name:String) extends LogNode(t2)
case class Module(t2:DateTime, name:String, targets:List[Target]) extends LogNode(t2)

object LogParser extends RegexParsers
{
  val TIME_PARSER =
    DateTimeFormat.forPattern("HH:mm")

  def main(args:Array[String]) = {
    System.out.println(parse(all, new jio.InputStreamReader(System.in)).get.mkString("\n"))
  }

//  override val whiteSpace = "\n"r
  override def skipWhitespace = false

  val linechars = ("[^\n]*"r)
  val eol = "\n"
  val timestamp =  ("""\d\d:\d\d"""r) ~ " " ^^
                      { case t ~ _ => TIME_PARSER.parseDateTime(t) }
  val h1_text = "end h1:"
  val h2_text = "h2:"
  val h1 = timestamp ~ h1_text ~ linechars ~ eol ^^
            { case t ~ _ ~ title ~ _ => (t,title) }
  val h2 = timestamp ~ h2_text ~ linechars ~ eol ^^
            { case t ~ _ ~ title ~ _ => Target(t,title) }
  val genline = timestamp ~ (not (h1_text | h2_text)) ~ linechars ~ eol
//  val line = h1 | h2 | genline
  val target = h2 ~ (genline*) ^^
            { case h2 ~ _ => h2 }
  val module = (target*) ~ h1 ^^
            { case ts ~ ((t,title)) => Module(t, title, ts) }
//  val all = repsep(line, "\n")
  val all = module+
  
}
