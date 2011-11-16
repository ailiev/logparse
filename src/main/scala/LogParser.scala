import scala.util.parsing.combinator.{
  RegexParsers
}
import java.{io=>jio}
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

sealed trait LogNode
case class Target(name:String, start:DateTime, end:DateTime) extends LogNode
case class Module(name:String, targets:List[Target]) extends LogNode

object LogParser extends RegexParsers
{
  val TIME_PARSER =
    DateTimeFormat.forPattern("HH:mm")

  def main(args:Array[String]) = {
    System.out.println(parse(all, new jio.FileReader("src/test/resources/test.txt")).get.mkString("\n"))
  }

//  override val whiteSpace = "\n"r
  override def skipWhitespace = false

  val linechars = ("[^\n]*"r)
  val eol = "\n"
  val timestamp =  ("""\d\d:\d\d"""r) ~ " " ^^
            { case t ~ _ => TIME_PARSER.parseDateTime(t) }
  val h1_text = "end h1:"
  val h2_text = "h2:"
  val h1 = (timestamp <~ h1_text) ~ linechars <~ eol ^^
            { case t ~ title => (t,title) }
  val h2 = (timestamp <~ h2_text) ~ linechars <~ eol ^^
            { case t ~ title => (t,title) }
  val genline = timestamp <~ (not (h1_text | h2_text)) <~ linechars <~ eol
  val target = h2 ~ (genline*) ^^
            { case ((start,title)) ~ times =>
              Target(title, start, times.lastOption.getOrElse(start)) }
  val module = (target*) ~ h1 ^^
            { case ts ~ ((t,title)) => Module(title, ts) }
  val all = module+
  
}
