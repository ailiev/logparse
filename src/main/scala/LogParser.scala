import scala.util.parsing.combinator.{
  RegexParsers
}
import java.{io=>jio}
import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime,MutableDateTime}
import org.joda.time.format.DateTimeFormatter
import org.joda.time.Period
import org.joda.time.Duration
import scala.util.parsing.input.{Position}

sealed abstract class Section{val name:String; val start:DateTime; val end:DateTime}
/** section 1 */
case class S1(name:String, override val start:DateTime, override val end:DateTime, children:List[Section])
  extends Section
case class S2(name:String, override val start:DateTime, override val end:DateTime, children:List[S3])
  extends Section
case class S3(name:String, override val start:DateTime, override val end:DateTime)
  extends Section

trait LogParser extends RegexParsers with DateTimeParsers
{
  val TIME_PARSER =
    DateTimeFormat.forPattern("HH:mm")

  override def skipWhitespace = false

  val linechars = "[^\n]*"r
  
  val eol:Parser[String] = "\n"

  // terminology:
  // h1, h2 etc: headings at level N
  // s1, s2 etc: sections at level N
  val h2_start_line1:Parser[String] = "=="
  val h2_start_line2:Parser[String] = "[==]"

  val h1_start_text:Parser[String] = "=>"
  val h1_end_text:Parser[String] = "=<"

  val keywords = List(h1_start_text, h1_end_text, h2_start_line1, h2_start_line2)


  val timestamp:Parser[DateTime] = datetime(TIME_PARSER)

  val prefix = timestamp <~ " [some stuff] - "

  val genline = prefix <~ (noneOf(regex(".*:"r) :: keywords)) <~ linechars <~ eol


  val h3 = prefix ~ (".*:"r) <~ eol ^^
      { case t ~ title => (t,title) }
  val s3 = h3 ~ (genline*) ^^
      { case ((start,title)) ~ times =>
              S3(title, start, times.lastOption.getOrElse(start)) }

  
  val h2 = (prefix <~ h2_start_line1 <~ eol <~
            prefix <~ h2_start_line2) ~ linechars <~ eol ^^
      { case t ~ title => (t,title) }
  val s2 = (h2 <~ (genline*)) ~ (s3*) ^^
      { case ((start,title)) ~ s3s =>
                val end = s3s.lastOption.map(_.end).getOrElse(start)
                S2(title, start, end, s3s)
      }

  // as an example, do h1 in monadic style
  val h1 =   for (t <- prefix;
                  _ <- h1_start_text;
                  title <- linechars;
                  _ <- eol) yield (t,title)
  val h1_end = (prefix <~ h1_end_text) ~ linechars <~ eol ^^
      { case t ~ title => (t, title) }
  val stray_h3 = h3
  lazy val s1_child : Parser[Section] = s1 | s2 // note not s3
  val s1 = ((h1) <~ ((genline|stray_h3)*)) ~ (s1_child*) ~ with_pos(h1_end) <~ (genline*) >>
      { case ((t,title)) ~ s2s ~ (( (t_end,title_end), pos )) =>
          if (title_end == title) success (S1(title, t, t_end, s2s))
          else err("Mismatched s1 tags: '%s' and '%s' at %s" format (title, title_end, pos))
      }


  val all = (genline*) ~> (s1+)

  // general combinators

  def with_pos[A](p:Parser[A]) = new Parser[(A,Position)] {
    def apply(in:Input) = {
      val pos = in.pos
      for (a <- p(in)) yield (a,pos)
    }
  }

  def any[E] (ps:TraversableOnce[Parser[E]]) = ps.reduce(_ | _)
  def noneOf[E](ps:TraversableOnce[Parser[E]]) = not (any(ps))
//  def noneOf = (any _) andThen (not _)


}


object LogParser extends LogParser
{
  def _1[A] : ((A,_)) => A = _._1
  def head[A] : List[A] => A = _.head

  def main(args:Array[String]) : Unit = {
    val parsed = parse(phrase(all), new jio.FileReader(args(0)))
    parsed match {
      case Success(nodes, _) => {
        val flat = flatten(nodes).
          // pick out the S3's only
          filter(head andThen { case S3(_,_,_) => true; case _ => false }).
          // what info do we want to show?
          map(ss => (duration(ss.head), ss.map(_.name))).
          sortBy(_._1)  // sort by duration
        System.out.println(flat.mkString("\n"))
      }
      case NoSuccess(msg, in) => System.err.println("Parse error at %s: %s" format (in.pos, msg))
    } 

  }
  
  def flatten(nodes:List[Section]) : List[List[Section]] = {
    // want each element in the result to be in reverse order, eg. S3, S2, S1, S1
    def walk1 : Section => List[List[Section]] =
      _ match {
        case s1@S1(_,_,_,_) => walkS1(s1)
        case s2@S2(_,_,_,_) => walkS2(s2)
        // no top-level S3's
      }

    def walkS1(s1:S1) : List[List[Section]] =
      List(s1) :: s1.children.flatMap(walk1).map(_ ++ List(s1))

    def walkS2(s2:S2) : List[List[Section]] =
      List(s2) :: s2.children.map(List(_,s2))
    
    nodes.flatMap(walk1)
  }

  def duration(n:Section) : Long = {
    new Duration(n.start, n.end).getStandardSeconds()
  }
  
}