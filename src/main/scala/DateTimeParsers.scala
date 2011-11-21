import scala.util.parsing.combinator.Parsers
import org.joda.time.format.DateTimeFormatter
import org.joda.time.DateTime
import org.joda.time.MutableDateTime

/** Parsers which use a Joda time DateTimeFormatter to parse text into a
 * DateTime object.
 */
trait DateTimeParsers extends Parsers
{
  implicit def datetime(format: DateTimeFormatter): Parser[DateTime] = new Parser[DateTime] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val dest = new MutableDateTime(0)
      val timeParser = format.getParser
      val workString = source.subSequence(offset,
          math.min(offset+timeParser.estimateParsedLength, source.length)).toString
      val rc = format.parseInto(dest, workString, 0)
      if (rc >= 0) Success(dest.toDateTime(dest.getChronology), in.drop(rc))
      else {
        val found = if (offset == source.length()) "end of source" else "`"+source.charAt(offset)+"'" 
        Failure("string matching date/time format " + format + " expected but " + found + " found",
            in)
      }
    }
  }  
}