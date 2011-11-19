import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.util.parsing.combinator.{
  RegexParsers, Parsers
}
import scala.util.parsing.input.CharSequenceReader
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

// inspiration for this test from
// http://henkelmann.eu/2011/01/29/an_introduction_to_scala_parser_combinators-part_3_unit_tests

@RunWith(classOf[JUnitRunner])
class Tests extends LogParser with FlatSpec with ShouldMatchers
{
  private def parsing[T](s:String)(implicit p:Parser[T]):T = {
    //wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(p)
    //we need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s) 
    phraseParser(input) match {
        case Success(t,_)     => t
        case NoSuccess(msg,_) => throw new IllegalArgumentException(
                                     "Could not parse '" + s + "': " + msg)
    }
  }

  private def assertFail[T](input:String)(implicit p:Parser[T]) {
    evaluating(parsing(input)(p)) should produce[IllegalArgumentException]
  }

  def time (h:Int,m:Int) = new DateTime(0).withHourOfDay(h).withMinuteOfHour(m)

  "The LogParser" should "parse DateTimes" in {
    val TIME_PARSER =
      DateTimeFormat.forPattern("HH:mm")

    val parserToTest = datetime(TIME_PARSER)

    val in = new CharSequenceReader("10:24")
    val parsed = parse(parserToTest, in)
    parsed match {
      case Success(t, rest) => {
        t should equal(time(10,24))
        rest.offset should equal(5)
      }
    }
//    parsing("  10:24") should equal(time(10,24))
  }

  "The LogParser" should "parse section 3's" in {
    implicit val parserToTest = s3

    parsing("""10:27 [some stuff] - the-first-h3:
10:29 [some stuff] - yeah baby
10:33 [some stuff] - come on let's code
""") should equal(S3("the-first-h3:",time(10,27),time(10,33)))
    
    parsing("""10:27 [some stuff] - the-first-h3:
""") should equal(S3("the-first-h3:",time(10,27),time(10,27)))
    
    parsing("""10:27 [some stuff] - the:first:h3:
10:29 [some stuff] - yeah baby        
""") should equal(S3("the:first:h3:",time(10,27),time(10,29)))     
  }
  
  "The LogParser" should "parse heading 2's" in {
    implicit val parserToTest = h2
parsing("""10:24 [some stuff] - ==
10:24 [some stuff] - [==]the first h2
""") should equal((time(10,24), "the first h2"))
  }

  "The LogParser" should "parse section 2's" in {
    implicit val parserToTest = s2

    parsing("""10:24 [some stuff] - ==
10:24 [some stuff] - [==]the first h2
10:27 [some stuff] - some more filler
""") should equal (S2("the first h2", time(10,24), time(10,24), List()))

    parsing("""10:24 [some stuff] - ==
10:24 [some stuff] - [==]the first h2
10:27 [some stuff] - some more filler
10:27 [some stuff] - the-first-h3:
10:29 [some stuff] - yeah baby
10:35 [some stuff] - come on let's code
""") should equal (S2("the first h2", time(10,24), time(10,35),
                    List(S3("the-first-h3:", time(10,27),time(10,35)))))
  }

  "The LogParser" should "parse section 1's" in {
    implicit val parserToTest = s1

    parsing("""10:24 [some stuff] - =>the first h1
10:24 [some stuff] - jdjsc kdk
10:24 [some stuff] - jkdlslow
10:32 [some stuff] - =<the first h1
""") should equal(S1("the first h1", time(10,24), time(10,32), List()))

    parsing("""10:24 [some stuff] - =>the first h1
10:25 [some stuff] - ==
10:25 [some stuff] - [==]the first h2
10:26 [some stuff] - some h3:
10:27 [some stuff] - jkdlslow
10:32 [some stuff] - =<the first h1
""") should equal(S1("the first h1", time(10,24), time(10,32),
                    List(S2("the first h2", time(10,25), time(10,27),
                        List(S3("some h3:", time(10,26), time(10,27)))))))

  }

}


