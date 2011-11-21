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

  val t = for (m <- 0 until 20) yield time(10,m)

  "flatten" should "work" in {
    LogParser.flatten(List(S1("s1", t(0), t(1), Nil))
        ) should equal (List(List(S1("s1", t(0), t(1), Nil))))

    val s3_1 = S3("s3_1", t(2),  t(3))
    val s3_2 = S3("s3_2", t(4),  t(6))
    val s2   = S2("s2",   t(1),  t(6),  List(s3_1,s3_2))
    val s2_2 = S2("s2",   t(11), t(14), Nil)    
    val s1_1 = S1("s1",   t(0),  t(9),  List(s2))
    val s1_2 = S1("s1",   t(10), t(15), List(s2_2))

    LogParser.flatten(List(s1_1, s1_2)
    ) should equal (
        List(List(s1_1),
            List(s2, s1_1),
            List(s3_1, s2, s1_1),
            List(s3_2, s2, s1_1),
            List(s1_2),
            List(s2_2, s1_2)
        )
    )
  }

  "Unbalanced h1's" should "produce an error" in {
    implicit val parserToTest = s1

    parsing("""12:45 [some stuff] - =>some h1
12:47 [some stuff] - =<another h1
""") should equal(null)
  }

}


