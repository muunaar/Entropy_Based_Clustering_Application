package util

import org.scalatest.FlatSpec

import scala.util.parsing.combinator.RegexParsers

class DataParserSpec extends FlatSpec with RegexParsers {

  import DataParser._

  "Resource/ Activity  parser" should "succeed on a well formatted row" in {

    DataParser.parse(mapResourceActivity, "Mouna   Examine,Validate")
  }

  "Parsing a file row" should "fail on not formatted rows" in {

    DataParser.parse(mapResourceActivity, "Mouna   Examine ; Validate")
    fail("The row  is not  formatted according regex defined")

  }
}
