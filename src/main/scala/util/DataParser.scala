package util

import scala.util.parsing.combinator.RegexParsers

/** DataParser trait is used to parse the read file rows
  * in order to identify from each row the resource and the
  * activities performed.
  * It has three methods:
  * resource for parsing resources from a row
  * activity for parsing activities from a row
  * mapResourceActivity is a parser combinator for parsing both
  * resources and activities.
  */
trait DataParser extends RegexParsers {
  def resource: Parser[String]
  def activity: Parser[List[String]]
  def mapResourceActivity: Parser[ResourceActivities]
}

/** The DataParser object extends DataParser as well as RegexParsers
  * because we go lexical analysis
  * It provides the implementation for the three methods of DataParser
  * Trait
  */
object DataParser extends RegexParsers with DataParser {

  /** Here, It provides parsing a resource from the row file using Regex
    * """[a-zA-Z]+\t""".r is the regular expression for parsing a resource
    * If it succeeds the function on the right is executed which is in our case
    * does a conversion to String and trims it
    */
  val resource: Parser[String] = "[a-zA-Z]+\t".r ^^ {
    _.toString.trim
  }

  /** The parsing here uses a different regular expression as well as
    * a different function. If the parsing succeeds the result is splited and
    * stored in a List
    */
  val activity: Parser[List[String]] =
    "[a-zA-Z,]+".r ^^ { s =>
      s.split(",").toList
    }

  /** The parsing combines the two previous separate parsers
    * the result is stored into a Hashmap where the key is
    * The resource and the values are the activities performed
    */
  def mapResourceActivity: Parser[ResourceActivities] =
    resource ~ activity ^^ {
      case r ~ a => ResourceActivities(r, a)
    }

}
case class ResourceActivities(resource: String, activities: List[String])
