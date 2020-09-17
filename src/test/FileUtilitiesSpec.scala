package util

import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

class FileUtilitiesSpec extends FlatSpec with Matchers {

  import FileUtilities._

  "Path" should "be a string" in {

    getPath.toString() should include regex """[A-Za-z.a-zA-z]""".r
  }
  "Path" should "not be empty" in {

    assert(getPath.toString().isEmpty != true)
  }

  "Beta" should "be in range" in {

    assert(compareBetaToRange(0.23f) == false)
  }

  "Case zero" should "Return false" in {

    assert(compareBetaToRange(0.0f) == false)
  }
}
