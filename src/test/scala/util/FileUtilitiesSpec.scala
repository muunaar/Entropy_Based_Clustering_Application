package util

import org.scalatest.FlatSpec
import org.scalatest.matchers.should.Matchers

class FileUtilitiesSpec extends FlatSpec with Matchers {

  import FileUtilities._

  "Path" should "be a string" in {
    /*
      What are you trying to test here? getPath returns an IO effect, a description of a method that asks for an user
      input not the user input itself.
      toString will return something like IO$1146825051, I think that's not what you're expecting.
      If the idea is that the path should match a specific pattern then you should validate that in the getPath
      method itself  (validating if the file exists would be better though) and then test here with some valid and
      invalid inputs.
     */
    getPath.toString() should include regex """[A-Za-z.a-zA-z]""".r
  }
  "Path" should "not be empty" in {
    //Same here
    assert(getPath.toString().isEmpty != true)
  }

  "Beta" should "be in range" in {
    /*
    You could add some more cases here (closer to the range limits):
      assert(compareBetaToRange(0.5f) == false)
      assert(compareBetaToRange(0.9f) == false)
      assert(compareBetaToRange(0.5f) == false)

      assert(compareBetaToRange(0.89f) == true) //this one will fail
      assert(compareBetaToRange(0.51f) == true)

      could also test with a negative value
      assert(compareBetaToRange(-0.6f) == false)
     */
    assert(compareBetaToRange(0.23f) == false)
  }

  "Case zero" should "Return false" in {
    //This one could be part of the test above
    assert(compareBetaToRange(0.0f) == false)
  }
}
