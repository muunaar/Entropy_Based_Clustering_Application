package util

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

class ClusteringSpec extends FlatSpec with Matchers {
  import Clustering._
  "Minimum Resource" should " have the lowest entropy value" in {

    val entopy: HashMap[String, Double] =
      HashMap("Haley" -> 0.4, "Nite" -> 0.95, "Mike" -> 0.2)
    val minEntropy = minEntropyResource(entopy)
    minEntropy shouldBe "Mike"
  }

  "Minimum resource in Entropy Map where all values are Null " should "return one of the resources with null entropy value" in {

    val entopy: HashMap[String, Double] =
      HashMap("Haley" -> 0.0, "Nite" -> 0.0, "Mike" -> 0.0)

    val minEntropy = minEntropyResource(entopy)
    minEntropy shouldBe "Haley"
  }

  "if the similarity threshold is greater than beta" should "return true " in {

    /* add a case where the value is equal to the beta
      assert(compareSimilarityIndexToBeta(0.5, 0.5))
     */
    assert(compareSimilarityIndexToBeta(0.54, 0.5))
  }

  "if the similarity threshold is less than beta" should "return false" in {

    assert(!compareSimilarityIndexToBeta(0.10, 0.6))
  }

  "If the resource is positioned as a key in similarity map" should "return true " in {

    val testMap = HashMap(
      "Haley" -> HashMap("Mike" -> 0.25, "Deran" -> 0.23),
      "Deran" -> HashMap("Mike" -> 0.0),
    )

    assert(checkIfKey("Haley", testMap))
    // add this one too to make sure your method works with multiple keys
    // assert(checkIfKey("Deran", testMap))
  }

  "If the resource is not positioned as a key in similarity map" should "return false" in {

    val testMap = HashMap(
      "Haley" -> HashMap("Mike" -> 0.25, "Deran" -> 0.23),
      "Deran" -> HashMap("Susan" -> 0.0),
    )

    /*
      Sue is not even part of your testMap, would be nice to also add a test with Susan for example
     */
    assert(!checkIfKey("Sue", testMap))
  }

  "If a resource is positioned in a key in values map in similarity map" should "return true" in {

    val testMap = HashMap(
      "Haley" -> HashMap("Mike" -> 0.25, "Deran" -> 0.23),
      "Deran" -> HashMap("Susan" -> 0.0),
    )

    assert(checkIfValue("Susan", testMap))
    /*
      Add Mike and Deran here too
      assert(checkIfValue("Mike", testMap))
      assert(checkIfValue("Deran", testMap))
     */
  }

  "If a resource is not positioned in a key in values map in similarity map" should "return false" in {

    val testMap = HashMap(
      "Haley" -> HashMap("Mike" -> 0.25, "Deran" -> 0.23),
      "Deran" -> HashMap("Susan" -> 0.0),
    )

    assert(checkIfValue("Sam", testMap) == false)
    /*
      Sam is not even part of your testMap, would be nice to also add a test with Haley
     */
  }

  "Removing elements from Entropy map" should "return an entropy map without the resource passed as parameter" in {
    val entropy =
      HashMap("Deran" -> 0.44, "Mouna" -> 0.24, "Nate" -> 0.23, "Felix" -> 0.6)

    assert(
      removeResourceFromEntropy(entropy, "Mouna") == HashMap(
        "Deran" -> 0.44,
        "Nate" -> 0.23,
        "Felix" -> 0.6
      )
    )
  }

  "In case of the resource is positioned as value, It" should "return a tuple of entropy map without the elements belonging to the cluster which is the second element of the tuple" in {

    val entropy =
      HashMap(
        "Deran" -> 0.44,
        "Mouna" -> 0.24,
        "Felix" -> 0.6,
        "Sue" -> 0.46,
        "Mike" -> 0.5
      )

    val m = HashMap(
      "Mike" -> HashMap("Deran" -> 0.34, "Nate" -> 0.43, "Sue" -> 0.1),
      "Felix" -> HashMap("Nate" -> 0.31)
    )

    assert(
      caseValue(m, entropy, "Nate", 0.26) == (HashMap(
        "Deran" -> 0.44,
        "Mouna" -> 0.24,
        "Sue" -> 0.46,
      ), ("Nate", Set("Felix", "Mike")))
    )
  }

  "It" should "return a single node cluster if the threshold criteria is not verified or the resource already belongs to a cluster" in {

    val entropy =
      HashMap("Deran" -> 0.44, "Mouna" -> 0.24, "Sue" -> 0.46, "Mike" -> 0.5)

    val m = HashMap(
      "Mike" -> HashMap("Deran" -> 0.34, "Nate" -> 0.23, "Sue" -> 0.1),
      "Felix" -> HashMap("Nate" -> 0.31)
    )

    assert(
      caseValue(m, entropy, "Nate", 0.26) == (HashMap(
        "Deran" -> 0.44,
        "Mouna" -> 0.24,
        "Sue" -> 0.46,
        "Mike" -> 0.5
      ), ("Nate", Set()))
    )
  }

  "In case of the resource is positioned as key, It" should "return a tuple of entropy map without the elements belonging to the cluster which is the second element of the tuple" in {

    val m = HashMap(
      "Deran" -> HashMap("Mouna" -> 0.34, "Nate" -> 0.33, "Sue" -> 0.03),
      "Felix" -> HashMap("Mouna" -> 0.01)
    )

    val entropy =
      HashMap("Mouna" -> 0.27, "Nate" -> 0.25, "Felix" -> 0.6, "Sue" -> 0.03)

    assert(
      caseKey(m, entropy, "Deran", 0.26) == (HashMap(
        "Felix" -> 0.6,
        "Sue" -> 0.03,
      ), ("Deran", Set("Mouna", "Nate")))
    )
  }

  "It case of one of the criteria is not verified" should "return a tuple of entropy map not modified and a single node cluster" in {

    val entropy =
      HashMap("Mouna" -> 0.24, "Sue" -> 0.46, "Nate" -> 0.15)

    val m = HashMap(
      "Mike" -> HashMap("Deran" -> 0.24, "Nate" -> 0.23, "Sue" -> 0.1),
      "Felix" -> HashMap("Nate" -> 0.31)
    )

    assert(
      caseKey(m, entropy, "Mike", 0.26) == (HashMap(
        "Mouna" -> 0.24,
        "Sue" -> 0.46,
        "Nate" -> 0.15
      ), ("Mike", Set()))
    )
  }

  "Fusing the elements belonging to the same cluster " should " return a tuple of the cluster center and the resources belonging to it" in {

    assert(
      mergeResults(
        (
          HashMap("Felix" -> 0.6, "Sue" -> 0.03),
          ("Deran", Set("Mouna", "Nate"))
        ),
        (
          HashMap("Mouna" -> 0.27, "Nate" -> 0.25, "Sue" -> 0.03),
          ("Deran", Set("Felix"))
        ),
        "Deran"
      ) == (Map("Sue" -> 0.03), ("Deran", Set("Mouna", "Nate", "Felix")))
    )
  }

  "Clusters construction" should "be done recursively and return the result in a Map" in {

    val entropy =
      HashMap("Deran" -> 0.44, "Mouna" -> 0.24, "Nate" -> 0.23, "Felix" -> 0.6)

    val m = HashMap(
      "Deran" -> HashMap("Mouna" -> 0.34, "Nate" -> 0.03),
      "Mouna" -> HashMap("Felix" -> 0.41)
    )

    assert(
      clustering(m, entropy, 0.25) == HashMap(
        "Nate" -> Set(),
        "Mouna" -> Set("Deran", "Felix")
      )
    )
  }
}
