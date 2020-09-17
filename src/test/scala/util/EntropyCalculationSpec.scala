package util

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

class EntropyCalculationSpec extends FlatSpec with Matchers {

  "Check if a resource exists in entropy Map" should "return true " in {

    val entropyMap = HashMap("Haley" -> 0.2, "Mia" -> 0.3)
    assert(valueInEntropyMap("Haley", entropyMap) == true)
  }

  "The entropy value of a resource positioned as Key in similarity Matrix" should "use all the similarity indexes of all similar resources " in {

    val similarityMapElement =
      "Haley" -> HashMap("Deran" -> 0.25, "Mike" -> 0.2)
    assert(caseKey(similarityMapElement) == 0.18844804635220233)
  }

  "Entropy Calculation" should "Verify the given formula " in {

    assert(indexToEntropy(0.25) == 0.1724398555538391)
  }

  "In case of null entropy value, It" should "return zero" in {

    assert(indexToEntropy(0.0) == 0.0)
  }

  "If a resource already exists in similarity map, It" should "return a Map" in {

    val entropyMap = HashMap("X" -> 0.18844804635220233)
    val similarityElement = "Haley" -> Map("Deran" -> 0.25, "Mike" -> 0.2)
    assert(
      checkAndAdd(similarityElement, entropyMap) == HashMap(
        "Haley" -> 0.18844804635220233,
        "Deran" -> 0.1724398555538391,
        "Mike" -> 0.15078895382896995
      )
    )
  }

  "Entropy Map" should "contain the resources with their associated entropy value" in {

    val similarityMap = HashMap(
      "Haley" -> Map("Deran" -> 0.25, "Mike" -> 0.2),
      "Mike" -> Map("Deran" -> 0.0)
    )
    assert(
      constructEntropyMap(similarityMap) == HashMap(
        "Haley" -> 0.18844804635220233,
        "Deran" -> 0.1724398555538391,
        "Mike" -> 0.15078895382896995
      )
    )
  }
}
