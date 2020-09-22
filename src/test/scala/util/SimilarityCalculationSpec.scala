package util

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

class SimilarityCalculationSpec extends FlatSpec with Matchers {
  import SimilarityCalculation._
  "Resource performing the given list of activities" should "be returned" in {

    assert(
      getKey(HashMap("Deran" -> List("Eat", "Drink")), List("Eat", "Drink")) == Some(
        "Deran"
      )
    )
  }

  "When the list of activities is not performed by the resource" should "return None" in {

    assert(
      getKey(HashMap("Deran" -> List("Eat", "Drink")), List("Dance", "Walk")) == None
    )
  }

  "Pairs of all possible set of activities performed by the resources" should "be returned in a List" in {

    assert(
      getPairs(
        HashMap(
          "Haley" -> List("Eat", "Sleep"),
          "Deran" -> List("Breathe", "Walk"),
          "Sue" -> List("Calls", "Receives")
        )
      ) == List(
        List(List("Calls", "Receives"), List("Eat", "Sleep")),
        List(List("Calls", "Receives"), List("Breathe", "Walk")),
        List(List("Eat", "Sleep"), List("Breathe", "Walk"))
      )
    )
  }

  "The union length of two sets" should "return the number of all elements of the sets" in {

    assert(
      calculateUnionLength((List("Eat", "Breathe"), List("Breathe", "Walk"))) == 4
    )
  }

  "The intersection length of two sets" should "return the number of all elements that exist in both sets" in {

    assert(
      calculateIntersectionLength(
        (List("Eat", "Breathe"), List("Breathe", "Walk"))
      ) == 1
    )
  }

  "In case of Empty union set, jaccard similarity index" should "be 0" in {

    assert(calculateJaccardSimilarity(0, 3) == 0)
  }

  "In case of Non empty union set, jaccard similarity index" should "be calculated according to the formula" in {

    assert(calculateJaccardSimilarity(3, 1) == 0.3333333333333333)
  }

  "Calculating jaccard similarity index between two sets" should "use both the intersection and the union" in {

    assert(
      similarityIndexes(List(1, 2, 1), List(4, 2, 6)) === List(
        0.25,
        1.0,
        0.16666666666666666
      )
    )
  }

  "Similarity Map" should "contains the resources and their related similar resources as well as the jaccard similarity index" in {

    val resourceActivity = HashMap(
      "Haley" -> List("Eat", "Sleep"),
      "Deran" -> List("Breathe", "Eat"),
      "Sue" -> List("Eat", "Receives")
    )
    assert(
      ConstructingSimilarityMap(resourceActivity) == HashMap(
        "Sue" -> HashMap("Haley" -> 0.25, "Deran" -> 0.25),
        "Haley" -> HashMap("Deran" -> 0.25)
      )
    )
  }

  "Taking the list of resources and the list of similarity indexes" should "return the similarity Map" in {

    val resourcesList =
      List(("Haley", "Deran"), ("Sue", "John"))
    val similarityIndexes = List(0.4, 0.45)

    assert(
      constructElementsOfSimilarityMap(resourcesList, similarityIndexes) == HashMap(
        "Haley" -> HashMap("Deran" -> 0.4),
        "Sue" -> HashMap("John" -> 0.45)
      )
    )
  }

  "Checking if a resource exists in similarity map" should "be done before adding it" in {

    val simMap = HashMap("Haley" -> HashMap("Deran" -> 0.4))
    val recordElement = (("Mike", "Peter"), 0.4)
    assert(
      checkAndAddToMap(simMap)(recordElement) == ("Mike" -> HashMap(
        "Peter" -> 0.4
      ))
    )
  }

}
