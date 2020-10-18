/*
  This one could also be in the core package in my opinion
 */
package util

import scala.collection.immutable.HashMap

/** This object consists of constructing the similarity matrix. The steps are
  * separated according to six methods.
  *
  *  ------Details on Similarity Matrix construction --------------------------
  *  Soo, the calculation is done as follows : Taking the HashMap of resource
  *  with the activities they are involved in, for each resource, we proceed with
  *  constructing all the possible combinason of pairs with all the other
  *  resources. As a result we get the list of all possible tuples. After that we
  *  proceed with getting the associated list of tuples of resources based on the
  *  pair of activities constructed earlier. For calculating the jaccard similarity
  *  index, we first calculate the union as well as the intersection for each tuple
  *  of activities. We combine the result then in a collection of Tuple2, where the
  *  first element is a tuple of resources and the second element is the similarity
  *  index. The constructed collection is used to insert the data in the similarity
  *  matrix.
  *  For instance, from List(((Kate,Deran), 0.5), ((Mike, Deran), 0.4)),  refers to the similarity between
  *  Kate and Deran. They will be inserted in the similarity Matrix as follows :
  *  HashMap(Kate -> HashMap(Deran -> 0.5), Mike -> HashMap(Mike ->0.4) )
  */
object SimilarityCalculation {

  /** Takes hashMap of Resource and list of activities and list of activities
    * and return the associated key
    *
    * @param resourceActivities : Resource/Activity Map
    * @param listofActivities :  List of activities
    * @return Associated Resource for a  given resource
    */
  def getKey(resourceActivities: HashMap[String, List[String]],
             listofActivities: List[String]): String =
    resourceActivities
      .filter(_._2 == listofActivities)
      .keys
      .headOption
      .getOrElse("No resource is found")

  /** Takes the hashmap and returns all pair combinasons of possible values from
    *  the hashmap
    *  @param resourceListOfActivities : Map of Resources and their associated
    *                                  activities.
    *  @return List of list of pair combinasons of activities.
    */
  def getPairs(
    resourceListOfActivities: HashMap[String, List[String]]
  ): List[(List[String], List[String])] = {
    resourceListOfActivities.values.toList
      .combinations(2)
      .foldLeft(List.empty[(List[String], List[String])]) { (acc, el) =>
        acc ++ Vector(
          (
            el.headOption.getOrElse(List("Not Found")),
            el.lastOption.getOrElse(List("Not Found"))
          )
        )
      }
  }

  /**  Takes List of  List activities and calculates the union between the sets.
    *
    * @param tuple: Tuple of  list of activities performed.
    * @return the size of the union.
    */
  def calculateUnionLength(tuple: (List[String], List[String])): Int =
    tuple match {
      case (a, b) => a.concat(b).length
    }

  /**  Returns intersection length : number of elements belonging to the
    *  intersection.
    *
    *  @param tuple: tuple of List of activities performed.
    *  @return the size of the intersection.
    */
  def calculateIntersectionLength(tuple: (List[String], List[String])): Int =
    tuple match {
      case (a, b) => a.intersect(b).length
    }

  /**  Calculating jaccard similarity index using the union and intersection
    *
    * @param  unionSize : the size of the union between two resources.
    * @param intersectionSize : the size of the intersection between two resources.
    */
  def calculateJaccardSimilarity(unionSize: Double,
                                 intersectionSize: Int): Double =
    unionSize match {
      case 0 => 0d
      case _ => intersectionSize / unionSize
    }

  /** Construct the similarity Map
    *
    * @param resources : resources performing activities.
    * @param similarities : Jaccard similarity indexes.
    * @return Map of Resources and associated similar resources with the
    *         similarity index.
    */
  def constructElementsOfSimilarityMap(
    resources: List[(String, String)],
    similarities: List[Double]
  ): Map[String, Map[String, Double]] = {

    val combinedList = resources.zip(similarities)

    combinedList.foldLeft(Map.empty[String, Map[String, Double]]) { (acc, e) =>
      {
        acc + checkAndAddToMap(acc)(e)
      }
    }
  }

  /** Before adding an element to similarity Map, a check should be performed
    * in order to determine how the element to insert in similarity map is
    * constructed.
    *
    * @param m : similarity Map
    * @param element : a tuple of list of resources and the associated similar-
    *                ity index between them
    * @return Entry of similarity matrix
    */
  def checkAndAddToMap(
    m: Map[String, Map[String, Double]]
  )(element: ((String, String), Double)): (String, Map[String, Double]) = {

    val resourceFstKey: String =
      element._1._1
    val resourceScdKey: String =
      element._1._2
    val indexJaccard = element._2
    if (m.contains(resourceFstKey)) {
      resourceFstKey -> m(resourceFstKey).++(
        HashMap(resourceScdKey -> indexJaccard)
      )
    } else {
      resourceFstKey -> HashMap(resourceScdKey -> indexJaccard)
    }
  }

  /** Calculating Jaccard similarity index.
    * Takes list of intersection and union values of all possible combinasons
    * and constructs a collection of  jaccard similarity indexes
    *
    * @param intersection : List of intersection values
    * @param union : List of intersection values
    */
  def similarityIndexes(intersection: List[Int],
                        union: List[Int]): List[Double] =
    for ((i, u) <- intersection zip union)
      yield calculateJaccardSimilarity(u, i)

  /**  Helper function calling steps for constructing similarity Map.
    *
    * @param resourceActivityMap : Map of resources and the list of the perfor-
    *                            med activities
    * @return Similarity Matrix
    */
  def ConstructingSimilarityMap(
    resourceActivityMap: HashMap[String, List[String]]
  ): Map[String, Map[String, Double]] = {

    //Rename variable 'a' to 'pairs' maybe?
    val a = getPairs(resourceActivityMap)

    val keys = a.foldLeft(List.empty[(String, String)]) { (acc, el) =>
      acc ++ List(
        (getKey(resourceActivityMap, el._1), getKey(resourceActivityMap, el._2))
      )
    }

    val union = a.map(e => calculateUnionLength(e))

    val intersection = a.map(e => calculateIntersectionLength(e))

    val jaccardIndexes = similarityIndexes(intersection, union)

    constructElementsOfSimilarityMap(keys, jaccardIndexes)
  }
}
