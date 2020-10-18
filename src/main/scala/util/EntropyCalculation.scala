/*
  This one could also be in the core package in my opinion
 */
package util

import scala.collection.immutable.HashMap
import scala.math._

/** This object consists of constructing the entropy matrix.
  * In our case, the entropy collection is a Map[String, Double]
  * It comprises the resources and the associated entropy measure.
  *
  * ------Details on Entropy calculation-------------------------
  *
  * The entropy measure value is calculating with the formula given in the
  * method indexToEntropy defined in line 38. Since we are working with
  * similarity matrix : HashMap[Resource, HashMap[Resource, SimilarityIndex]]
  * the value depends on the Resource's position : If it is located as a Key,
  * we sum all associated similarity indexes of resources having similarity with
  * it. The sum is used to calculate the related entropy measure.If it is located
  * as a key in the hashmap representing the values, we take the associated
  * similarity index and we use it to calculate the entropy measure.
  * The result is added to the Entropy Map.
  * Note that if the resource is already found in the  Entropy map at the moment
  * of insertion, the recent entropy measure becomes the sum of the existing and
  * the new calculated value.
  */
object EntropyCalculation {

  /** Checks if a resource exists in Entropy Map
    * @param resource : Resource name we are checking on
    * @param entropy : Entropy Map containing resources and their associated
    *                similarity index value.
    * @return true if a resource is found
    */
  def valueInEntropyMap(resource: String,
                        entropy: Map[String, Double]): Boolean =
    entropy.keys.exists(_.contains(resource))

  /** Takes a record in the similarity, and returns its associated entropy value.
    *
    * @param similarityMapElement : Similarity Matrix.
    * @return Entropy value for the key resource.
    */
  def caseKey(similarityMapElement: (String, Map[String, Double])): Double =
    indexToEntropy(similarityMapElement._2.values.sum)

  /** Takes a similarity index, and returns entropy value according to
    * the entropy measure calculation formula.
    *
    * @param index : similarity index
    * @return entropy value
    */
  def indexToEntropy(index: Double): Double =
    ((index * log(2) * index) + ((1 - index) * log(1 - index))).abs

  /** Takes a record of the similarity map, and returns a map of resources and
    * their related entropy value. Also, the calculation of entropy map depends
    * on whether the resource is the first element of the tuple, or belongs to
    * the second element of the tuple.
    *
    * @param element : A record in similarity Map, consisting of a resource and
    *                the related similar resources.
    * @param entropyMap : Entropy Map
    * @return Map of resources and their associated entropy value.
    */
  def checkAndAdd(element: (String, Map[String, Double]),
                  entropyMap: Map[String, Double]): Map[String, Double] = {
    val resource1 = element._1

    val asKeyResult = if (valueInEntropyMap(resource1, entropyMap)) {
      HashMap(resource1 -> (entropyMap(resource1) + caseKey(element)))
    } else {
      HashMap(resource1 -> caseKey(element))
    }

    val asValueResult = element._2.foldLeft(Map.empty[String, Double]) {
      (acc, el) =>
        {
          if (valueInEntropyMap(el._1, entropyMap)) {
            acc ++ HashMap(el._1 -> (entropyMap(el._1) + indexToEntropy(el._2)))
          } else {
            acc ++ HashMap(el._1 -> indexToEntropy(el._2))
          }
        }
    }
    asKeyResult ++ asValueResult
  }

  /** Iterates over the similarity map and construct the entropy value.
    *
    * @param similarityMap : Similarity Matrix.
    * @return Entropy Map composed of resources as keys, and the calculated entropy values.
    */
  def constructEntropyMap(
    similarityMap: Map[String, Map[String, Double]]
  ): Map[String, Double] = {
    similarityMap
      .foldLeft(Map.empty[String, Double]) { (mapAcc, el) =>
        {
          mapAcc ++ checkAndAdd(el, mapAcc)
        }
      }
  }
}
