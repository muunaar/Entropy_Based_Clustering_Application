package util

import cats.effect.IO
import cats.implicits._
import io.chrisdavenport.log4cats.Logger

import scala.collection.immutable.HashMap

/** This object aims to cluster resources according  to entropy measure.
  * Based on Entropy measure of each resource, we consider the resource with
  * the minimum entropy, we retrieve resources having similarity with it
  * greater than beta and that constitute a cluster.
  */
object Clustering {

  /** Checks whether the Jaccard index value is greater than beta.
    *
    * @param index : Jaccard index for some resource
    * @param beta  similarity threshold
    * @return true if value is greater than beta, false otherwise
    */
  def compareSimilarityIndexToBeta(index: Double, beta: Double): Boolean =
    index >= beta match {
      case true  => true
      case false => false
    }

  /** Get the minimum entropy value in the collection and retrieves the
    * associated resource name.
    *
    * @param entropyMap : the entropy map storing resources and their associated
    *                   entropy value
    * @return the resource having the minimum entropy value.
    */
  def minEntropyResource(entropyMap: Map[String, Double]): String =
    entropyMap.minBy(_._2)._1

  /** Checks if a resource exists in the entropy Map
    *
    * @param resource : name of a resource that we are looking for in entropy Map
    * @return true if the resource is found.
    */
  def valueInEntropy(resource: String, entropy: Map[String, Double]): Boolean =
    entropy.keysIterator.contains(resource)

  /** Removes  the element passed as parameter from the entropy Map
    *
    * @param entropy  : Entropy Map.
    * @param resource : Key of element we want to remove.
    * @return A new Entropy Map without the removed element.
    */
  def removeResourceFromEntropy(entropy: Map[String, Double],
                                resource: String): HashMap[String, Double] = {
    entropy.foldLeft(HashMap.empty[String, Double]) { (acc, el) =>
      if (el._1 != resource) {
        acc + el
      } else acc
    }
  }

  /** Checks of a resource is positioned as a key in similarity map.
    *
    * @param resource      : resource name
    * @param similarityMap : similarity collection containing a resource and a
    *                      collection of resources having similarity with it and
    *                      the jaccard similarity value
    * @return true if a resource is a key in s, false otherwise.
    */
  def checkIfKey(resource: String,
                 similarityMap: Map[String, Map[String, Double]]): Boolean =
    similarityMap.keysIterator.contains(resource)

  /** Checks of a resource is belonging to the keys in the  map value in
    * similarity map.
    *
    * @param resource      : resource name
    * @param similarityMap : similarity collection containing a resource and a
    *                      collection of resources having similarity with it and
    *                      the jaccard similarity value
    * @return true if a resource is a key in the value map in similarity map,
    *         false otherwise.
    */
  def checkIfValue(resource: String,
                   similarityMap: Map[String, Map[String, Double]]): Boolean = {
    similarityMap.valuesIterator.exists(
      _.keysIterator.exists(_.contains(resource))
    )
  }

  /** Checks if one of the Maps is empty and uses the one containing elements
    *
    * @param entropy1 : An entropy Map
    * @param entropy2 An entropy Map
    * @return the Map which is not empty
    */
  def emptyEntropyCheck(entropy1: Map[String, Double],
                        entropy2: Map[String, Double]): Map[String, Double] = {
    if (entropy1.isEmpty) entropy2 else entropy1
  }

  /** Get the map of elements belonging to the same cluster. When The  resource
    *  key given belongs to the values of similarity map.
    *
    * @param resource resource name
    * @param similarityMap similarity map
    * @param entropy entropy map
    * @return Element of clusters which is represented with a tuple of the key
    *         given having as a value elements belonging to that cluster stored
    *         in a set.
    */
  def caseValue(similarityMap: Map[String, Map[String, Double]],
                entropy: Map[String, Double],
                resource: String,
                beta: Double): (Map[String, Double], (String, Set[String])) = {
    val r = similarityMap.filter(x => x._2.keysIterator.contains(resource))

    val elm = r.foldLeft((Map.empty[String, Double], Set.empty[String])) {
      (acc, el) =>
        {
          val ent = emptyEntropyCheck(acc._1, entropy)
          if (valueInEntropy(el._1, ent) && compareSimilarityIndexToBeta(
                el._2(resource),
                beta
              )) {
            val entropyNew = removeResourceFromEntropy(ent, el._1)
            (entropyNew, acc._2 + el._1)
          } else {
            (ent, acc._2)
          }
        }
    }
    (elm._1, (resource, elm._2))
  }

  /** Get the map of elements belonging to the same cluster as the key. When The resource
    *  key given belongs to the keys of similarity map.
    *
    * @param similarityMap: similarity map
    * @param entropy:  entropy map
    * @param resource: resource name
    * @param beta : Similarity threshold
    * @return An element of clusters : tuple of the key given having as a value elements belonging to
    *         that cluster stored in a set.
    */
  def caseKey(similarityMap: Map[String, Map[String, Double]],
              entropy: Map[String, Double],
              resource: String,
              beta: Double): (Map[String, Double], (String, Set[String])) = {
    val elements = similarityMap(resource).foldLeft(
      (Map.empty[String, Double], Set.empty[String])
    ) { (acc, el) =>
      {
        val ent = emptyEntropyCheck(acc._1, entropy)
        if (valueInEntropy(el._1, ent) && compareSimilarityIndexToBeta(
              el._2,
              beta
            )) {
          val entropyNew = removeResourceFromEntropy(ent, el._1)

          (entropyNew, acc._2 + el._1)

        } else {
          (ent, acc._2)
        }
      }
    }
    (elements._1, (resource, elements._2))
  }

  /** Takes two tuples, the first is the cluster elements constructed when the resource
    * is positioned as a key, and the second as a value as well as the related entropy
    * Map. It merges both and returns a single tuple with the entropy and the cluster for
    * this particular resource.
    *
    * @param t1 : entropy map and cluster elements as when resource is Key.
    * @param t2 : entropy map and cluster elements as when resource is Value.
    * @param resource : the key individual.
    * @return : a tuple which the final entropy as well as the cluster elements
    *            for this particular resource.
    */
  def mergeResults(
    t1: (Map[String, Double], (String, Set[String])),
    t2: (Map[String, Double], (String, Set[String])),
    resource: String
  ): (Map[String, Double], (String, Set[String])) = {
    val entropy = findElementsLeft(t1._1, t2._1)
    val concatSets = t1._2._2 concat t2._2._2
    val clusters = (resource, concatSets)
    (entropy, clusters)
  }

  def findElementsLeft(e1: Map[String, Double],
                       e2: Map[String, Double]): Map[String, Double] = {
    e2.keySet.filter(e1.keySet).foldLeft(Map.empty[String, Double]) {
      (acc, el) =>
        acc + (el -> e1(el))
    }
  }

  /** For a given resource, it returns the entropy as well as the cluster
    * elements belonging to it for each the case when the resource is key, or
    * a value or both.
    *
    * @param s : similarity Map
    * @param entropy : comprises resources with  their related entropy value.
    * @param resource : an individual performing a task
    * @param beta : similarity threshold
    * @return
    */
  def getElement(s: Map[String, Map[String, Double]],
                 entropy: Map[String, Double],
                 resource: String,
                 beta: Double): (Map[String, Double], (String, Set[String])) = {

    (checkIfKey(resource, s), (checkIfValue(resource, s))) match {

      case (true, true) => {
        mergeResults(
          caseKey(s, entropy, resource, beta),
          caseValue(s, entropy, resource, beta),
          resource
        )
      }

      case (true, false) => caseKey(s, entropy, resource, beta)
      case (false, true) => caseValue(s, entropy, resource, beta)
    }
  }

  /** Serves for constructing clusters based on the similarity, and entropy maps.
    *
    * @param s : Similarity map.
    * @param entropy : Entropy map.
    * @return : Clusters constructed.
    */
  def clustering(s: Map[String, Map[String, Double]],
                 entropy: Map[String, Double],
                 beta: Double): Map[String, Set[String]] = {

    @scala.annotation.tailrec
    def recClusters(
      entropy: Map[String, Double],
      clusters: Map[String, Set[String]]
    ): Map[String, Set[String]] = {
      if (entropy.isEmpty) {
        clusters
      } else {
        val minimum = minEntropyResource(entropy)
        val res = getElement(
          s,
          removeResourceFromEntropy(entropy, minimum),
          minimum,
          beta
        )
        recClusters(res._1, clusters + res._2)
      }
    }

    recClusters(entropy, Map.empty[String, Set[String]])
  }

  /** Displaying the constructed clusters.
    *
    * @param clusters : Clusters of resources performing similar tasks
    */
  def displayClusters(clusters: Map[String, Set[String]],
                      logger: Logger[IO]): IO[Unit] =
    logger.info("Clusters constructed" + clusters.toString())
}
