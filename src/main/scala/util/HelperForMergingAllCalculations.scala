package util

import cats.data.Validated.{Invalid, Valid}
import cats.effect.IO
import cats.implicits._
import cats.data.ValidatedNec
import io.chrisdavenport.log4cats.Logger

import scala.collection.immutable.HashMap

object HelperForMergingAllCalculations {

  import DataParser._
  import Exceptions._
  import EntropyCalculation._
  import FileUtilities._
  import SimilarityCalculation._
  import Clustering._

  def fileToClusters(file: String,
                     beta: Double)(implicit logger: Logger[IO]): IO[Unit] = {
    openFile(file).use { source =>
      val parsingResourceActivitiesHashmap
        : List[ParseResult[ResourceActivities]] =
        source.getLines().toList.map(e => parse(mapResourceActivity, e))

      val validResourceActivity
        : List[ValidatedNec[String, ResourceActivities]] =
        validatingParseResult(parsingResourceActivitiesHashmap)

      val traverseMapDataToOption
        : ValidatedNec[String, List[ResourceActivities]] =
        validResourceActivity.sequence

      traverseMapDataToOption match {
        case Invalid(_) =>
          IO.raiseError(FileNotCorrectlyFormated)
        case Valid(list) =>
          val resourceActivityMap =
            list.foldLeft(HashMap.empty[String, List[String]]) { (acc, el) =>
              acc ++ HashMap(el.resource -> el.activities)
            }

          val jaccardSimilarityIndexMap: Map[String, Map[String, Double]] =
            ConstructingSimilarityMap(resourceActivityMap)

          val entropyMatrix: Map[String, Double] =
            constructEntropyMap(jaccardSimilarityIndexMap)

          val clustersOfResources =
            clustering(jaccardSimilarityIndexMap, entropyMatrix, beta)

          displayClusters(clustersOfResources, logger)
      }
    }
  }

  /** Mapping of ParseResult Type to Validated.
    *
    * @param  listResult : List of parsed HashMaps of Resource/Activities
    *                           constructed from each row of the file.
    * @return returns List of Validated of Resource/Activities HashMaps
    */
  def validatingParseResult(
    listResult: List[ParseResult[ResourceActivities]]
  ): List[ValidatedNec[String, ResourceActivities]] =
    listResult map {
      case Success(matched, _) => matched.validNec
      case _                   => FileNotCorrectlyFormated.toString.invalidNec
    }

}
