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

  def fileToClusters(file: String,
                     beta: Double)(implicit logger: Logger[IO]): IO[Unit] = {
    openFile(file).use { source =>
      val parsedMap: List[ParseResult[ResourceActivities]] =
        source.getLines().toList.map(e => parse(mapResourceActivity, e))

      val validResourceActivity
        : List[ValidatedNec[String, ResourceActivities]] =
        validatingParseResult(parsedMap)

      val listValidatedResAc: ValidatedNec[String, List[ResourceActivities]] =
        validResourceActivity.sequence

      listValidatedResAc match {
        case Invalid(_) =>
          IO.raiseError(FileNotCorrectlyFormated)
        case Valid(list) =>
          val resourceActivityMap =
            list.foldLeft(HashMap.empty[String, List[String]]) { (acc, el) =>
              acc ++ HashMap(el.resource -> el.activities)
            }

          val similarityMatrix: Map[String, Map[String, Double]] =
            ConstructingSimilarityMap(resourceActivityMap)

          val entropyMatrix: Map[String, Double] =
            constructEntropyMap(similarityMatrix)
          IO(entropyMatrix)

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
