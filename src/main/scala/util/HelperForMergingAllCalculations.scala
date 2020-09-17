package util

import cats.effect.IO
import cats.implicits._
import cats.data.ValidatedNec
import io.chrisdavenport.log4cats.Logger

object HelperForMergingAllCalculations {

  import DataParser._
  import FileUtilities._
  import Exceptions._

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
      IO(listValidatedResAc)

    }
  }

  /** Mapping of ParseResult Type to Option.
    *
    * @param  listResult : List of parsed HashMaps of Resource/Activities
    *                           constructed from each row of the file.
    * @return returns List of Option of Resource/Activities HashMaps*/
  def validatingParseResult(
    listResult: List[ParseResult[ResourceActivities]]
  ): List[ValidatedNec[String, ResourceActivities]] =
    listResult map {
      case Success(matched, _) => matched.validNec
      case _                   => FileNotCorrectlyFormated.toString.invalidNec
    }

}
