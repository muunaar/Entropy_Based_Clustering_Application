package util

import cats.effect.{IO, Resource}
import io.chrisdavenport.log4cats.Logger

import scala.io.BufferedSource

/** Englobes all methods related to reading the data file, opening it
  * as well as the similarity Threashold used later in clustering
  *
  */
object FileUtilities {

  /** Asks the user to enter the file path */
  def getPath: IO[String] = IO { scala.io.StdIn.readLine() }

  /** Asks the user to enter the similarity threshold beta */
  def readBeta: IO[Float] = IO { scala.io.StdIn.readFloat() }

  /** Display in the console beta value provided */
  def printBeta(beta: Float)(implicit logger: Logger[IO]): IO[Unit] =
    logger.info(s"Threshold provided is : $beta ")

  /** Effectfully allocates the resource for opening the file
    * and release it once done
    * @param file the path of the file
    */
  def openFile(
    file: String
  )(implicit logger: Logger[IO]): Resource[IO, BufferedSource] =
    Resource.make {
      IO(scala.io.Source.fromFile(file))
    } { in =>
      IO(in.close()).handleErrorWith(_ => logger.error("Error reading file"))
    }

  /** Checks if threshold satisfy the criteria */
  def compareBetaToRange(b: Float): Boolean =
    if (b > 0.5 && b < 0.8) true else false

}
