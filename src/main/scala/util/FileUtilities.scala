package util

import cats.effect.{IO, Resource}
import io.chrisdavenport.log4cats.Logger

import scala.io.BufferedSource


/*
  Either this object name should be more generic or you should move all beta methods to a different file,
  since they have nothing to to with file utilities.
 */
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
      //The error you are handling here is when something goes wrong closing the file, not when reading it
      IO(in.close()).handleErrorWith(_ => logger.error("Error reading file"))
    }

  /** Checks if threshold satisfy the criteria */
  def compareBetaToRange(b: Float): Boolean =
    /*
      The program says "It should be < 0.9 and > 0.5" but 0.8 and 0.81 are not accepted.
      This 'if' returns true when the condition is true and false when it's false, so why not just return the condition it's self?
      like: def compareBetaToRange(b: Float): Boolean = b > 0.5 && b < 0.8
    */
    if (b > 0.5 && b < 0.8) true else false

}
