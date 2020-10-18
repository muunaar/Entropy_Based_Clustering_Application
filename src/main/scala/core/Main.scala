package core

import cats.effect.ExitCase.Canceled
import cats.effect.{ExitCode, IO, IOApp}

import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.Logger

object Main extends IOApp {

  import util.FileUtilities._
  import util.HelperForMergingAllCalculations._
  import util.Exceptions._

  def subCall(logger: Logger[IO]): IO[Unit] = {
    for {
      _ <- logger.info("Starting Entropy clustering Application")
      _ <- logger.info("User is asked to provide clustering threshold value") *>
        logger.info("It should be < 0.9 and > 0.5")

      beta <- readBeta
      _ <- logger.trace(s"Receiving the clustering threshold Beta = $beta")
      //The validation below could even be part of the readBeta method
      _ <- if (compareBetaToRange(beta)) {
        logger.info("Similarity threshold received successfully")
      } else {
        IO.raiseError(ThresholdNotInRange)
      }

      _ <- logger.info("Reading the file path")
      path <- getPath
      _ <- logger.trace(s"Receiving file path = $path")
      _ <- fileToClusters(path, beta)(logger)
      _ <- logger.info("Finished")
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      logger <- Slf4jLogger.create[IO]
      x <- subCall(logger)
        .handleErrorWith {
          case ThresholdNotInRange =>
            logger.error(ThresholdNotInRange.toString)
          case FileNotCorrectlyFormated =>
            logger.error(FileNotCorrectlyFormated.toString)
          case _ => logger.error("Unexpected Error has occured")
        }
        /*
          Not sure if that's by design, but you are returning success code even if the program fails (beta out of range or
          file does not exists for example)
         */
        .as(ExitCode.Success)
        .guaranteeCase {
          case Canceled =>
            logger
              .error("Program interrupted, releasing and exiting")
          case _ => logger.info("Normal Termination ...releasing and exiting")
        }
    } yield x
  }

}
