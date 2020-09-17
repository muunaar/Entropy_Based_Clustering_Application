package core

import cats.effect.ExitCase.Canceled
import cats.effect.{ExitCode, IO, IOApp}

import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.Logger

object Main extends IOApp {

  def subCall(logger: Logger[IO]): IO[Unit] = {
    for {
      _ <- logger.info("Starting Entropy clustering Application")
      _ <- logger.info("User is asked to provide clustering threshold value") *>
        logger.info("It should be < 0.9 and > 0.5")

      _ <- logger.trace(s"Receiving the clustering threshold Beta")

      _ <- logger.info("Reading the file path")
      _ <- logger.trace(s"Receiving file path ")
      _ <- logger.info("Finished")
    } yield ()
  }

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      logger <- Slf4jLogger.create[IO]
      x <- subCall(logger)
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
