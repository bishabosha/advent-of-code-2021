package advent.challenge

import advent.io.IO
import advent.io.errors.IOError
import advent.files.readFile
import advent.console.printLine

import scala.util.control.NonLocalReturns.*

import scala.reflect.Typeable

import java.io.IOException
import java.nio.file.InvalidPathException

trait Show[-T]:
  extension (t: T) def show: String

object Show:
  def apply[A: Show] = summon[Show[A]]

  given Show[Exception] = _.toString
  given Show[Int] = _.toString

end Show

def readLinesAsInt(str: String): Either[NumberFormatException, List[Int]] =
  tryEither(str.split("\n").toList.map(_.toInt))

def tryEither[E <: Throwable: Typeable, A](f: => A): Either[E, A] =
  try Right(f) catch { case err: E => Left(err) }

class ParseError(msg: String) extends Exception(msg)

val runChallenge =
  [A, E] => (day: String, parse: String => Either[E, A]) =>
  [B] => (part: String, challenge: A => B) =>
  (sb: Show[B], se: Show[E]) ?=>

    val fullName = s"$day-$part"

    def printError[E](using Show[E])(err: E) = printLine(s"[error] $fullName failed: ${err.show}")
    def success(result: B) = printLine(s"$fullName: ${result.show}")

    def doParse(input: String): IOError[ParseError, A] =
      IOError
        .fromEither(parse(input))
        .mapError(err => ParseError(s"Could not parse input due to ${err.show}"))

    def result =
      for
        input <- readFile(s"inputs/$day")
        parsed <- doParse(input)
      yield
        challenge(parsed)

    for
      res <- result.asIO
      _   <- res.fold(printError, success)
    yield
      ()

end runChallenge

