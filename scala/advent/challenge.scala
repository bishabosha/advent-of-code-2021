package advent.challenge

import advent.io.IO
import advent.io.errors.IOE
import advent.files.readFile
import advent.console.printLine

import java.io.IOException
import java.nio.file.InvalidPathException

def runChallenge[A, B](day: String, parser: String => Option[A])(part: String, challenge: A => B) =

  def fullName = s"$day-$part"

  def handleResult[E](result: Either[E, B]) =
    result match
      case Left(err) => printLine(s"[error] $fullName failed: ${err.toString}")
      case Right(res) => printLine(s"$fullName: $res")

  def parse(input: String) = parser(input).toRight(IllegalArgumentException(s"Could not parse input for $fullName"))

  def result =
    for
      input <- readFile(s"inputs/$day")
      parsed <- IOE.fromEither(parse(input))
    yield
      challenge(parsed)

  for
    res <- result.asIO
    _ <- handleResult(res)
  yield
    ()
