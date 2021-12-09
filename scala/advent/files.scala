package advent.files

import advent.io.IO
import advent.io.errors.IOError

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.InvalidPathException
import java.nio.file.Path
import java.nio.file.Paths

def readFile(file: String): IOError[IOException | InvalidPathException, String] =

  def readBytes(path: Path): IOError[IOException, Array[Byte]] =
    IOError.effect(Files.readAllBytes(path))

  def getPath(file: String): IOError[InvalidPathException, Path] =
    IOError.effect(Paths.get(file))

  for
    path <- getPath(file)
    bytes <- readBytes(path)
  yield
    String(bytes, StandardCharsets.UTF_8)
