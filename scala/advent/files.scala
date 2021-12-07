package advent.files

import advent.io.IO
import advent.io.errors.IOE

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.InvalidPathException
import java.nio.file.Path
import java.nio.file.Paths

def readFile(file: String): IOE[IOException | InvalidPathException, String] =

  def readBytes(path: Path): IOE[IOException, Array[Byte]] =
    IOE(IO.effect(Files.readAllBytes(path)).refineError[IOException])

  def getPath(file: String): IOE[InvalidPathException, Path] =
    IOE(IO.effect(Paths.get(file)).refineError[InvalidPathException])

  for
    path <- getPath(file)
    bytes <- readBytes(path)
  yield
   new String(bytes, StandardCharsets.UTF_8)
