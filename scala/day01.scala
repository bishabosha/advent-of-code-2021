import advent.*
import advent.io.IO

@main def day01 =
  for
    x <- IO.Pure(23)
    y <- IO.Pure(42)
  yield
    (x, y)
  // println(IO.unsafeRunSync(res))
