package advent.io

import scala.util.control.NonFatal
import scala.reflect.Typeable

object errors:
  opaque type IOE[+E, +A] = IO[Either[E, A]]

  object IOE:
    def apply[E, A](io: IO[Either[E, A]]): IOE[E, A] =
      io
    def fromEither[E, A](e: Either[E, A]): IOE[E, A] =
      IO.pure(e)

    extension [E, A](ioe: IOE[E, A])
      def map[B](f: A => B): IOE[E, B] =
        ioe.map(_.map(f))
      def flatMap[E1, B](f: A => IOE[E1, B]): IOE[E | E1, B] =
        ioe.flatMap(_.fold(err => IO.pure(Left(err)), f))
      def asIO: IO[Either[E, A]] =
        ioe

// TODO add error support
enum IO[+A]:

  case Pure[+A](a: A) extends IO[A]
  case Lazy[+A](fa: () => A) extends IO[A]
  case FlatMap[A, +B](mapped: IO[A], k: A => IO[B]) extends IO[B]

  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
  def map[B](f: A => B): IO[B] = FlatMap(this, f andThen (Pure(_)))

object IO:

  extension [E <: Throwable, A](io: IO[Either[E, A]])
    def refineError[E1 <: E: Typeable]: IO[Either[E1, A]] =
      io.flatMap {
        case Left(e: E1) => Pure(Left(e))
        case Left(err) => throw err
        case Right(a) => Pure(Right(a))
      }

  def effect[A](f: => A): IO[Either[Throwable, A]] = Lazy(() => try Right(f) catch { case err => Left(err) })
  def delay[A](f: => A): IO[A] = Lazy(() => f)
  def pure[A](a: A): IO[A] = Pure(a)

  def unsafeRunSync[A](io: IO[A]): A =

    def eraseK[A, B](k: A => IO[B]): Any => IO[Any] = k.asInstanceOf[Any => IO[Any]]
    def eraseIO[A](a: IO[A]): IO[Any] = a.asInstanceOf[IO[Any]]

    def loop(io: IO[Any], stack: List[Any => IO[Any]]): Any =
      io match
        case Pure(a) =>
          stack match
            case Nil       => a
            case k :: rest => loop(k(a), rest)
        case Lazy(fa) =>
          stack match
            case Nil       => fa()
            case k :: rest => loop(k(fa()), rest)
        case FlatMap(mapped0, k0) =>
          val k = eraseK(k0)
          val mapped = eraseIO(mapped0)
          loop(mapped, k :: stack)

    loop(io, Nil).asInstanceOf[A]
