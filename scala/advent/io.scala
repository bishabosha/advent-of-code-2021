package advent.io

import scala.util.control.NonFatal
import scala.reflect.Typeable

import advent.challenge.tryEither

object errors:
  opaque type IOError[+E, +A] = IO[Either[E, A]]

  object IOError:

    def fromEither[E, A](e: Either[E, A]): IOError[E, A] =
      IO.pure(e)

    def effect[E <: Throwable: Typeable, A](f: => A): IOError[E, A] =
      IO.delay(tryEither(f))

    extension [E, A](ioe: IOError[E, A])
      def mapError[E1](f: E => E1): IOError[E1, A] =
        ioe.map(_.left.map(f))
      def map[B](f: A => B): IOError[E, B] =
        ioe.map(_.map(f))
      def flatMap[E1, B](f: A => IOError[E1, B]): IOError[E | E1, B] =
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
  def *>[B](io: IO[B]): IO[B] = flatMap(_ => io)

object IO:

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
        case FlatMap(mapped, k) =>
          val k0 = eraseK(k)
          val mapped0 = eraseIO(mapped)
          loop(mapped0, k0 :: stack)

    loop(io, Nil).asInstanceOf[A]
