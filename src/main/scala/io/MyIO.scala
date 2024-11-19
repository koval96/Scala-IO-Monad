package io

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/** Класс типов, позволяющий комбинировать описания вычислений, которые могут либо успешно
  * завершиться с некоторым значением, либо завершиться неуспешно, выбросив исключение Throwable.
  * @tparam F
  *   \- тип вычисления
  */
trait Computation[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
  def pure[A](a: A): F[A]
  def *>[A, B](fa: F[A])(another: F[B]): F[B]
  def as[A, B](fa: F[A])(newValue: => B): F[B]
  def void[A](fa: F[A]): F[Unit]
  def attempt[A](fa: F[A]): F[Either[Throwable, A]]
  def option[A](fa: F[A]): F[Option[A]]

  /** Если вычисление fa выбрасывает ошибку, то обрабатывает ее функцией f, без изменения типа
    * выходного значения.
    * @return
    *   результат вычисления fa или результат функции f
    */
  def handleErrorWith[A, AA >: A](fa: F[A])(f: Throwable => F[AA]): F[AA]

  /** Обрабатывает ошибку вычисления чистой функцией recover или преобразует результат вычисления
    * чистой функцией.
    * @return
    *   результат вычисления преобразованный функцией map или результат функции recover
    */
  def redeem[A, B](fa: F[A])(recover: Throwable => B, map: A => B): F[B]
  def redeemWith[A, B](fa: F[A])(recover: Throwable => F[B], bind: A => F[B]): F[B]

  /** Выполняет вычисление. "unsafe", потому что при неуспешном завершении может выбросить
    * исключение.
    * @param fa
    *   \- еще не начавшееся вычисление
    * @tparam A
    *   \- тип результата вычисления
    * @return
    *   результат вычисления, если оно завершится успешно.
    */
  def unsafeRunSync[A](fa: F[A]): A

  /** Оборачивает ошибку в контекст вычисления.
    * @param error
    *   \- ошибка
    * @tparam A
    *   \- тип результата вычисления. Т.к. вычисление сразу завершится ошибкой при выполнении, то
    *   может быть любым.
    * @return
    *   создает описание вычисления, которое сразу же завершается с поданной ошибкой.
    */
  def raiseError[A](error: Throwable): F[A]

}

object Computation {
  def apply[F[_]: Computation]: Computation[F] = implicitly[Computation[F]]
}

sealed trait MyIO[A] { self =>

  import io.MyIO._

  def map[B](f: A => B)(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.map(this)(f)

  def flatMap[B](f: A => MyIO[B])(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.flatMap(this)(f)

  def tailRecM[B](f: A => MyIO[Either[A, B]])(implicit
    comp: Computation[MyIO]
  ): MyIO[B] =
    comp.tailRecM(this.unsafeRunSync(comp))(f)

  def *>[B](another: MyIO[B])(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.*>(this)(another)

  def as[B](newValue: => B)(implicit
    comp: Computation[MyIO]
  ): MyIO[B] = comp.as(this)(newValue)

  def void(implicit
    comp: Computation[MyIO]
  ): MyIO[Unit] = comp.void(this)

  def attempt(implicit
    comp: Computation[MyIO]
  ): MyIO[Either[Throwable, A]] = comp.attempt(this)

  def option(implicit
    comp: Computation[MyIO]
  ): MyIO[Option[A]] = comp.option(this)

  def handleErrorWith[AA >: A](f: Throwable => MyIO[AA])(implicit
    comp: Computation[MyIO]
  ): MyIO[AA] =
    comp.handleErrorWith[A, AA](this)(f)

  def redeem[B](recover: Throwable => B, map: A => B)(implicit
    comp: Computation[MyIO]
  ): MyIO[B] =
    comp.redeem(this)(recover, map)

  def redeemWith[B](recover: Throwable => MyIO[B], bind: A => MyIO[B])(implicit
    comp: Computation[MyIO]
  ): MyIO[B] =
    comp.redeemWith(this)(recover, bind)

  def step: Either[() => MyIO[A], A] =
    self match {
      case Pure(v)         => Right(v)
      case Suspended(call) => Left(call)
      case Throwed(th)     => throw th
      case Recover(io, recover, convert) =>
        io match {
          case Throwed(th)     => Left(() => recover(th))
          case Pure(v)         => Left(() => convert(v))
          case Suspended(call) => Left(() => Recover(call(), recover, convert))
          case Recover(io2, recover2, convert2) =>
            Left(() =>
              Recover(
                io2,
                x => FlatMap(recover2(x), convert),
                (x: Any) => FlatMap(convert2(x), convert)
              )
            )
          case FlatMap(sub, cont) =>
            sub match {
              case Pure(v) => Left(() => Recover(cont(v), recover, convert))
              case Suspended(call) =>
                Left(() => Recover(FlatMap(call(), cont), recover, convert))
              case Throwed(th) =>
                Left(() => recover(th))
              case FlatMap(sub2, cont2) =>
                Left(() =>
                  Recover(
                    FlatMap(sub2, (x: Any) => FlatMap(cont2(x), cont)),
                    recover,
                    convert
                  )
                )
              case Recover(io2, recover2, convert2) =>
                Left(() =>
                  Recover(
                    io2,
                    x => FlatMap(FlatMap(recover2(x), cont), convert),
                    (x: Any) => FlatMap(FlatMap(convert2(x), cont), convert)
                  )
                )
            }

        }
      case FlatMap(sub, cont) =>
        sub match {
          case Pure(v)         => Left(() => cont(v))
          case Suspended(call) => Left(() => FlatMap(call(), cont))
          case Throwed(th)     => Left(() => Throwed(th))
          case Recover(io, recover, convert) =>
            Left(() =>
              Recover(io, x => FlatMap(recover(x), cont), (x: Any) => FlatMap(convert(x), cont))
            )
          case FlatMap(sub2, cont2) =>
            Left(() => FlatMap(sub2, (x: Any) => FlatMap(cont2(x), cont)): MyIO[A])
        }
    }

  def unsafeRunSync(implicit
    comp: Computation[MyIO]
  ): A = comp.unsafeRunSync(this)

}

object MyIO {

  private final case class Pure[A](a: A)                                   extends MyIO[A]
  private final case class Suspended[A](call: () => MyIO[A])               extends MyIO[A]
  private final case class FlatMap[A, B](sub: MyIO[A], cont: A => MyIO[B]) extends MyIO[B]
  private final case class Throwed[A](th: Throwable)                       extends MyIO[A]

  private final case class Recover[A, B](
    io: MyIO[A],
    recover: Throwable => MyIO[B],
    convert: A => MyIO[B]
  ) extends MyIO[B]

  implicit val computationInstanceForIO: Computation[MyIO] = new Computation[MyIO] {
    override def map[A, B](fa: MyIO[A])(f: A => B): MyIO[B]           = flatMap(fa)(v => pure(f(v)))
    override def flatMap[A, B](fa: MyIO[A])(f: A => MyIO[B]): MyIO[B] = FlatMap(fa, f)

    override def tailRecM[A, B](a: A)(f: A => MyIO[Either[A, B]]): MyIO[B] =
      FlatMap(
        f(a),
        (v: Either[A, B]) =>
          v match {
            case Left(a)  => tailRecM(a)(f)
            case Right(b) => pure(b)
          }
      )
    override def pure[A](a: A): MyIO[A]                           = MyIO(a)
    override def *>[A, B](fa: MyIO[A])(another: MyIO[B]): MyIO[B] = flatMap(fa)(_ => another)
    override def as[A, B](fa: MyIO[A])(newValue: => B): MyIO[B] =
      map(fa)(_ => newValue)
    override def void[A](fa: MyIO[A]): MyIO[Unit] = as(fa)(())
    override def attempt[A](fa: MyIO[A]): MyIO[Either[Throwable, A]] =
      redeemWith(fa)(th => pure(Left(th)), (v: A) => pure(Right(v)))

    override def option[A](fa: MyIO[A]): MyIO[Option[A]] = fa match {
      case Throwed(_) => pure(None)
      case io @ _     => map(io)(Some(_))
    }

    override def handleErrorWith[A, AA >: A](fa: MyIO[A])(f: Throwable => MyIO[AA]): MyIO[AA] =
      redeemWith(fa)(f, (aa: AA) => pure(aa))

    override def redeem[A, B](fa: MyIO[A])(recover: Throwable => B, map: A => B): MyIO[B] =
      redeemWith(fa)(th => pure(recover(th)), (v: A) => pure(map(v)))

    override def redeemWith[A, B](
      fa: MyIO[A]
    )(recover: Throwable => MyIO[B], bind: A => MyIO[B]): MyIO[B] = Recover(fa, recover, bind)

    @tailrec
    override def unsafeRunSync[A](fa: MyIO[A]): A = fa.step match {
      case Right(v)   => v
      case Left(more) => unsafeRunSync(more())
    }
    override def raiseError[A](error: Throwable): MyIO[A] = Throwed(error)
  }

  def apply[A](body: => A): MyIO[A]          = delay(body)
  def suspend[A](thunk: => MyIO[A]): MyIO[A] = Suspended(() => thunk)

  def delay[A](body: => A): MyIO[A] = suspend(Try(body) match {
    case Success(body) => pure(body)
    case Failure(th)   => Throwed(th)
  })

  def pure[A](a: A): MyIO[A] = Pure(a)

  def fromEither[A](e: Either[Throwable, A])(implicit
    comp: Computation[MyIO]
  ): MyIO[A] = e match {
    case Left(th) => comp.raiseError(th)
    case Right(a) => pure(a)
  }

  def fromOption[A](option: Option[A])(orElse: => Throwable)(implicit
    comp: Computation[MyIO]
  ): MyIO[A] = option match {
    case Some(a) => pure(a)
    case _       => comp.raiseError(orElse)
  }

  def fromTry[A](t: Try[A])(implicit
    comp: Computation[MyIO]
  ): MyIO[A] = t match {
    case Success(a)  => pure(a)
    case Failure(th) => comp.raiseError(th)
  }

  def none[A]: MyIO[Option[A]] = MyIO(None)

  def raiseUnless(cond: Boolean)(e: => Throwable)(implicit
    comp: Computation[MyIO]
  ): MyIO[Unit] =
    if (!cond) comp.raiseError(e) else unit

  def raiseWhen(cond: Boolean)(e: => Throwable)(implicit
    comp: Computation[MyIO]
  ): MyIO[Unit] =
    raiseUnless(!cond)(e)(comp)

  def unlessA(cond: Boolean)(action: => MyIO[Unit]): MyIO[Unit] = if (!cond) action else unit
  def whenA(cond: Boolean)(action: => MyIO[Unit]): MyIO[Unit]   = unlessA(!cond)(action)
  val unit: MyIO[Unit]                                          = pure(())

  def raiseError[A](error: Throwable)(implicit
    comp: Computation[MyIO]
  ): MyIO[A] = comp.raiseError(error)

}
