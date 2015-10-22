/* Copyright (c) 2015 Andrée Ekroth.
 * Distributed under the MIT License (MIT).
 * See accompanying file LICENSE or copy at
 * http://opensource.org/licenses/MIT
 */

package com.github.ekroth

package object errorhandling {

  import scala.collection.immutable.Seq
  import scala.collection.generic.CanBuildFrom
  import scala.concurrent.{ Future, ExecutionContext }

  import scalaz._
  import Scalaz._

  trait Error {
    def reason: String
  }

  object Error {
    final case class Collection(errors: Seq[Error]) extends Error {
      override def reason = errors.map(_.reason).mkString
    }
    final case class Thrown(e: Exception) extends Error {
      override def reason = e.toString
    }
    final case class Unknown(reason: String) extends Error
    final case class Usage(reason: String) extends Error
  }

  trait Errors {
    val Collection = Error.Collection
    val Thrown = Error.Thrown
    val Unknown = Error.Unknown
  }

  type Result[A] = Error \/ A
  type ResultF[A] = EitherT[Future, Error, A]

  object Result {

    /** Construct a failed `Result[A]`. */
    def fail[A](e: Error): Result[A] = e.left

    /** Construct an OK `Result[A]`. */
    def ok[A](x: A): Result[A] = x.right

    /** Construct a failed `ResultF[A]`. */
    def failF[A](e: Error): ResultF[A] = EitherT(Future.successful(e.left))

    /** Construct an OK `ResultF[A]`. */
    def okF[A](x: Future[Result[A]]): ResultF[A] = EitherT(x)

    /** Transforms a `TraversableOnce[Result[A]]` into a `Result[TraversableOnce[A]]`.
     *  Useful for reducing many `Result`s into a single `ResultF`. In case of error the
     *  `Result` will contain an `Error.Collection`, containing all errors.
     */
    def sequence[A, M[X] <: TraversableOnce[X]](in: M[Result[A]])(implicit cbf: CanBuildFrom[M[Result[A]], A, M[A]]): Result[M[A]] = {
      in.foldLeft(Result.ok(cbf(in))) {

        case (r @ -\/(Error.Collection(errors)), a) => a match {
          case -\/(e) => -\/(Error.Collection(errors :+ e))
          case _ => r
        }

        case (\/-(y), a) => a match {
          case -\/(x) => Error.Collection(Seq(x)).left
          case \/-(x) => (y += x).right
        }

        case (r @ -\/(_), _) => ???

      } map (_.result())
    }

    /** Transforms a `TraversableOnce[Future[Result[A]]]` into a `ResultF[TraversableOnce[A]]`.
     *  Useful for reducing many `Result`s into a single `Result`. See `sequence`.
     */
    def sequenceF[A, M[X] <: TraversableOnce[X]](in: M[Future[Result[A]]])(implicit
      cbff: CanBuildFrom[M[Future[Result[A]]], Result[A], M[Result[A]]],
      cbfr: CanBuildFrom[M[Result[A]], A, M[A]],
      exc: ExecutionContext): ResultF[M[A]] = EitherT {
      for {
        seq <- Future.sequence(in)
      } yield Result.sequence(seq)
    }

    /** Run all `ResultF` in a `TraversableOnce[ResultF[A]]`. See `sequenceF`. */
    def run[A, M[X] <: TraversableOnce[X]](in: M[ResultF[A]])(implicit
      cbff: CanBuildFrom[M[Future[Result[A]]], Result[A], M[Result[A]]],
      cbfr: CanBuildFrom[M[Result[A]], A, M[A]],
      cbfv: CanBuildFrom[M[ResultF[A]], Future[Result[A]], M[Future[Result[A]]]],
      exc: ExecutionContext): ResultF[M[A]] = {

      val c = cbfv(in)
      in.foreach(x => c += x.run)
      Result.sequenceF(c.result)
    }
  }
}
