/* Copyright (c) 2015 Andrée Ekroth.
 * Distributed under the MIT License (MIT).
 * See accompanying file LICENSE or copy at
 * http://opensource.org/licenses/MIT
 */

package com.github.ekroth

package object errorhandling {

  import scala.collection.generic.CanBuildFrom
  import scala.concurrent.Future

  import scalaz._
  import Scalaz._

  trait Error {
    def reason: String
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
     *  Useful for reducing many `Result`s into a single `Result`. Stops at the first failed
     *  `Result`.
     */
    def sequence[A, M[X] <: TraversableOnce[X]](in: M[Result[A]])(implicit cbf: CanBuildFrom[M[Result[A]], A, M[A]]): Result[M[A]] = {
      in.foldLeft(Result.ok(cbf(in))) {

        case (r @ -\/(_), _) => r

        case (\/-(y), a) => a match {
          case x @ -\/(_) => x
          case \/-(x) => (y += x).right
        }

      } map (_.result())
    }

  }
}
