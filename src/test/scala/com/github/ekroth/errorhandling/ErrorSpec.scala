package com.github.ekroth
package errorhandling

import org.scalacheck._

object StringSpecification extends Properties("ErrorHandling") {
  import Prop.forAll
  import Gen._
  import Arbitrary.arbitrary

  case class Test(i: Int) extends Error {
    override def reason = i.toString
  }

  val genError = arbitrary[Int].map(Test.apply)
  implicit val arbError = Arbitrary[Error](genError)

  val genRok = arbitrary[Int].map(Result.ok)
  val genRfail = arbitrary[Error].map(Result.fail)
  val genR = oneOf(genRok, genRfail)
  implicit val arbR = Arbitrary(genR)

  property("sequence") = forAll(arbitrary[List[Result[Int]]]) { is =>
    val rs = is.flatMap(_.toList)
    val es = is.flatMap(_.swap.toList)
    val xs = Result.sequence(is)

    if (es.nonEmpty)
      xs == Result.fail(Error.Collection(es))
    else
      xs == Result.ok(rs)
  }
}
