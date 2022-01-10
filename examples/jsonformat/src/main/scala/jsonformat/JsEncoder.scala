// Copyright: 2017 - 2022 Sam Halliday
// License: http://www.gnu.org/licenses/lgpl-3.0.en.html

package jsonformat

import scalaz._, Scalaz._

trait JsEncoder[A] {
  def toJson(obj: A): JsValue

  // for performance
  final def xmap[B](@unused f: A => B, g: B => A): JsEncoder[B] = contramap(g)
  final def contramap[B](g: B => A): JsEncoder[B]               = b => toJson(g(b))
}
object JsEncoder
    extends JsEncoderScalaz1
    with JsEncoderRefined
    with JsEncoderStdlib1
    with JsEncoderScalaz2
    with JsEncoderStdlib2     {

  implicit val contravariant: Contravariant[JsEncoder] =
    new Contravariant[JsEncoder] {
      def contramap[A, B](fa: JsEncoder[A])(f: B => A): JsEncoder[B] =
        b => fa.toJson(f(b))
    }

  implicit val jsValue: JsEncoder[JsValue] = identity
  implicit val long: JsEncoder[Long]       = JsInteger(_)
  implicit val double: JsEncoder[Double]   = JsDouble(_)
  implicit val boolean: JsEncoder[Boolean] = JsBoolean(_)
  implicit val string: JsEncoder[String]   = JsString(_)

  implicit val float: JsEncoder[Float]   = double.contramap(_.toDouble)
  implicit val int: JsEncoder[Int]       = long.contramap(_.toLong)
  implicit val short: JsEncoder[Short]   = long.contramap(_.toLong)
  implicit val byte: JsEncoder[Byte]     = long.contramap(_.toLong)
  implicit val unit: JsEncoder[Unit]     = long.contramap(_ => 1)
  implicit val char: JsEncoder[Char]     = string.contramap(_.toString)
  implicit val symbol: JsEncoder[Symbol] = string.contramap(_.name)

  def apply[A](implicit a: JsEncoder[A]): JsEncoder[A] = a
}

private[jsonformat] trait JsEncoderScalaz1 {
  this: JsEncoder.type =>

  implicit def ilist[A: JsEncoder]: JsEncoder[IList[A]] =
    as => JsArray(as.map(JsEncoder[A].toJson(_)))

  implicit def nel[A: JsEncoder]: JsEncoder[NonEmptyList[A]] =
    ilist[A].contramap(_.list)

  implicit def imap[A: JsEncoder]: JsEncoder[String ==>> A] = { m =>
    val fields = m.toList.map { case (k, v) =>
      k -> JsEncoder[A].toJson(v)
    }
    JsObject(fields.toIList)
  }

  implicit def maybe[A: JsEncoder]: JsEncoder[Maybe[A]]                   = {
    case Maybe.Just(a) => JsEncoder[A].toJson(a)
    case Maybe.Empty() => JsNull
  }
  implicit def disjunction[A: JsEncoder, B: JsEncoder]: JsEncoder[A \/ B] = {
    case -\/(a) => JsEncoder[A].toJson(a)
    case \/-(b) => JsEncoder[B].toJson(b)
  }

  implicit def tagged[A: JsEncoder, Z]: JsEncoder[A @@ Z] =
    JsEncoder[A].contramap(Tag.unwrap)
}
private[jsonformat] trait JsEncoderRefined {
  this: JsEncoder.type =>

  import eu.timepit.refined.api.Refined
  implicit def refined[A: JsEncoder, B]: JsEncoder[A Refined B] =
    JsEncoder[A].contramap(_.value)
}
private[jsonformat] trait JsEncoderStdlib1 {
  this: JsEncoder.type =>

  implicit def option[A: JsEncoder]: JsEncoder[Option[A]]                  =
    maybe[A].contramap(_.toMaybe)
  implicit def either[A: JsEncoder, B: JsEncoder]: JsEncoder[Either[A, B]] =
    disjunction[A, B].contramap(_.toDisjunction)

  implicit def list[A: JsEncoder]: JsEncoder[List[A]] =
    ilist[A].contramap(_.toIList)
  implicit def dict[A: JsEncoder]: JsEncoder[Map[String, A]] = { m =>
    val fields = m.toList.map { case (k, v) =>
      k -> JsEncoder[A].toJson(v)
    }
    JsObject(fields.toIList)
  }
}
private[jsonformat] trait JsEncoderScalaz2 {
  this: JsEncoder.type =>

  // WARNING: encoder instances over Foldable support things that are not
  // isomorphic to IList, e.g. Map[BadThing, String], which has a Foldable over
  // the values. Think hard before adding such instances.
  //
  //implicit def foldable[F[_]: Foldable, A: JsEncoder]: JsEncoder[F[A]] =
  //  ilist[A].contramap(_.toIList)

}
private[jsonformat] trait JsEncoderStdlib2 {
  this: JsEncoder.type =>

  implicit def iterable[T[a] <: Iterable[a], A: JsEncoder]: JsEncoder[T[A]] =
    list[A].contramap(_.toList)
}
