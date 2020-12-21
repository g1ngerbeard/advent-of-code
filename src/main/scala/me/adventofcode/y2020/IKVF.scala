package me.adventofcode.y2020

import cats.implicits._
import cats.Monad
import me.adventofcode.y2020.IKVF.AST._
case class DecodingError(reason: String)

// IKVF = imaginary key value format
object IKVF {

  // todo: use ValidatedNel
  type DecodingResult[T] = Either[DecodingError, T]

  def parse[T: Decoder](raw: String): DecodingResult[Seq[T]] =
    for {
      ast <- AST.build(raw)
      result <- Decoder[Seq[T]].decode(ast)
    } yield result

  object AST {

    sealed trait IKVFValue

    case class Field(name: String, value: String) extends IKVFValue

    case class IKVFObject(fields: Map[String, Field]) extends IKVFValue

    case class IKVFArray(objects: Seq[IKVFObject]) extends IKVFValue

    // todo: use parser combinators
    def build(raw: String): DecodingResult[IKVFValue] =
      raw
        .split("\n\n")
        .toVector
        .traverse(
          _.split("[ \n]").toVector
            .traverse {
              case s"$key:$value" => Right(Field(key, value))
              case invalid        => Left(DecodingError(s"Invalid field format: $invalid"))
            }
            .map { fields =>
              IKVFObject(fields.map(f => f.name -> f).toMap)
            }
        )
        .map(IKVFArray)

  }

  object Decoder {

    def apply[T: Decoder]: Decoder[T] = implicitly[Decoder[T]]

    implicit def sequenceDecoder[T: Decoder]: Decoder[Seq[T]] = {
      case IKVFArray(objects) => objects.traverse(Decoder[T].decode)
      case unexpected         => DecodingError(s"Expected IKVFArray got ${unexpected.getClass}").asLeft
    }

    def optionalFieldDecoder[T: Decoder](name: String): Decoder[Option[T]] = {
      case IKVFObject(fields) => fields.get(name).traverse(Decoder[T].decode)
      case unexpected         => DecodingError(s"Expected IKVFObject got ${unexpected.getClass}").asLeft
    }

    def fieldDecoder[T: Decoder](name: String): Decoder[T] = {
      case IKVFObject(fields) =>
        for {
          field <- fields.get(name).toRight(DecodingError(s"Required field $name is missing"))
          parsedValue <- Decoder[T].decode(field)
        } yield parsedValue
      case unexpected => DecodingError(s"Expected IKVFObject got ${unexpected.getClass}").asLeft
    }

    def fieldValueDecoder[T](p: String => Option[T]): Decoder[T] = {
      case Field(name, value) => p(value).toRight(DecodingError(s"Invalid value for the field $name: $value"))
      case unexpected         => DecodingError(s"Expected Field got ${unexpected.getClass}").asLeft
    }

  }

  trait Decoder[T] {

    def decode(value: IKVFValue): DecodingResult[T]

  }

}

object catsInstances {

  import IKVF.Decoder

  implicit val decoderMonad: Monad[Decoder] = new Monad[Decoder] {

    def flatMap[A, B](fa: Decoder[A])(f: A => Decoder[B]): Decoder[B] =
      (obj: IKVFValue) =>
        for {
          a <- fa.decode(obj)
          b <- f(a).decode(obj)
        } yield b

    // fixme: this is not tailrec ;(
    def tailRecM[A, B](a: A)(f: A => Decoder[Either[A, B]]): Decoder[B] = { obj =>
      f(a)
        .decode(obj)
        .flatMap {
          _.leftFlatMap(tailRecM(_)(f).decode(obj))
        }
    }

    def pure[A](x: A): Decoder[A] = _ => x.asRight

  }

}
