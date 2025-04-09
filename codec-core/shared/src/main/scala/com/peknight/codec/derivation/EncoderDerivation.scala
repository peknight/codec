package com.peknight.codec.derivation

import cats.Applicative
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.functor.*
import com.peknight.codec.Encoder
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.sum.ObjectType
import com.peknight.generic.Generic
import com.peknight.generic.tuple.syntax.forall

trait EncoderDerivation:
  def derived[F[_], S, A](using configuration: EncoderConfiguration)(using
    applicative: Applicative[F],
    objectType: ObjectType[S],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, S, A] =
    instances.derive(
      inst ?=> derivedProduct[F, S, A](using configuration)(using applicative, objectType, inst),
      inst ?=> derivedSum[F, S, A](using configuration)(using applicative, objectType, stringEncoder, inst)
    )

  def derivedProduct[F[_], S, A](using configuration: EncoderConfiguration)(using
    applicative: Applicative[F],
    objectType: ObjectType[S],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, S, A] =
    (a: A) => encodeProduct(a, configuration, objectType, instances)

  def derivedSum[F[_], S, A](using configuration: EncoderConfiguration)(using
    applicative: Applicative[F],
    objectType: ObjectType[S],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, S, A] =
    if instances.singletons.forall([X] => (x: X) => x.asInstanceOf[Option[Any]].isDefined) then
      (a: A) => EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, instances.generic)
    else
      new SumEncoder[F, S, A]:
        def encode(a: A): F[S] = encodeSum(a, configuration, objectType, stringEncoder, instances)
  end derivedSum

  private[derivation] def encodeProduct[F[_]: Applicative, S, A](
    a: A,
    configuration: EncoderConfiguration,
    objectType: ObjectType[S],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): F[S] =
    instances.foldRightWithLabel(a)(List.empty[(String, S)].pure[F]) {
      [X] => (encoder: Encoder[F, S, X], x: X, label: String, acc: F[List[(String, S)]]) =>
        (encoder.encode(x), acc).mapN { (s, acc) =>
          if configuration.extField.contains(label) then
            objectType.asObject(s).fold(List.empty[(String, S)])(obj => objectType.toList(obj)) ::: acc
          else (configuration.transformMemberNames(label).head, s) :: acc
        }
    }.map(f => objectType.to(objectType.fromFoldable(f)))

  private[derivation] def encodeSum[F[_]: Applicative, S, A](
    a: A,
    configuration: EncoderConfiguration,
    objectType: ObjectType[S],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A]
  ): F[S] =
    val constructorName = configuration.transformConstructorNames(instances.label(a)).head
    val encoder = instances.instance(a)
    (
      encoder.encode(a),
      stringEncoder.encode(constructorName)
    ).mapN { (s, encodedConstructorName) =>
      val sum = encoder match
        case e: SumEncoder[F, S, ?] => true
        case _ =>
          configuration.discriminator.exists(discriminator => objectType.asObject(s).exists(obj =>
            objectType.contains(obj, discriminator)
          ))
      if sum then s
      else
        configuration.discriminator match
          case Some(discriminator) =>
            objectType.to(objectType.asObject(s).fold(objectType.singleton(discriminator, s))(obj =>
              objectType.prepended(obj, discriminator, encodedConstructorName))
            )
          case _ => objectType.to(objectType.singleton(constructorName, s))
    }
end EncoderDerivation
object EncoderDerivation extends EncoderDerivation
