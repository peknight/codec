package com.peknight.codec.derivation

import cats.Applicative
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.codec.sum.ObjectType
import com.peknight.codec.{Encoder, Object}
import com.peknight.generic.Generic
import com.peknight.generic.tuple.syntax.forall

trait EncoderDerivation:
  def derived[F[_], S, O, A](using configuration: EncoderConfiguration)(using
    applicative: Applicative[F],
    objectType: ObjectType.Aux[S, O],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, S, A] =
    instances.derive(
      inst ?=> derivedProduct[F, S, O, A](configuration, objectType, inst),
      inst ?=> derivedSum[F, S, O, A](configuration, objectType, stringEncoder, inst)
    )

  private[this] def derivedProduct[F[_] : Applicative, S, O, A](
    configuration: EncoderConfiguration,
    objectType: ObjectType.Aux[S, O],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, S, A] =
    (a: A) => encodeProduct(a, configuration, objectType, instances)

  private[this] def derivedSum[F[_] : Applicative, S, O, A](
    configuration: EncoderConfiguration,
    objectType: ObjectType.Aux[S, O],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, S, A] =
    if instances.singletons.forall([X] => (x: X) => x.asInstanceOf[Option[Any]].isDefined) then
      (a: A) => EnumEncoderDerivation.encodeEnum(a, configuration, stringEncoder, instances.generic)
    else
      new SumEncoder[F, S, A]:
        def encode(a: A): F[S] = encodeSum(a, configuration, objectType, stringEncoder, instances)
  end derivedSum

  private[derivation] def encodeProduct[F[_] : Applicative, S, O, A](
    a: A,
    configuration: EncoderConfiguration,
    objectType: ObjectType.Aux[S, O],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): F[S] =
    instances.foldRightWithLabel(a)(List.empty[F[(String, S)]]) {
      [X] => (encoder: Encoder[F, S, X], x: X, label: String, acc: List[F[(String, S)]]) =>
        encoder.encode(x).map((configuration.transformMemberNames(label), _)) :: acc
    }.sequence.map(f => objectType.to(objectType.fromFoldable(f)))

  private[derivation] def encodeSum[F[_] : Applicative, S, O, A](
    a: A,
    configuration: EncoderConfiguration,
    objectType: ObjectType.Aux[S, O],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A]
  ): F[S] =
    val constructorName = configuration.transformConstructorNames(instances.label(a))
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
              objectType.add(obj, discriminator, encodedConstructorName))
            )
          case _ => objectType.to(objectType.singleton(constructorName, s))
    }
end EncoderDerivation
object EncoderDerivation extends EncoderDerivation
