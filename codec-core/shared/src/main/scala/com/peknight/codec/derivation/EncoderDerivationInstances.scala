package com.peknight.codec.derivation

import cats.Applicative
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import com.peknight.codec.Encoder
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.generic.Generic

trait EncoderDerivationInstances:
  def derived[F[_], S, A](using configuration: EncoderConfiguration)(using
    applicative: Applicative[F],
    encodeObjectOps: EncodeObjectOps[S],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, S, A] =
    instances.derive(
      inst ?=> derivedProduct[F, S, A](configuration, encodeObjectOps, inst),
      inst ?=> derivedSum[F, S, A](configuration, encodeObjectOps, stringEncoder, inst)
    )

  private[this] def derivedProduct[F[_] : Applicative, S, A](
    configuration: EncoderConfiguration,
    encodeObjectOps: EncodeObjectOps[S],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder[F, S, A] =
    (a: A) => encodeProduct(a, configuration, encodeObjectOps, instances)

  private[this] def derivedSum[F[_] : Applicative, S, A](
    configuration: EncoderConfiguration,
    encodeObjectOps: EncodeObjectOps[S],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A]
  ): SumEncoder[F, S, A] =
    (a: A) => encodeSum(a, configuration, encodeObjectOps, stringEncoder, instances)

  private[derivation] def encodeProduct[F[_] : Applicative, S, A](
    a: A,
    configuration: EncoderConfiguration,
    encodeObjectOps: EncodeObjectOps[S],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): F[S] =
    instances.foldRightWithLabel(a)(List.empty[F[(String, S)]]) {
      [X] => (encoder: Encoder[F, S, X], x: X, label: String, acc: List[F[(String, S)]]) =>
        encoder.encode(x).map((configuration.transformMemberNames(label), _)) :: acc
    }.sequence.map(encodeObjectOps.fromFoldable)

  private[derivation] def encodeSum[F[_] : Applicative, S, A](
    a: A,
    configuration: EncoderConfiguration,
    encodeObjectOps: EncodeObjectOps[S],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A]
  ): F[S] =
    val constructorName = configuration.transformConstructorNames(instances.label(a))
    val encoder = instances.instance(a)
    (
      encoder.encode(a),
      stringEncoder.encode(constructorName)
    ).mapN { (b, encodedConstructorName) =>
      val sum = encoder match
        case e: SumEncoder[F, S, ?] => true
        case _ => configuration.discriminator.exists(discriminator => encodeObjectOps.contains(b, discriminator))
      if sum then b
      else
        configuration.discriminator match
          case Some(discriminator) => encodeObjectOps.prepended(b, (discriminator, encodedConstructorName))
          case _ => encodeObjectOps.singleton(constructorName, b)
    }

end EncoderDerivationInstances
object EncoderDerivationInstances extends EncoderDerivationInstances
