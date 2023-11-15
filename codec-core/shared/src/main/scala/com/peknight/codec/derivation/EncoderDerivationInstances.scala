package com.peknight.codec.derivation

import cats.Applicative
import cats.syntax.apply.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import com.peknight.codec.{ObjectType, Encoder}
import com.peknight.codec.configuration.EncoderConfiguration
import com.peknight.generic.Generic

trait EncoderDerivationInstances:
  def derived[F[_], S, O, A](using configuration: EncoderConfiguration)(using
    applicative: Applicative[F],
    objectType: ObjectType.Aux[S, O],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder.AsObject.Aux[F, S, O, A] =
    if instances.isProduct then derivedProduct[F, S, O, A](configuration, objectType, instances.asInstanceOf)
    else derivedSum[F, S, O, A](configuration, objectType, stringEncoder, instances.asInstanceOf)

  private[this] def derivedProduct[F[_] : Applicative, S, O, A](
    configuration: EncoderConfiguration,
    objectType0: ObjectType.Aux[S, O],
    instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): Encoder.AsObject.Aux[F, S, O, A] =
    new Encoder.AsObject[F, S, A]:
      type Object = O
      protected def objectType: ObjectType.Aux[S, O] = objectType0
      def encodeObject(a: A): F[O] = encodeProduct(a, configuration, objectType0, instances)
  end derivedProduct

  private[this] def derivedSum[F[_] : Applicative, S, O, A](
    configuration: EncoderConfiguration,
    objectType0: ObjectType.Aux[S, O],
    stringEncoder: Encoder[F, S, String],
    instances: => Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A]
  ): SumEncoder.Aux[F, S, O, A] =
    new SumEncoder[F, S, A]:
      type Object = O
      protected def objectType: ObjectType.Aux[S, O] = objectType0
      def encodeObject(a: A): F[O] = encodeSum(a, configuration, objectType0, stringEncoder, instances)
  end derivedSum

  private[derivation] def encodeProduct[F[_] : Applicative, S, O, A](
                                                                      a: A,
                                                                      configuration: EncoderConfiguration,
                                                                      objectType: ObjectType.Aux[S, O],
                                                                      instances: => Generic.Product.Instances[[X] =>> Encoder[F, S, X], A]
  ): F[O] =
    instances.foldRightWithLabel(a)(List.empty[F[(String, S)]]) {
      [X] => (encoder: Encoder[F, S, X], x: X, label: String, acc: List[F[(String, S)]]) =>
        encoder.encode(x).map((configuration.transformMemberNames(label), _)) :: acc
    }.sequence.map(objectType.fromFoldable)

  private[derivation] def encodeSum[F[_] : Applicative, S, O, A](
                                                                  a: A,
                                                                  configuration: EncoderConfiguration,
                                                                  objectType: ObjectType.Aux[S, O],
                                                                  stringEncoder: Encoder[F, S, String],
                                                                  instances: => Generic.Sum.Instances[[X] =>> Encoder[F, S, X], A]
  ): F[O] =
    val constructorName = configuration.transformConstructorNames(instances.label(a))
    val encoder = instances.instance(a)
    (
      encoder.encode(a),
      stringEncoder.encode(constructorName)
    ).mapN { (s, encodedConstructorName) =>
      val obj = objectType.asObject(s).getOrElse(objectType.empty)
      val sum = encoder match
        case e: SumEncoder[F, S, ?] => true
        case _ => configuration.discriminator.exists(discriminator => objectType.contains(obj, discriminator))
      if sum then obj
      else
        configuration.discriminator match
          case Some(discriminator) => objectType.prepended(obj, (discriminator, encodedConstructorName))
          case _ => objectType.singleton(constructorName, s)
    }

end EncoderDerivationInstances
object EncoderDerivationInstances extends EncoderDerivationInstances
