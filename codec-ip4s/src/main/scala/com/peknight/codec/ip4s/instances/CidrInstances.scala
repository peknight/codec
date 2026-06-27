package com.peknight.codec.ip4s.instances

import cats.{Applicative, Show}
import com.comcast.ip4s.{Cidr, IpAddress, Ipv4Address, Ipv6Address}
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType

trait CidrInstances:
  given stringCodecIpv4Cidr[F[_]: Applicative]: Codec[F, String, String, Cidr[Ipv4Address]] =
    Codec.mapOption[F, String, String, Cidr[Ipv4Address]](_.toString)(Cidr.fromString4)
  given codecIpv4Cidr[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], Cidr[Ipv4Address]] =
    Codec.codecS[F, S, Cidr[Ipv4Address]]
  given stringCodecIpv6Cidr[F[_]: Applicative]: Codec[F, String, String, Cidr[Ipv6Address]] =
    Codec.mapOption[F, String, String, Cidr[Ipv6Address]](_.toString)(Cidr.fromString6)
  given codecIpv6Cidr[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], Cidr[Ipv6Address]] =
    Codec.codecS[F, S, Cidr[Ipv6Address]]
  given stringCodecCidr[F[_]: Applicative]: Codec[F, String, String, Cidr[IpAddress]] =
    Codec.mapOption[F, String, String, Cidr[IpAddress]](_.toString)(Cidr.fromString)
  given codecCidr[F[_]: Applicative, S: {StringType, Show}]: Codec[F, S, Cursor[S], Cidr[IpAddress]] =
    Codec.codecS[F, S, Cidr[IpAddress]]
end CidrInstances
object CidrInstances extends CidrInstances
