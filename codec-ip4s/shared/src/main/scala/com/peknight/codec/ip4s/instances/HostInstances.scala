package com.peknight.codec.ip4s.instances

import cats.Applicative
import com.comcast.ip4s.*
import com.peknight.codec.Codec
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.sum.StringType

import scala.reflect.ClassTag

trait HostInstances:
  private def stringCodecHostRemoveBrackets[F[_]: Applicative, A <: Host: ClassTag](f: String => Option[A])
  : Codec[F, String, String, A] =
    Codec.mapOption[F, String, String, A](_.toString)(host => f(removeBrackets(host)))

  private def removeBrackets(address: String): String =
    if address.startsWith("[") && address.endsWith("]") then address.substring(1, address.length - 1)
    else address

  given stringCodecHostname[F[_]: Applicative]: Codec[F, String, String, Hostname] =
    Codec.mapOption[F, String, String, Hostname](_.toString)(Hostname.fromString)
  given codecHostnameS[F[_]: Applicative, S: StringType]: Codec[F, S, Cursor[S], Hostname] = Codec.codecS[F, S, Hostname]

  given stringCodecIDN[F[_]: Applicative]: Codec[F, String, String, IDN] =
    Codec.mapOption[F, String, String, IDN](_.toString)(IDN.fromString)
  given codecIDNS[F[_]: Applicative, S: StringType]: Codec[F, S, Cursor[S], IDN] = Codec.codecS[F, S, IDN]


  given stringCodecIpv4Address[F[_]: Applicative]: Codec[F, String, String, Ipv4Address] =
    stringCodecHostRemoveBrackets[F, Ipv4Address](Ipv4Address.fromString)
  given codecIpv4AddressS[F[_]: Applicative, S: StringType]: Codec[F, S, Cursor[S], Ipv4Address] =
    Codec.codecS[F, S, Ipv4Address]


  given stringCodecIpv6Address[F[_] : Applicative]: Codec[F, String, String, Ipv6Address] =
    stringCodecHostRemoveBrackets[F, Ipv6Address](Ipv6Address.fromString)
  given codecIpv6AddressS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], Ipv6Address] =
    Codec.codecS[F, S, Ipv6Address]

  given stringCodecIpAddress[F[_] : Applicative]: Codec[F, String, String, IpAddress] =
    stringCodecHostRemoveBrackets[F, IpAddress](IpAddress.fromString)
  given codecIpAddressS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], IpAddress] =
    Codec.codecS[F, S, IpAddress]

  given stringCodecHost[F[_] : Applicative]: Codec[F, String, String, Host] =
    Codec.mapOption[F, String, String, Host](_.toString)(Host.fromString)
  given codecHostS[F[_] : Applicative, S: StringType]: Codec[F, S, Cursor[S], Host] =
    Codec.codecS[F, S, Host]
end HostInstances
object HostInstances extends HostInstances
