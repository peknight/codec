package com.peknight.codec.ip4s.instances

import cats.Applicative
import com.comcast.ip4s.*
import com.peknight.codec.cursor.Cursor
import com.peknight.codec.error.DecodingFailure
import com.peknight.codec.sum.StringType
import com.peknight.codec.{Decoder, Encoder}

trait HostInstances:
  given stringEncodeHostname[F[_]: Applicative]: Encoder[F, String, Hostname] =
    Encoder.toStringEncoder[F, Hostname]
  given encodeHostname[F[_]: Applicative, S: StringType]: Encoder[F, S, Hostname] =
    Encoder.stringEncoder[F, S, Hostname]
  given stringDecodeHostname[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Hostname] =
    Decoder.decodeWithOption[F, Hostname](Hostname.fromString)
  given decodeHostname[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Hostname] =
    Decoder.stringDecoder[F, S, Hostname]

  given stringEncodeIDN[F[_]: Applicative]: Encoder[F, String, IDN] =
    Encoder.toStringEncoder[F, IDN]
  given encodeIDN[F[_]: Applicative, S: StringType]: Encoder[F, S, IDN] =
    Encoder.stringEncoder[F, S, IDN]
  given stringDecodeIDN[F[_]: Applicative]: Decoder[F, String, DecodingFailure, IDN] =
    Decoder.decodeWithOption[F, IDN](IDN.fromString)
  given decodeIDN[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, IDN] =
    Decoder.stringDecoder[F, S, IDN]

  given stringEncodeIpv4Address[F[_]: Applicative]: Encoder[F, String, Ipv4Address] =
    Encoder.toStringEncoder[F, Ipv4Address]
  given encodeIpv4Address[F[_]: Applicative, S: StringType]: Encoder[F, S, Ipv4Address] =
    Encoder.stringEncoder[F, S, Ipv4Address]
  given stringDecodeIpv4Address[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Ipv4Address] =
    Decoder.decodeWithOption[F, Ipv4Address](Ipv4Address.fromString)
  given decodeIpv4Address[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Ipv4Address] =
    Decoder.stringDecoder[F, S, Ipv4Address]

  given stringEncodeIpv6Address[F[_]: Applicative]: Encoder[F, String, Ipv6Address] =
    Encoder.toStringEncoder[F, Ipv6Address]
  given encodeIpv6Address[F[_]: Applicative, S: StringType]: Encoder[F, S, Ipv6Address] =
    Encoder.stringEncoder[F, S, Ipv6Address]
  given stringDecodeIpv6Address[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Ipv6Address] =
    Decoder.decodeWithOption[F, Ipv6Address](Ipv6Address.fromString)
  given decodeIpv6Address[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Ipv6Address] =
    Decoder.stringDecoder[F, S, Ipv6Address]

  given stringEncodeIpAddress[F[_]: Applicative]: Encoder[F, String, IpAddress] =
    Encoder.toStringEncoder[F, IpAddress]
  given encodeIpAddress[F[_]: Applicative, S: StringType]: Encoder[F, S, IpAddress] =
    Encoder.stringEncoder[F, S, IpAddress]
  given stringDecodeIpAddress[F[_]: Applicative]: Decoder[F, String, DecodingFailure, IpAddress] =
    Decoder.decodeWithOption[F, IpAddress](IpAddress.fromString)
  given decodeIpAddress[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, IpAddress] =
    Decoder.stringDecoder[F, S, IpAddress]

  given stringEncodeHost[F[_]: Applicative]: Encoder[F, String, Host] =
    Encoder.toStringEncoder[F, Host]
  given encodeHost[F[_]: Applicative, S: StringType]: Encoder[F, S, Host] =
    Encoder.stringEncoder[F, S, Host]
  given stringDecodeHost[F[_]: Applicative]: Decoder[F, String, DecodingFailure, Host] =
    Decoder.decodeWithOption[F, Host](Host.fromString)
  given decodeHost[F[_]: Applicative, S: StringType]: Decoder[F, Cursor[S], DecodingFailure, Host] =
    Decoder.stringDecoder[F, S, Host]
end HostInstances
object HostInstances extends HostInstances
