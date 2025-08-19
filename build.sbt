import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val codec = (project in file("."))
  .aggregate(
    codecCore.jvm,
    codecCore.js,
    codecCore.native,
    codecEffect.jvm,
    codecEffect.js,
    codecBase.jvm,
    codecBase.js,
    codecBase.native,
    codecCirce.jvm,
    codecCirce.js,
    codecCirce.native,
    codecCirceParser.jvm,
    codecCirceParser.js,
    codecCirceParser.native,
    codecDoobie.jvm,
    codecDoobie.js,
    codecDoobie.native,
    codecFs2IO.jvm,
    codecFs2IO.js,
    codecHttp4s.jvm,
    codecHttp4s.js,
    codecHttp4s.native,
    codecHttp4sCirce.jvm,
    codecHttp4sCirce.js,
    codecHttp4sCirce.native,
    codecCiris.jvm,
    codecCiris.js,
    codecCiris.native,
    codecIp4s.jvm,
    codecIp4s.js,
    codecIp4s.native,
    codecSquants.jvm,
    codecSquants.js,
    codecSquants.native,
  )
  .settings(
    name := "codec"
  )

lazy val codecCore = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-core"))
  .settings(crossDependencies(
    peknight.generic.migration,
    peknight.ext.cats,
    peknight.error,
    peknight.commons.text,
    typelevel.catsParse,
  ))
  .settings(crossTestDependencies(
    scalaTest
  ))
  .settings(
    name := "codec-core",
  )

lazy val codecEffect = (crossProject(JVMPlatform, JSPlatform) in file("codec-effect"))
  .dependsOn(codecCore)
  .settings(crossDependencies(
    typelevel.catsEffect,
    peknight.commons.text,
  ))
  .settings(
    name := "codec-effect",
  )

lazy val codecBase = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-base"))
  .dependsOn(codecCore)
  .settings(crossDependencies(peknight.ext.scodec.bits))
  .settings(
    name := "codec-base",
  )

lazy val codecCirce = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-circe"))
  .dependsOn(codecCore)
  .settings(crossDependencies(peknight.ext.circe))
  .settings(crossTestDependencies(
    peknight.instances.cats.circe,
    scalaTest,
  ))
  .settings(
    name := "codec-circe",
  )

lazy val codecCirceParser = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-circe-parser"))
  .dependsOn(codecCirce)
  .settings(crossDependencies(peknight.ext.circeParser))
  .settings(crossTestDependencies(scalaTest))
  .settings(
    name := "codec-circe-parser",
  )

lazy val codecDoobie = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-doobie"))
  .dependsOn(codecCore)
  .settings(
    name := "codec-doobie",
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      jvmDependency(tpolecat.doobie),
    ),
  )

lazy val codecFs2IO = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-fs2-io"))
  .dependsOn(codecCore)
  .settings(
    name := "codec-fs2-io",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-io" % fs2Version,
    )
  )
lazy val codecHttp4s = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-http4s"))
  .dependsOn(codecCore)
  .settings(
    name := "codec-http4s",
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-core" % http4sVersion,
    )
  )

lazy val codecHttp4sCirce = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-http4s-circe"))
  .dependsOn(codecCirce)
  .settings(
    name := "codec-http4s-circe",
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-circe" % http4sVersion,
    )
  )

lazy val codecCiris = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-ciris"))
  .dependsOn(codecCore)
  .settings(
    name := "codec-ciris",
    libraryDependencies ++= Seq(
      "is.cir" %%% "ciris" % cirisVersion,
      "com.peknight" %%% "commons-text" % pekCommonsVersion,
    )
  )

lazy val codecIp4s = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-ip4s"))
  .dependsOn(codecCore)
  .settings(
    name := "codec-ip4s",
    libraryDependencies ++= Seq(
      "com.comcast" %%% "ip4s-core" % ip4sCoreVersion,
    )
  )

lazy val codecSquants = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("codec-squants"))
  .dependsOn(codecCore)
  .settings(
    name := "codec-squants",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "squants" % squantsVersion,
    )
  )

val catsEffectVersion = "3.6.1"
val catsParseVersion = "0.3.10"
val fs2Version = "3.12.0"
val scodecVersion = "1.2.1"
val doobieVersion = "1.0.0-RC9"
val http4sVersion = "1.0.0-M34"
val cirisVersion = "3.9.0"
val ip4sCoreVersion = "3.7.0"
val squantsVersion = "1.8.3"
val scalaTestVersion = "3.2.19"
val pekVersion = "0.1.0-SNAPSHOT"
val pekCommonsVersion = pekVersion
val pekExtVersion = pekVersion
val pekInstancesVersion = pekVersion
val pekGenericVersion = pekVersion
val pekErrorVersion = pekVersion

val doobieCore = "org.tpolecat" %% "doobie-core" % doobieVersion
