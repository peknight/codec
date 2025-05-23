ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.0"

ThisBuild / organization := "com.peknight"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-Xfatal-warnings",
    "-language:strictEquality",
    "-Xmax-inlines:64"
  ),
)

lazy val codec = (project in file("."))
  .aggregate(
    codecCore.jvm,
    codecCore.js,
    codecEffect.jvm,
    codecEffect.js,
    codecBase.jvm,
    codecBase.js,
    codecCirce.jvm,
    codecCirce.js,
    codecCirceParser.jvm,
    codecCirceParser.js,
    codecDoobie.jvm,
    codecDoobie.js,
    codecFs2IO.jvm,
    codecFs2IO.js,
    codecHttp4s.jvm,
    codecHttp4s.js,
    codecHttp4sCirce.jvm,
    codecHttp4sCirce.js,
    codecCiris.jvm,
    codecCiris.js,
    codecIp4s.jvm,
    codecIp4s.js,
    codecSquants.jvm,
    codecSquants.js,
  )
  .settings(commonSettings)
  .settings(
    name := "codec"
  )

lazy val codecCore = (crossProject(JSPlatform, JVMPlatform) in file("codec-core"))
  .settings(commonSettings)
  .settings(
    name := "codec-core",
    libraryDependencies ++= Seq(
      "com.peknight" %%% "generic-migration" % pekGenericVersion,
      "com.peknight" %%% "cats-ext" % pekExtVersion,
      "com.peknight" %%% "error-core" % pekErrorVersion,
      "com.peknight" %%% "commons-text" % pekCommonsVersion,
      "org.typelevel" %%% "cats-parse" % catsParseVersion,
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
    ),
  )

lazy val codecEffect = (crossProject(JSPlatform, JVMPlatform) in file("codec-effect"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-effect",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-effect" % catsEffectVersion,
      "com.peknight" %%% "commons-text" % pekCommonsVersion,
    ),
  )

lazy val codecBase = (crossProject(JSPlatform, JVMPlatform) in file("codec-base"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-base",
    libraryDependencies ++= Seq(
      "com.peknight" %%% "scodec-bits-ext" % pekExtVersion,
    ),
  )

lazy val codecCirce = (crossProject(JSPlatform, JVMPlatform) in file("codec-circe"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-circe",
    libraryDependencies ++= Seq(
      "com.peknight" %%% "circe-ext" % pekExtVersion,
      "com.peknight" %%% "cats-instances-circe" % pekInstancesVersion % Test,
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
    ),
  )

lazy val codecCirceParser = (crossProject(JSPlatform, JVMPlatform) in file("codec-circe-parser"))
  .dependsOn(codecCirce)
  .settings(commonSettings)
  .settings(
    name := "codec-circe-parser",
    libraryDependencies ++= Seq(
      "com.peknight" %%% "circe-parser-ext" % pekExtVersion,
      "org.scalatest" %%% "scalatest" % scalaTestVersion % Test,
    ),
  )

lazy val codecDoobie = (crossProject(JSPlatform, JVMPlatform) in file("codec-doobie"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-doobie",
    libraryDependencies ++= Seq(
    )
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      doobieCore,
    ),
  )

lazy val codecFs2IO = (crossProject(JSPlatform, JVMPlatform) in file("codec-fs2-io"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-fs2-io",
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-io" % fs2Version,
    )
  )
lazy val codecHttp4s = (crossProject(JSPlatform, JVMPlatform) in file("codec-http4s"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-http4s",
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-core" % http4sVersion,
    )
  )

lazy val codecHttp4sCirce = (crossProject(JSPlatform, JVMPlatform) in file("codec-http4s-circe"))
  .dependsOn(codecCirce)
  .settings(commonSettings)
  .settings(
    name := "codec-http4s-circe",
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-circe" % http4sVersion,
    )
  )

lazy val codecCiris = (crossProject(JSPlatform, JVMPlatform) in file("codec-ciris"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-ciris",
    libraryDependencies ++= Seq(
      "is.cir" %%% "ciris" % cirisVersion,
      "com.peknight" %%% "commons-text" % pekCommonsVersion,
    )
  )

lazy val codecIp4s = (crossProject(JSPlatform, JVMPlatform) in file("codec-ip4s"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-ip4s",
    libraryDependencies ++= Seq(
      "com.comcast" %%% "ip4s-core" % ip4sCoreVersion,
    )
  )

lazy val codecSquants = (crossProject(JSPlatform, JVMPlatform) in file("codec-squants"))
  .dependsOn(codecCore)
  .settings(commonSettings)
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
val cirisVersion = "3.8.0"
val ip4sCoreVersion = "3.7.0"
val squantsVersion = "1.8.3"
val scalaTestVersion = "3.2.19"
val pekVersion = "0.1.0-SNAPSHOT"
val pekGenericVersion = pekVersion
val pekCommonsVersion = pekVersion
val pekExtVersion = pekVersion
val pekInstancesVersion = pekVersion
val pekErrorVersion = pekVersion

val doobieCore = "org.tpolecat" %% "doobie-core" % doobieVersion
