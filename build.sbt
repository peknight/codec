ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

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
    codecCirce.jvm,
    codecCirce.js,
    codecDoobie.jvm,
    codecDoobie.js,
    codecHttp4s.jvm,
    codecHttp4s.js,
    codecCiris.jvm,
    codecCiris.js,
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
    ),
  )

lazy val codecCirce = (crossProject(JSPlatform, JVMPlatform) in file("codec-circe"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-circe",
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % circeVersion,
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

lazy val codecHttp4s = (crossProject(JSPlatform, JVMPlatform) in file("codec-http4s"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-http4s",
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-core" % http4sVersion,
    )
  )

lazy val codecCiris = (crossProject(JSPlatform, JVMPlatform) in file("codec-ciris"))
  .dependsOn(codecCore)
  .settings(commonSettings)
  .settings(
    name := "codec-ciris",
    libraryDependencies ++= Seq(
      "is.cir" %%% "ciris" % cirisVersion,
    )
  )



val circeVersion = "0.14.6"
val doobieVersion = "1.0.0-RC4"
val http4sVersion = "1.0.0-M34"
val cirisVersion = "3.2.0"
val pekVersion = "0.1.0-SNAPSHOT"
val pekGenericVersion = pekVersion
val pekExtVersion = pekVersion
val pekErrorVersion = pekVersion

val doobieCore = "org.tpolecat" %% "doobie-core" % doobieVersion
