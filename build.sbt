import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val codec = (project in file("."))
  .settings(name := "codec")
  .aggregate(codecCore.projectRefs *)
  .aggregate(codecEffect.projectRefs *)
  .aggregate(codecBase.projectRefs *)
  .aggregate(codecCaseInsensitive.projectRefs *)
  .aggregate(codecCirce.projectRefs *)
  .aggregate(codecCirceParser.projectRefs *)
  .aggregate(codecDoobie.projectRefs *)
  .aggregate(codecFs2IO.projectRefs *)
  .aggregate(codecHttp4s.projectRefs *)
  .aggregate(codecHttp4sCirce.projectRefs *)
  .aggregate(codecCiris.projectRefs *)
  .aggregate(codecIp4s.projectRefs *)
  .aggregate(codecSquants.projectRefs *)

lazy val codecCore = (projectMatrix in file("codec-core"))
  .settings(name := "codec-core")
  .settings(libraryDependencies ++= dependencies(
    peknight.generic.migration,
    peknight.cats,
    peknight.error,
    peknight.commons.text,
    typelevel.catsParse,
  ))
  .settings(libraryDependencies ++= testDependencies(scalaTest))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecEffect = (projectMatrix in file("codec-effect"))
  .dependsOn(codecCore)
  .settings(name := "codec-effect")
  .settings(libraryDependencies ++= dependencies(typelevel.catsEffect))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecBase = (projectMatrix in file("codec-base"))
  .dependsOn(codecCore)
  .settings(name := "codec-base")
  .settings(libraryDependencies ++= dependencies(peknight.scodec.bits))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecCaseInsensitive = (projectMatrix in file("codec-case-insensitive"))
  .dependsOn(codecCore)
  .settings(name := "codec-case-insensitive")
  .settings(libraryDependencies ++= dependencies(typelevel.caseInsensitive))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecCirce = (projectMatrix in file("codec-circe"))
  .dependsOn(codecCore)
  .settings(name := "codec-circe")
  .settings(libraryDependencies ++= dependencies(peknight.circe))
  .settings(libraryDependencies ++= testDependencies(scalaTest))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecCirceParser = (projectMatrix in file("codec-circe-parser"))
  .dependsOn(codecCirce)
  .settings(name := "codec-circe-parser")
  .settings(libraryDependencies ++= dependencies(peknight.circe.parser))
  .settings(libraryDependencies ++= testDependencies(scalaTest))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecDoobie = (projectMatrix in file("codec-doobie"))
  .dependsOn(codecCore)
  .settings(name := "codec-doobie")
  .jvmPlatform(
    scalaVersions = Seq(scala.scala3.version),
    settings = Seq(
      libraryDependencies ++= dependencies(typelevel.doobie)
    )
  )
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecFs2IO = (projectMatrix in file("codec-fs2-io"))
  .dependsOn(codecCore)
  .settings(name := "codec-fs2-io")
  .settings(libraryDependencies ++= dependencies(fs2.io))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecHttp4s = (projectMatrix in file("codec-http4s"))
  .dependsOn(codecCaseInsensitive)
  .settings(name := "codec-http4s")
  .settings(libraryDependencies ++= dependencies(http4s))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecHttp4sCirce = (projectMatrix in file("codec-http4s-circe"))
  .dependsOn(codecCirce)
  .settings(name := "codec-http4s-circe")
  .settings(libraryDependencies ++= dependencies(http4s.circe))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecCiris = (projectMatrix in file("codec-ciris"))
  .dependsOn(codecCore)
  .settings(name := "codec-ciris")
  .settings(libraryDependencies ++= dependencies(cir.ciris))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecIp4s = (projectMatrix in file("codec-ip4s"))
  .dependsOn(codecCore)
  .settings(name := "codec-ip4s")
  .settings(libraryDependencies ++= dependencies(peknight.ip4s))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

lazy val codecSquants = (projectMatrix in file("codec-squants"))
  .dependsOn(codecCore)
  .settings(name := "codec-squants")
  .settings(libraryDependencies ++= dependencies(typelevel.squants))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))
