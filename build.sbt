import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val codec = (project in file("."))
  .settings(name := "codec")
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

lazy val codecCore = (crossProject(JVMPlatform, JSPlatform) in file("codec-core"))
  .settings(name := "codec-core")
  .settings(crossDependencies(
    peknight.generic.migration,
    peknight.cats,
    peknight.error,
    peknight.commons.text,
    typelevel.catsParse,
  ))
  .settings(crossTestDependencies(scalaTest))

lazy val codecEffect = (crossProject(JVMPlatform, JSPlatform) in file("codec-effect"))
  .dependsOn(codecCore)
  .settings(name := "codec-effect")
  .settings(crossDependencies(
    typelevel.catsEffect,
  ))

lazy val codecBase = (crossProject(JVMPlatform, JSPlatform) in file("codec-base"))
  .dependsOn(codecCore)
  .settings(name := "codec-base")
  .settings(crossDependencies(peknight.scodec.bits))

lazy val codecCaseInsensitive = (crossProject(JVMPlatform, JSPlatform) in file("codec-case-insensitive"))
  .dependsOn(codecCore)
  .settings(name := "codec-case-insensitive")
  .settings(crossDependencies(typelevel.caseInsensitive))

lazy val codecCirce = (crossProject(JVMPlatform, JSPlatform) in file("codec-circe"))
  .dependsOn(codecCore)
  .settings(name := "codec-circe")
  .settings(crossDependencies(peknight.circe))
  .settings(crossTestDependencies(scalaTest))

lazy val codecCirceParser = (crossProject(JVMPlatform, JSPlatform) in file("codec-circe-parser"))
  .dependsOn(codecCirce)
  .settings(name := "codec-circe-parser")
  .settings(crossDependencies(peknight.circe.parser))
  .settings(crossTestDependencies(scalaTest))

lazy val codecDoobie = (crossProject(JVMPlatform, JSPlatform) in file("codec-doobie"))
  .dependsOn(codecCore)
  .settings(name := "codec-doobie")
  .jvmSettings(libraryDependencies ++= Seq(dependency(tpolecat.doobie)))

lazy val codecFs2IO = (crossProject(JVMPlatform, JSPlatform) in file("codec-fs2-io"))
  .dependsOn(codecCore)
  .settings(name := "codec-fs2-io")
  .settings(crossDependencies(fs2.io))

lazy val codecHttp4s = (crossProject(JVMPlatform, JSPlatform) in file("codec-http4s"))
  .dependsOn(codecCaseInsensitive)
  .settings(name := "codec-http4s")
  .settings(crossDependencies(http4s))

lazy val codecHttp4sCirce = (crossProject(JVMPlatform, JSPlatform) in file("codec-http4s-circe"))
  .dependsOn(codecCirce)
  .settings(name := "codec-http4s-circe")
  .settings(crossDependencies(http4s.circe))

lazy val codecCiris = (crossProject(JVMPlatform, JSPlatform) in file("codec-ciris"))
  .dependsOn(codecCore)
  .settings(name := "codec-ciris")
  .settings(crossDependencies(
    cir.ciris,
  ))

lazy val codecIp4s = (crossProject(JVMPlatform, JSPlatform) in file("codec-ip4s"))
  .dependsOn(codecCore)
  .settings(name := "codec-ip4s")
  .settings(crossDependencies(peknight.ip4s))

lazy val codecSquants = (crossProject(JVMPlatform, JSPlatform) in file("codec-squants"))
  .dependsOn(codecCore)
  .settings(name := "codec-squants")
  .settings(crossDependencies(typelevel.squants))
