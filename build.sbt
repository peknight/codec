import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

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
  .settings(
    name := "codec"
  )

lazy val codecCore = (crossProject(JVMPlatform, JSPlatform) in file("codec-core"))
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

lazy val codecBase = (crossProject(JVMPlatform, JSPlatform) in file("codec-base"))
  .dependsOn(codecCore)
  .settings(crossDependencies(peknight.ext.scodec.bits))
  .settings(
    name := "codec-base",
  )

lazy val codecCirce = (crossProject(JVMPlatform, JSPlatform) in file("codec-circe"))
  .dependsOn(codecCore)
  .settings(crossDependencies(peknight.ext.circe))
  .settings(crossTestDependencies(
    peknight.instances.cats.circe,
    scalaTest,
  ))
  .settings(
    name := "codec-circe",
  )

lazy val codecCirceParser = (crossProject(JVMPlatform, JSPlatform) in file("codec-circe-parser"))
  .dependsOn(codecCirce)
  .settings(crossDependencies(peknight.ext.circeParser))
  .settings(crossTestDependencies(scalaTest))
  .settings(
    name := "codec-circe-parser",
  )

lazy val codecDoobie = (crossProject(JVMPlatform, JSPlatform) in file("codec-doobie"))
  .dependsOn(codecCore)
  .settings(
    name := "codec-doobie",
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      dependency(tpolecat.doobie),
    ),
  )

lazy val codecFs2IO = (crossProject(JVMPlatform, JSPlatform) in file("codec-fs2-io"))
  .dependsOn(codecCore)
  .settings(crossDependencies(fs2.io))
  .settings(
    name := "codec-fs2-io",
  )

lazy val codecHttp4s = (crossProject(JVMPlatform, JSPlatform) in file("codec-http4s"))
  .dependsOn(codecCore)
  .settings(crossDependencies(http4s))
  .settings(
    name := "codec-http4s",
  )

lazy val codecHttp4sCirce = (crossProject(JVMPlatform, JSPlatform) in file("codec-http4s-circe"))
  .dependsOn(codecCirce)
  .settings(crossDependencies(http4s.circe))
  .settings(
    name := "codec-http4s-circe",
  )

lazy val codecCiris = (crossProject(JVMPlatform, JSPlatform) in file("codec-ciris"))
  .dependsOn(codecCore)
  .settings(crossDependencies(
    cir.ciris,
    peknight.commons.text,
  ))
  .settings(
    name := "codec-ciris",
  )

lazy val codecIp4s = (crossProject(JVMPlatform, JSPlatform) in file("codec-ip4s"))
  .dependsOn(codecCore)
  .settings(crossDependencies(comcast.ip4s))
  .settings(
    name := "codec-ip4s",
  )

lazy val codecSquants = (crossProject(JVMPlatform, JSPlatform) in file("codec-squants"))
  .dependsOn(codecCore)
  .settings(crossDependencies(typelevel.squants))
  .settings(
    name := "codec-squants",
  )
