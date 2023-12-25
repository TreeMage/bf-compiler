val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "bf-compiler",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "com.monovore" %% "decline" % "2.4.1"
    )

  )
