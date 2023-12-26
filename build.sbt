lazy val root = project
  .in(file("."))
  .settings(
    name         := "bf-compiler",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"     % "0.7.29" % Test,
      "org.typelevel" %% "cats-core" % "2.9.0",
      "com.monovore"  %% "decline"   % "2.4.1"
    )
  )
val scala3Version = "3.3.1"
