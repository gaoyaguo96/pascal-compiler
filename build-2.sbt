lazy val versions = new {
  val cats = "2.6.1"
  val shapeless = "2.3.7"
  val monocle = "3.0.0"
  val catsMTL = "1.2.1"
  val catsEffect = "3.1.1"
  val circe = "0.14.1"
  val akka = "2.6.15"
  val akkaHttp = "10.2.4"
  val akkaProjection = "1.2.1"
  val akkaPersistenceJdbc = "5.0.1"
  val hikariCP = "4.0.3"
  val postgresql = "42.2.20"
  val okhttp3 = "4.9.1"
  val scalikejdbc = "3.5.0"
  val jsoup = "1.13.1"
  val logbackClassic = "1.2.3"
  val scalatest = "3.2.9"
  val scalatestScalaCheck = "3.2.9.0"
  val kindProjector = "0.13.0"
  val betterMonadicFor = "0.3.1"
}
lazy val dependencies = new {
  val cats = "org.typelevel" %% "cats-core" % versions.cats
  val catsMTL = "org.typelevel" %% "cats-mtl" % versions.catsMTL
  val catsEffect = "org.typelevel" %% "cats-effect" % versions.catsEffect
  val shapeless = "com.chuusai" %% "shapeless" % versions.shapeless
  val monocleCore =
    "dev.optics" %% "monocle-core" % versions.monocle
  val monocleMacro = "dev.optics" %% "monocle-macro" % versions.monocle
  val akkaActor = "com.typesafe.akka" %% "akka-actor-typed" % versions.akka
  val akkaStream = "com.typesafe.akka" %% "akka-stream" % versions.akka
  val akkaHttp = "com.typesafe.akka" %% "akka-http" % versions.akkaHttp
  val akkaPersistence =
    "com.typesafe.akka" %% "akka-persistence-typed" % versions.akka
  val akkaPersistenceJdbc =
    "com.lightbend.akka" %% "akka-persistence-jdbc" % versions.akkaPersistenceJdbc
  val akkaProjectionJdbc =
    "com.lightbend.akka" %% "akka-projection-jdbc" % versions.akkaProjection
  val akkaProjectionEventsourced =
    "com.lightbend.akka" %% "akka-projection-eventsourced" % versions.akkaProjection
  val hikariCP = "com.zaxxer" % "HikariCP" % versions.hikariCP
  val scalikeJdbc = "org.scalikejdbc" %% "scalikejdbc" % versions.scalikejdbc
  val scalikeJdbcConfig =
    "org.scalikejdbc" %% "scalikejdbc-config" % versions.scalikejdbc
  val postgresql = "org.postgresql" % "postgresql" % versions.postgresql
  val jsoup = "org.jsoup" % "jsoup" % versions.jsoup
  val okhttp3 = "com.squareup.okhttp3" % "okhttp" % versions.okhttp3
  val circeCore = "io.circe" %% "circe-core" % versions.circe
  val circeGeneric = "io.circe" %% "circe-generic" % versions.circe
  val circeParser = "io.circe" %% "circe-parser" % versions.circe
  val logbackClassic =
    "ch.qos.logback" % "logback-classic" % versions.logbackClassic
  val scalatest = "org.scalatest" %% "scalatest" % versions.scalatest
  val scalatestScalaCheck =
    "org.scalatestplus" %% "scalacheck-1-15" % versions.scalatestScalaCheck
  val akkaPersistenceTestkit =
    "com.typesafe.akka" %% "akka-persistence-testkit" % versions.akka
}

lazy val commonLibraryDependencies =
  Seq(
    dependencies.cats,
    dependencies.catsMTL,
    dependencies.shapeless,
    dependencies.monocleCore,
    dependencies.monocleMacro,
    dependencies.scalatest % Test,
    dependencies.scalatestScalaCheck % Test)

lazy val compiler =
  (project in file("compiler")).settings(
    name := "compiler",
    libraryDependencies ++=
      commonLibraryDependencies ++ Seq(dependencies.catsEffect)
      map (_ withSources () withJavadoc ()),
    addCompilerPlugin(
      "org.typelevel" % "kind-projector" % versions.kindProjector cross CrossVersion.full),
    addCompilerPlugin(
      "com.olegpy" %% "better-monadic-for" % versions.betterMonadicFor))

ThisBuild / organization := "com.jourei"
ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "2.13.6"
ThisBuild / scalacOptions ++= Seq("-target:11")
ThisBuild / autoCompilerPlugins := true
