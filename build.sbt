lazy val versions = new {
  val cats = "2.6.1"
  val zioPrelude = "1.0.0-RC7"
  val zio = "2.0.0-M4"
  val spire = "0.17.0"
  val monocle = "3.1.0"
  val catsMTL = "1.2.1"
  val shapeless3 = "3.0.3"
  val catsEffect = "3.2.9"
  val circe = "0.14.1"
  val akka = "2.6.16"
  val akkaHttp = "10.2.6"
  val hikariCP = "4.0.3"
  val postgresql = "42.2.20"
  val logbackClassic = "1.2.3"
  val scalatest = "3.2.10"
  val scalatestScalaCheck = "3.2.9.0"
}
lazy val dependencies = new {
  val zio = "dev.zio" %% "zio" % versions.zio
  val zioPrelude = "dev.zio" %% "zio-prelude" % versions.zioPrelude
  val cats = "org.typelevel" %% "cats-core" % versions.cats
  val spire = "org.typelevel" %% "spire" % versions.spire
  val shapeless3 =
    "org.typelevel" %% "shapeless3-deriving" % versions.shapeless3
  val catsMTL = "org.typelevel" %% "cats-mtl" % versions.catsMTL
  val catsLaws = "org.typelevel" %% "cats-laws" % versions.cats
  val catsEffect = "org.typelevel" %% "cats-effect" % versions.catsEffect
  val monocleCore =
    "dev.optics" %% "monocle-core" % versions.monocle
  val monocleMacro = "dev.optics" %% "monocle-macro" % versions.monocle
  val akkaActor =
    "com.typesafe.akka" %% "akka-actor-typed" % versions.akka
  val akkaStream =
    "com.typesafe.akka" %% "akka-stream" % versions.akka
  val akkaHttp =
    "com.typesafe.akka" %% "akka-http" % versions.akkaHttp
  val hikariCP = "com.zaxxer" % "HikariCP" % versions.hikariCP
  val postgresql = "org.postgresql" % "postgresql" % versions.postgresql
  val circeCore = "io.circe" %% "circe-core" % versions.circe
  val circeGeneric = "io.circe" %% "circe-generic" % versions.circe
  val circeParser = "io.circe" %% "circe-parser" % versions.circe
  val logbackClassic =
    "ch.qos.logback" % "logback-classic" % versions.logbackClassic
  val scalatest = "org.scalatest" %% "scalatest" % versions.scalatest
  val akkaPersistenceTestkit =
    "com.typesafe.akka" %% "akka-persistence-testkit" % versions.akka
}

lazy val commonLibraryDependencies =
  Seq(
    dependencies.cats withSources () withJavadoc (),
    dependencies.monocleCore withSources () withJavadoc (),
    dependencies.catsMTL withSources () withJavadoc (),
    dependencies.shapeless3 withSources () withJavadoc (),
    dependencies.monocleCore withSources () withJavadoc (),
    dependencies.monocleMacro withSources () withJavadoc (),
    dependencies.catsLaws % Test withSources () withJavadoc (),
    dependencies.scalatest % Test withSources ())

lazy val compiler =
  (project in file("compiler")).settings(
    name := "compiler",
    libraryDependencies ++=
      commonLibraryDependencies ++ Seq(
        dependencies.catsEffect,
        dependencies.zio)
      map (_ withSources ()))

ThisBuild / organization := "com.jourei"
ThisBuild / version := "0.0.1"
ThisBuild / scalaVersion := "3.1.0"
ThisBuild / scalacOptions ++= Seq("-source:future")
ThisBuild / autoCompilerPlugins := true

