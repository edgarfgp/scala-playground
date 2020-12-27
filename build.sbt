name := "scala-playground"

version := "0.1"

scalaVersion := "2.13.4"

val CatsVersion = "2.1.1"

libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" %CatsVersion
)
