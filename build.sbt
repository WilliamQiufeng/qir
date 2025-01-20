ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "qir"
  )

libraryDependencies += "com.github.j-mie6" %% "parsley" % "4.5.3"
libraryDependencies += "com.github.j-mie6" %% "parsley-cats" % "1.3.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
libraryDependencies += "org.scalatestplus" %% "junit-5-11" % "3.2.19.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.19" % "test"