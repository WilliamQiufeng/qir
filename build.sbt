ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

ThisBuild / semanticdbEnabled := true

lazy val root = (project in file("."))
  .settings(
    name := "qir",
    scalacOptions += "-Wunused:imports"
  )

libraryDependencies += "com.github.j-mie6" %% "parsley" % "4.5.3"
libraryDependencies += "com.github.j-mie6" %% "parsley-cats" % "1.3.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0"
libraryDependencies += "org.scalatestplus" %% "junit-5-11" % "3.2.19.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.19" % "test"
libraryDependencies += "org.scala-graph" %% "graph-core" % "2.0.2"
libraryDependencies += "org.scala-graph" %% "graph-dot" % "2.0.0"
libraryDependencies ++= Seq(
  "dev.optics" %% "monocle-core" % "3.3.0",
  "dev.optics" %% "monocle-macro" % "3.3.0",
)
libraryDependencies += "org.typelevel" %% "cats-collections-core" % "0.9.9"
libraryDependencies += "org.typelevel" %% "cats-mtl" % "1.5.0"