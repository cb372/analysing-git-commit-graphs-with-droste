scalaVersion := "2.12.9"
libraryDependencies ++= Seq(
  "io.higherkindness" %% "droste-core" % "0.7.0",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "5.4.2.201908231537-r"
)
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.10.3")
scalacOptions += "-Ypartial-unification"
