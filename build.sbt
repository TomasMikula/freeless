name := "freeless"

scalaVersion := "2.11.8"

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.0" cross CrossVersion.binary)
addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.0-M5"

tutSettings
