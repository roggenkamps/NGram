name := "NGram"

version := "0.01"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq (
)

// scalacOptions += "-deprecation"

//
mainClass in (Compile,packageBin) := Some("NGram")

//
mainClass in (Compile,run) := Some("NGram")