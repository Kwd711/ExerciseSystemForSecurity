name := "ExerciseSystemForSecurity"

version := "1.0"

scalaVersion := "2.13.10"

lazy val akkaVersion = "2.7.0"

// Run in a separate JVM, to make sure sbt waits until all threads have
// finished before returning.
// If you want to keep the application running while executing other
// sbt tasks, consider https://github.com/spray/sbt-revolver/
fork := true

//下のが無いとIntelliJのターミナル以外で標準入力が受け取れないので注意
connectInput := true

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.2.11",
  "com.typesafe.akka" %% "akka-actor-testkit-typed" % akkaVersion % Test,
  "org.scalatest" %% "scalatest" % "3.1.4" % Test
)
