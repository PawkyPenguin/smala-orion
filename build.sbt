enablePlugins(Antlr4Plugin)
Global / cancelable := true

val version = "0.1"

lazy val main = (project in file("main"))
	.settings(
		name := "Main",
		scalaVersion := "2.12.6",
		libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0",
		antlr4GenVisitor := true,
		commonSettings,
	)

libraryDependencies += "com.jsuereth" %% "scala-arm" % "2.0"

Antlr4 / antlr4GenVisitor := true
Antlr4 / antlr4GenListener := false

lazy val commonSettings = Seq(
	javacOptions ++= Seq(
		"-source", "1.8",
		"-target", "1.8",
		"-encoding", "UTF-8",
		"-Xlint"
	),
	scalacOptions ++= Seq(
		"-Ypartial-unification",
		"-Xlint",
		"-Ywarn-dead-code",
		"-Ywarn-value-discard",
		"-Ywarn-numeric-widen",
		"-Ywarn-unused",
		"-Ywarn-unused-import",
		"-unchecked",
		"-deprecation",
		"-feature",
		"-encoding", "UTF-8",
		"-target:jvm-1.8"
		),
	cancelable := true,
	exportJars := true
)

