val dottyVersion = "0.10.0-RC1"
val version = "0.1"

lazy val main = (project in file("main"))
	.settings(
		name := "Main",
		scalaVersion := dottyVersion,
		commonSettings,
	)

lazy val commonSettings = Seq(
	scalacOptions ++= Seq(
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
