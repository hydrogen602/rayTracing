
name := "RayTracer"
version := "1.0"
scalaVersion := "2.12.8"
run / fork := true

//mainClass in (Compile, packageBin) := Some("rayTracing.Main")

mainClass in (Compile, run) := Some("rayTracing.Main")

libraryDependencies ++= Seq(
	"org.scalafx" %% "scalafx" % "8.0.192-R14",
	"com.novocode" % "junit-interface" % "0.11" % Test,
	"org.scalactic" %% "scalactic" % "3.0.8",
	"org.scalatest" %% "scalatest" % "3.0.8" % "test",
	"org.scala-lang.modules" %% "scala-xml" % "1.2.0"
)

