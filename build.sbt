
lazy val genSrc = taskKey[List[File]]("generate sources")

genSrc <<= (sourceGenerators in Compile) { _.join.map(_.flatten.toList) }

organization := "org.microvm"

name := "microvm-refimpl2"

version := "2.2.0"

description := "The second reference implementation of Mu, the micro virtual machine"

licenses := Seq("CC BY-SA 4.0" -> url("https://creativecommons.org/licenses/by-sa/4.0/legalcode"))

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % "2.11.8",
    "org.antlr" % "antlr4" % "4.5.2-1",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "com.github.jnr" % "jnr-ffi" % "2.0.9",
    "com.github.jnr" % "jffi" % "1.2.11",
    "com.github.jnr" % "jnr-posix" % "3.0.29",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "junit" % "junit" % "4.12" % "test"
)

testOptions in Test += Tests.Argument("-oF") // print full stack trace when testing

parallelExecution in Test := false  // disable parallel tests because the refimpl2 is not thread-safe

antlr4Settings

antlr4PackageName in Antlr4 := Some("uvm.ir.textinput.gen")

antlr4GenListener in Antlr4 := false

antlr4GenVisitor in Antlr4 := false

lazy val makeClasspathFile = taskKey[Unit]("write the run-time classpath to target/jars.txt as colon-separated list")

makeClasspathFile := {
  val cp = (fullClasspath in Runtime).value.files 

  println("fullClasspath: \n" + cp.mkString("\n"))

  val cpStr = cp.mkString(":")

  IO.write(new java.io.File("cbinding/classpath.txt"), cpStr)
}
