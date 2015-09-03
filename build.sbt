organization := "org.microvm"

name := "microvm-refimpl2"

description := "The second reference implementation of Mu, the micro virtual machine"

licenses := Seq("CC BY-SA 4.0" -> url("https://creativecommons.org/licenses/by-sa/4.0/legalcode"))

scalaVersion := "2.11.7"

libraryDependencies := Seq(
    "org.antlr" % "antlr4" % "4.5",
    "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
    "ch.qos.logback" % "logback-classic" % "1.1.2",
    "com.github.jnr" % "jnr-ffi" % "2.0.3",
    "com.github.jnr" % "jffi" % "1.2.9",
    "com.github.jnr" % "jnr-posix" % "3.0.17",
    "org.scalatest" %% "scalatest" % "2.2.0"
)
	
antlr4Settings

antlr4PackageName in Antlr4 := Some("uvm.ir.textinput.gen")

antlr4GenListener in Antlr4 := false

antlr4GenVisitor in Antlr4 := false

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource + EclipseCreateSrc.Managed
