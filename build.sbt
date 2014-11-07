organization := "org.microvm"

name := "microvm-refimpl2"

description := "The second reference implementation of MicroVM"

licenses := Seq("CC BY-SA 4.0" -> url("https://creativecommons.org/licenses/by-sa/4.0/legalcode"))

scalaVersion := "2.11.4"

libraryDependencies := Seq(
	"org.antlr" % "antlr4" % "4.3",
	"org.scalatest" %% "scalatest" % "2.2.0"
	)
	
antlr4Settings

antlr4PackageName in Antlr4 := Some("uvm.ir.textinput.gen")

antlr4GenListener in Antlr4 := false

antlr4GenVisitor in Antlr4 := false

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource + EclipseCreateSrc.Managed
