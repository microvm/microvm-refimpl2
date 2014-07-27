organization := "org.microvm"

name := "simplest-microvm-project"

description := "An attempt to make the simplest (in term of conciseness) Micro-VM implementation"

licenses := Seq("CC BY-SA 4.0" -> url("https://creativecommons.org/licenses/by-sa/4.0/legalcode"))

scalaVersion := "2.11.2"

libraryDependencies := Seq(
	"org.antlr" % "antlr4" % "4.3",
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2",
	"org.scalatest" %% "scalatest" % "2.2.0"
	)
	
antlr4Settings

antlr4PackageName in Antlr4 := Some("uvm.ir.textinput.gen")

antlr4GenListener in Antlr4 := false

antlr4GenVisitor in Antlr4 := false

sourceManaged := file("target/gen")

unmanagedSourceDirectories in Compile <++= baseDirectory { base =>
  Seq(
    base / "target/gen/main"
  )
}
