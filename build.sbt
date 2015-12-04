
lazy val genSrc = taskKey[List[File]]("generate sources")

genSrc <<= (sourceGenerators in Compile) { _.join.map(_.flatten.toList) }

lazy val root = (project in file(".")).settings(
    organization := "org.microvm",

    name := "microvm-refimpl2",
    
    version := "2.1.0",

    description := "The second reference implementation of Mu, the micro virtual machine",

    licenses := Seq("CC BY-SA 4.0" -> url("https://creativecommons.org/licenses/by-sa/4.0/legalcode")),

    scalaVersion := "2.11.7",

    libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-reflect" % "2.11.7",
        "org.antlr" % "antlr4" % "4.5.1-1",
        "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
        "ch.qos.logback" % "logback-classic" % "1.1.3",
        "com.github.jnr" % "jnr-ffi" % "2.0.7",
        "com.github.jnr" % "jffi" % "1.2.10",
        "com.github.jnr" % "jnr-posix" % "3.0.23",
        "org.scalatest" %% "scalatest" % "2.2.4" % "test",
        "junit" % "junit" % "4.12" % "test"
    ),

    testOptions in Test += Tests.Argument("-oF"), // print full stack trace when testing
            
    antlr4Settings,

    antlr4PackageName in Antlr4 := Some("uvm.ir.textinput.gen"),

    antlr4GenListener in Antlr4 := false,

    antlr4GenVisitor in Antlr4 := false
  )

lazy val jarList = taskKey[Seq[File]]("list dependency jars")

jarList := {
  val cp: Seq[File] = (dependencyClasspath in Compile).value.files
  cp
}

lazy val makeJarListFile = taskKey[Unit]("write a list of dependency jar paths separated by colons (:) to target/jars.txt")

makeJarListFile := {
  val jl = jarList.value
  val jlStr = jl.mkString(":")
  println("Dependency jars: " + jlStr)
  IO.write(new java.io.File("cbinding/jars.txt"), jlStr)
}
