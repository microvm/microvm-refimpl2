resolvers += "simplytyped.com" at "http://simplytyped.com/repo/releases"

// addSbtPlugin("com.simplytyped" % "sbt-antlr4" % "0.7.2")

lazy val root = (project in file(".")).dependsOn(sbtAntlr4Plugin)

lazy val sbtAntlr4Plugin = uri("https://github.com/ihji/sbt-antlr4.git")

