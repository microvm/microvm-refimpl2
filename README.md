Simplest MicroVM Project
========================

**This is an experimental project.** If you want to try out the MicroVM reference implementation, please visit the [microvm-refimpl](https://github.com/microvm-project/microvm-refimpl) project.

This project aims to be a much simpler (in term of conciseness) MicroVM implementation than the microvm-refimpl project. It is written in the Scala programming language.


How to compile
--------------

You need [Scala](http://scala-lang.org/) 2.11 and [sbt](http://www.scala-sbt.org/) 0.13. It is recommended to install them using the package manager of your operating system or distribution (including apt-get, yum, pacman, etc. for GNU/Linux distributions and Homebrew for Mac OS X).

To download all dependencies from the Maven central repository, invoke `sbt update`.

To generate the MicroVM IR parser from its Antlr grammar, invoke `sbt antlr4:antlr4Generate`.

To compile, invoke `sbt compile` or do this in your favourite IDE.

To generate an Eclipse project, install the [sbt-eclipse plugin](https://github.com/typesafehub/sbteclipse) and invoke `sbt eclipse`.

Author and Copyright
--------------------

This project is created by Kunshan Wang, Yi Lin, Steve Blackburn, Antony
Hosking, Michael Norrish.

This project is released under the CC-BY-SA license. See `LICENSE`.

Contact
-------

Kunshan Wang <kunshan.wang@anu.edu.au>