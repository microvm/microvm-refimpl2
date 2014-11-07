MicroVM Reference Implementation 2
==================================

**WORKING IN PROGRESS.** This project will be the next reference implementation
of MicroVM. It is based on the
[simplest-microvm-project](https://github.com/microvm/simplest-microvm-project).
This project will implement the currently MicroVM spec in
[microvm-spec](https://github.com/microvm/microvm-spec/wiki)

[microvm-refimpl](https://github.com/microvm-project/microvm-refimpl) is the
previous reference implementation.

How to compile
--------------

You need [Scala](http://scala-lang.org/) 2.11 and
[sbt](http://www.scala-sbt.org/) 0.13. It is recommended to install them using
the package manager of your operating system or distribution (including apt-get,
yum, pacman, etc. for GNU/Linux distributions and Homebrew for Mac OS X).

To download all dependencies from the Maven central repository, invoke `sbt
update`.

To generate the MicroVM IR parser from its Antlr grammar, invoke `sbt
antlr4:antlr4Generate`. The generated sources will be in
`target/scala-2.11/src_managed`. Make sure your IDE can see those generated
sources.

To compile, invoke `sbt compile` or do this in your favourite IDE.

To generate an Eclipse project, install the [sbt-eclipse
plugin](https://github.com/typesafehub/sbteclipse) and invoke `sbt eclipse`.

IntelliJ IDEA has plugins for Scala and SBT. Make sure you don't commit `.idea`
or generated project files into the repository.

Author and Copyright
--------------------

This project is created by Kunshan Wang, Yi Lin, Steve Blackburn, Antony
Hosking, Michael Norrish.

This project is released under the CC-BY-SA license. See `LICENSE`.

Contact
-------

Kunshan Wang <kunshan.wang@anu.edu.au>

<!--
vim: tw=80
-->
