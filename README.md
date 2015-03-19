Mu Reference Implementation version 2
=====================================

This project is the current reference implementation of Mu, the micro virtual
machine designed by [The Micro Virtual Machine Project](http://microvm.org). It
implements the [Mu Specification version
2](https://github.com/microvm/microvm-spec/wiki)

This project is based on the
[simplest-microvm-project](https://github.com/microvm/simplest-microvm-project).
[microvm-refimpl](https://github.com/microvm-project/microvm-refimpl) is the
previous reference implementation.

How to compile
--------------

**For the impatient**:

* If you use Mac, install [Homebrew](http://brew.sh/).
* Install [Scala](http://scala-lang.org/) 2.11. If you use Mac and Homebrew,
  `brew install scala`.
* Install [sbt](http://www.scala-sbt.org/) 0.13. If you use Mac and Homebrew,
  `brew install sbt`.
* Install [Scala IDE](http://scala-ide.org/) 4.0 (Eclipse with pre-installed
  plugins for Scala).
* Clone this repository:

```bash
git clone git@github.com:microvm/microvm-refimpl2.git
```

* In the directory `microvm-refimpl2`, do the following:

```bash
sbt update
sbt antlr4:antlr4Generate
sbt eclipse
```

* Open Scala IDE and import the generated project as "existing project into
  workspace".

**Detailed guide**:

You need [Scala](http://scala-lang.org/) 2.11 and
[sbt](http://www.scala-sbt.org/) 0.13. It is recommended to install them using
the package manager of your operating system or distribution (including apt-get,
yum, pacman, etc. for GNU/Linux distributions and Homebrew for Mac OS X).

To download all dependencies from the Maven central repository, invoke `sbt
update`.

To generate the Mu IR parser from its Antlr grammar, invoke `sbt
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
