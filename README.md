Mu Reference Implementation version 2
=====================================

This project is the current reference implementation of Mu, the micro virtual
machine designed by [The Micro Virtual Machine Project](http://microvm.org).

Version 2.1.x implements the [master branch of the Mu
Specification](https://github.com/microvm/microvm-spec/tree/goto-with-values)
which uses the goto-with-values form.

This project is based on the previous works of
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
* Install [Scala IDE](http://scala-ide.org/) 4.x (Eclipse with pre-installed
  plugins for Scala).
* Clone this repository:

```bash
git clone git@github.com:microvm/microvm-refimpl2.git
```

* In the directory `microvm-refimpl2`, do the following:

```bash
sbt update genSrc eclipse
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

To generate the Mu IR parser from the Antlr grammar, invoke `sbt genSrc`. The
generated sources will be in the `target/scala-2.11/src_managed` directory.

To generate an Eclipse project, install the [sbt-eclipse
plugin](https://github.com/typesafehub/sbteclipse) and invoke `sbt eclipse`.
Make sure you generate the parser before creating the Eclipse project, so that
the generated sources will be on the Eclipse build path.

To compile, invoke `sbt compile`. This will also generate the Mu IR parser using
Antlr.

IntelliJ IDEA has plugins for Scala and SBT. Make sure you don't commit `.idea`
or generated project files into the repository.

How to run
----------

There is a sample factorial program (generously provided by @johnjiabinzhang) in
the `src/test` directory. To run the program with all dependencies on the
classpath, you need to run it with sbt. Invoke `sbt` to enter the interactive
shell. Then type:

```
set fork := true
test:runMain junks.FactorialFromRPython
```

or directly from the command line:

```
sbt 'set fork:=true' 'test:runMain junks.FactorialFromRPython'
```

`fork := true` tells sbt to run the program in a different process than the one
running sbt itself.

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
