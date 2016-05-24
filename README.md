# Mu Reference Implementation version 2

[![Build Status](https://travis-ci.org/microvm/microvm-refimpl2.svg?branch=master)](https://travis-ci.org/microvm/microvm-refimpl2)

Version 2.2.0

This project is the current reference implementation of Mu, the micro virtual
machine designed by [The Micro Virtual Machine Project](http://microvm.org).

Version 2.2.0 implements the [Mu Specification with experimental threadlocal
object references](https://github.com/microvm/microvm-spec/tree/issue52-threadlocal).

This project is based on the previous works of
[simplest-microvm-project](https://github.com/microvm/simplest-microvm-project).
[microvm-refimpl](https://github.com/microvm-project/microvm-refimpl) is the
previous reference implementation.

## How to compile

**For the impatient**:

* Install JDK 8. If you use Mac, download from
  [Oracle](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html).
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

The reference implementation is developed and tested with Java VM 8. You need a
JRE to build the Scala/Java part, and a JDK to build the C binding.

You also need [Scala](http://scala-lang.org/) 2.11 and
[sbt](http://www.scala-sbt.org/) 0.13. It is recommended to install them using
the package manager of your operating system or distribution (such as apt-get,
yum, pacman, etc. for GNU/Linux distributions and Homebrew for Mac OS X) if such
packages are available.

For Ubuntu users: Ubuntu 15.10 does not provide sbt in its repository. Please
[download sbt](http://www.scala-sbt.org/download.html) from the official sbt web
site, or follow the [official sbt installing guide for
Linux](http://www.scala-sbt.org/0.13/tutorial/Installing-sbt-on-Linux.html).  If
you experience any "certificate" problems, [this
page](https://github.com/sbt/sbt/issues/2295) provides a solution.

Then after cloning this repository, you can simply invoke `sbt compile` to
compile this project. Or you can do it step by step:

* To download all dependencies from the Maven central repository, invoke `sbt
  update`.

* To generate the Mu IR parser from the Antlr grammar, invoke `sbt genSrc`. The
  generated sources will be in the `target/scala-2.11/src_managed` directory.

* To compile, invoke `sbt compile`. This will also generate the Mu IR parser
  using Antlr.

To generate an Eclipse project, install the [sbt-eclipse
plugin](https://github.com/typesafehub/sbteclipse) and invoke `sbt eclipse`.
Make sure you generate the parser (`sbt genSrc`) before creating the Eclipse
project, so that the generated sources will be on the Eclipse build path.

IntelliJ IDEA has plugins for Scala and SBT. Make sure you don't commit `.idea`
or generated project files into the repository.

### C binding and Python binding

The C binding is in the `cbinding` directory. Just run `make` inside `cbinding`.

The Python binding is in the `pythonbinding` directory. It depends on the C
binding, so make sure you make the C binding first. The Python binding does not
need to be built.

See the `README.md` files in `cbinding` and `pythonbinding` for more details.

## How to run

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

## Implementation details

This reference implementation aims to be easy to work with, but does not have
high performance. It may be used by client writers to evaluate the Mu micro VM,
and may also be used by Mu micro VM implementers as a reference to compare with.

The micro VM is implemented as an interpreter written in Scala. The main class
is `uvm.refimpl.MicroVM`, which implements the `MuVM` struct specified by the
[client
API](https://github.com/microvm/microvm-spec/blob/master/uvm-client-interface.rest),
but is more Scala-like. The client interacts with the micro VM via
`uvm.refimpl.MuCtx` instances created by the `MicroVM` instance, which
corresponds to the `MuCtx` struct in the spec. `uvm.refimpl.MuValue` and its
subclasses implement the `MuValue` handles, but has a real Scala type hierarchy
and does extra type checking when converting, which is not required by the spec.

The client can also be written in C, Python or other languages that can
interface with C.

### Threading model

It uses green threads to execute multiple Mu threads and uses a round-robin
scheduler: the interpreter iterates over all active threads, executes one
instruction for each active thread, then repeat this process. However, the whole
Scala-based program itself is **not thread safe**. Do not run multiple JVM or
native threads. This means, you can still experiment with concurrent Mu
programs, but there are some corner cases that do not work in this refimpl. For
example:

- Waiting for other Mu threads in the trap handler. The trap handler is executed
  by the same thread executing the Mu IR. During trap handler, no Mu program is
  executed. So if you want to use
  [watchpoints](https://github.com/microvm/microvm-spec/blob/master/instruction-set.rest#traps-and-watchpoints)
  to wait for certain Mu thread to come to a certain rendezvous point (a common
  optimisation trick), you should either wait within Mu IR (not in trap
  handlers) or try the high-performance Mu implementation which is still being
  written.

- Synchronising with concurrent native programs via pointers, atomic memory
  access and futex. This is the realistic way for Mu to synchronise with
  native programs or foreign languages, but this refimpl implements atomic
  memory access as not-atomic (since it uses green thread) and implements futex
  in Scala (since it has its own scheduler).

The MicroVM instance will not start executing unless its `execute()` method is
called. This method is specific to this implementation, and is not defined in
the specification. This also means the *client cannot run concurrently with the
MicroVM*, i.e. once started, the client can only intervene in the execution in
**trap handlers**. So a common use pattern is:

```scala
val microVM = new MicroVM()

val uir = myCompiler.compile(sourceCode)
val ctx = microVM.newContext()
ctx.loadBundle(uir)

microVM.setTrapHandler(theTrapHandler)  // Set the trap handler so the client
                                        // can talk with the VM when trapped.

val stack = ctx.newStack(theMainFunction)
val thread = ctx.newThread(stack, Seq(params, to, the, main, function))

microVM.execute() // The current JVM thread will run on behalf of the MicroVM.
                  // This blocks until all Mu threads stop.
                  // However, MicroVM will call theTrapHandler.
```

Only the text-based IR and HAIL are implemented. The binary-based IR and HAIL
script do not have high priority at this point, because our current focus is to
implement a correct Mu VM and the text-based IR is easier for debugging. IR
parsing is also not yet known as the bottleneck.

### Garbage collection

This reference implementation has an exact tracing garbage collector with a
mark-region small object space and a mark-sweep large object space.

### IR implementation-specific details

- Many undefined behaviours in the specification will raise
  `UvmRuntimeException`, such as division by zero, going below the last frame of
  a stack, accessing a NULL reference, etc. But this behaviour is not
  guaranteed.

- `int<n>` for n = 1 to 64 are implemented. `vec<T n>` is implemented for all T
  that are int, float or double, and all n >= 1. However, only 8, 16, 32, 64-bit
  integers, float, double, `vec<int<32> 4>`, `vec<float 4>` and `vec<double 2>`
  can be loaded or stored from the memory.

- The tagged reference type `tagref64` is fully implemented.

- Out-of-memory errors will terminate the VM rather than letting the Mu IR
  handle such failures via the exception clauses.

### Native interface

This reference implementation assumes it is running on x86-64 on either Linux or
OSX. It implements the [AMD64 Unix Native
Interface](https://github.com/microvm/microvm-spec/blob/master/native-interface-x64-unix.rest)
of the specification. It can call native functions from Mu IR and let native
programs call back to Mu IR. 

It does not support throwing Mu exceptions into native programs, or handing
C++-based exceptions.

## Author and Copyright

This project is created by Kunshan Wang, Yi Lin, Steve Blackburn, Antony
Hosking, Michael Norrish.

This project is released under the CC-BY-SA license. See `LICENSE`.

## Contact

Kunshan Wang <kunshan.wang@anu.edu.au>

<!--
vim: tw=80
-->
