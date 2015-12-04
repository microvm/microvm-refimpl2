# The C binding of the Mu reference implementation

This directory contains the C binding so you can write the Mu client in C.  If
You write the client in a JVM language, you don't need this binding since the
reference implementation is already implemented on JVM.

## Building

Make sure you build the micro VM. Go to the parent directory and type `sbt
compile`. Then come here and type `make JAVA_HOME=/path/to/the/java/home`.

This will produce the `libmurefimpl2start.so` dynamic library that contains code
that starts the JVM and creates the MuVM instance for your client written in C.
This library will **hard code the classpath and the JVM path** into the shared
object, so it only works for this particular `microvm-refimpl2` repository you
cloned.

## Usage

The `refimpl2-config` script generates the necessary compiler and linker flags
for you. 

But you should think first: Does my client start the Mu VM, or some other
program starts the VM for me?

### Your program starts Mu

You write the client in C, and it starts the JVM and creates a micro VM
instance.

The `libmurefimpl2start.so` library contains functions (defined in
`refimpl2-start.h`) that starts the JVM and create the Mu instance for you.
Your client invokes the `mu_refimpl2_new()` function which returns a `MuVM*`.
After using, call `mu_refimpl2_close` to close it. The `mu_refimpl2_new_ex`
function provides more options.

Use the `refimpl2-config` script with the `--istart` flag to indicate your
program will create the Mu reference implementation instance. Such clients need
to link against `libmurefimpl2start.so`.

For example:

```
cc `/path/to/refimpl2-config --istart --cflags --libs` -o my_client my_client.c
```

### Other program starts Mu

You write the client in C, but some other program starts the Mu micro VM and
gives your client a pointer to the `MuVM` struct. In this case, you don't know
how the micro VM is created. You only depend on the implementation-neutral API.

In this case, all your program need is the `muapi.h` header. Use
`refimpl2-config` without the `--istart` flag will only generate the inclusion
path.

As another example, you wrote a Scala program which you call a "Mu loader". The
program creates a `MicroVM` instance, then dynamically loads your client from a
".so" file, calls one of its functions and passes the `MuVM*` pointer to it. You
can write your client like this:

```
void my_entry(MuVM* mvm) {
    ...
}
```

And compile it to a dynamic library by:

```
cc `/path/to/refimpl2-config --cflags` -fPIC -shared -o libmyclient.so my_client.c
```

## Implementation details

The `MuVM` struct has an extra non-standard function `execute()`. See
[../README.md](../README.md) for more details.

<!--
vim: tw=80
-->
