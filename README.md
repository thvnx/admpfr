![main](https://github.com/thvnx/admpfr/actions/workflows/main.yml/badge.svg)

# Ada bindings for MPFR

This package introduces a new type: `Mpfloat`, allowing to use `mpfr_t` C
objects in Ada code. `Mpfloat` is a `Limited_Controlled` type, therefore memory
initialization and freeing is automatically handled by Admpfr, thanks to the
`Initialize` and `Finalize` operations given by the controlled type.

The C developer can consult [this document](doc/for-the-C-dev.md) if familiar
with the C library.

## Build

First, note that usage of Admpfr is still experimental. The easiest way to try
it is to modify the `examples/main.adb` file.

### Prerequisites

- An Ada 2022 compiler.
- [MPFR 4.1.0](https://www.mpfr.org).
- (Optional) AdaCore's [e3-testsuite](https://github.com/AdaCore/e3-testsuite).

### Using Alire

Simply get [Alire](https://alire.ada.dev/) and run the following commands:

```bash
$ cd admpfr
# Export some variable if MPFR is installed in a custom location
$ export LIBRARY_PATH=/usr/local/lib
$ export C_INCLUDE_PATH=/usr/local/include
$ alr build
```

You can also run the testsuite to ensure everything is properly supported on
your system:

```bash
$ alr exec -- testsuite/testsuite.py
```

or just play around with the examples:

```bash
$ cd examples
$ alr exec -- gprbuild -Pexamples
$ ./obj/main
1.0000000000000001e-01
```

## Example

The following code:
```ada
with Ada.Text_IO; use Ada.Text_IO;
with Admpfr;      use Admpfr;

procedure Main is
   N : Mpfloat;
begin
   N.Set ("0.1");
   Put_Line (N'Image);
end Main;
```
will print:
```
1.0000000000000001e-01
```
