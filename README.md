# Ada bindings for MPFR

This package introduces a new type: `Mpfloat`, allowing to use `mpfr_t` C
objects in Ada code. `Mpfloat` is a `Limited_Controlled` type, therefore memory
initialization and freeing is automatically handled by Admpfr, thanks to the
`Initialize` and `Finalize` operations given by the controlled type.

Only setting and printing operations have been implemented so far, but a
complete binding to the C library will follow.

## Example:

The following code:
```ada
with Ada.Text_Io; use Ada.Text_Io;
with Admpfr;      use Admpfr;

procedure Main is
   N : Mpfloat;
begin
   N.Set ("0.1");
   Put_Line (N.To_String);
end Main;
```
will print:
```
1.0000000000000001e-01
```
