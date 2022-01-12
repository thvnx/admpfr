# Ada bindings for MPFR

This package introduces a new type: `Mpfr_Float`, allowing to use `mpfr_t` C
objects in Ada code. `Mpfr_Float` is a `Limited_Controlled` type, therefore
memory initialization and freeing is automatically handled by AdMPFR, thanks to
the `Initialize` and `Finalize` operations given by the controlled type.

Only setting and printing operations have been implemented so far, but a
complete binding to the C library will follow.

## Example:

The following code:
```ada
with Ada.Text_Io; use Ada.Text_Io;
with AdMPFR;      use AdMPFR;

procedure Main is
   N : Mpfr_Float;
begin
   Set (N, "0.1");
   Put_Line (To_String (N));
end Main;
```
will print:
```
1.0000000000000001e-01
```
