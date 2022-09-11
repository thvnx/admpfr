# Admpfr for the C user

This section describes the main differences between the use of MPFR in C and
these Ada bindings:

- The `mpfr_t` equivalent in these bindings is the `Mpfloat` type.
- You don't have to allocate/free memory, `Mpfloat` is a controlled type, which
means that initialization (`mpfr_init`) and finalization (`mpfr_clear`) is
automatically handled by the language. So declaring `A : Mpfloat;` is enough to
automatically init the `mpfr_t` object. It is cleared as soon as the scope of A
is reached out.
- `Admpfr` uses the same names introduced by MPFR, except that the `mpfr_`
prefix is removed in Ada, i.e. `mpfr_add` is callable by `Admpfr.Add`, or just
`Add` in Ada. `long` (`mpfr_add_si`), or `double` (`mpfr_add_d`) variants are
also just named `Add`. `unsigned long` variants are not supported since Ada
requires that all predefined types be symmetric around zero (excepting an extra
negative value). `mpfr_*_ui` functions are supported anyway if no `mpfr_*_si`
equivalent is available (but an exception is raised if a negative value is
used).
- All rounding modes are supported, `MPFR_RNDN` is called `RNDN` (the Ada
corresponding type is `Admpfr.Rouding`), and so on for the other modes.
- Rounding mode parameter is optional when using the Admpfr functions and
procedures (the default value is set by `mpfr_get_default_rounding_mode`).
- `A'Image` (with `A` a `Mpfloat` number) can be used to get the string
representation of `A`. It can't be configured and simply mimics the behavior of
`mpfr_printf("%.RNe", A)`.
- `mpz_t`, `mpq_t`, and `mpf_t` based C functions are not supported.
- Ternary values returned by the C functions are embedded in the `Mpfloat`
type. Use `Get_Ternary_Value (X)` to get it (see `Ternary_Value` enumeration).

If not explicitly stated below, all C functions are supported by Admpfr.

## Initialization functions

- `mpfr_init*` and `mpfr_clear`* functions are useless in these Ada bindings,
memory is automatically handled. Precision can be set at initialization: `A :
Mpfloat (53);` is equivalent to `mpfr_t A; mpfr_init2 (A, 53);`.

## Assignment functions

All assignment functions are name set, here are the supported ones:

| C                  | Ada procedure                                                                      |
|--------------------|------------------------------------------------------------------------------------|
| `mpfr_set`         | `Set (Rop : out Mpfloat; Op : Mpfloat; Rnd : Rounding := RNDEF)`                    |
| `mpfr_set_si`      | `Set (Rop : out Mpfloat; Op : Long_Integer; Rnd : Rounding := RNDEF)`               |
| `mpfr_set_f`       | `Set (Rop : out Mpfloat; Op : Float; Rnd : Rounding := RNDEF)`                      |
| `mpfr_set_d`       | `Set (Rop : out Mpfloat; Op : Long_Float; Rnd : Rounding := RNDEF)`                 |
| `mpfr_set_ld`      | `Set (Rop : out Mpfloat; Op : Long_Long_Float; Rnd : Rounding := RNDEF)`            |
| `mpfr_set_si_2exp` | `Set (Rop : out Mpfloat; Op : Long_Integer; E : Exponent; Rnd : Rounding := RNDEF)` |
| `mpfr_strtofr`     | `Set (Rop : out Mpfloat; Op : String; B : Base := 10; Rnd : Rounding := RNDEF)`     |
| `mpfr_set_nan`     | `Set_Nan (Rop : out Mpfloat)`                                                      |
| `mpfr_set_inf`     | `Set_Inf (Rop : out Mpfloat; S : Sign)`                                            |
| `mpfr_set_zero`    | `Set_Zero (Rop : out Mpfloat; S : Sign)`                                           |
| `mpfr_swap`        | `Swap (X, Y : in out Mpfloat)`                                                     |

## Combined Initialization and Assignment Functions

None are supported, see the two sections above.

## Conversion Functions

- `Get` functions follow the same scheme as described
[here](#assignment-functions). The floating-point variants will raise an
exception if the value to get is a NaN or infinity.
- `mpfr_get_str` is replaced by the Ada function `To_String`, see documentation
in `src/admpfr.ads`.
- Only `mpfr_fits_slong_p` and `mpfr_fits_sint_p` are supported:
`Fits_Long_Integer` are `Fits_Integer` are the Ada equivalents.

## Arithmetic Functions

- Ada arithmetic operators are not overloaded for `Mpfloat` type since the
operations take arguments, such as the precision of the result.
- `Sum` and `Dot` operations use an array of `Mpfloat` as operand arguments, see
`Mpfloat_Array` type.

## Comparison Functions

- All comparison functions are supported. Operators are also defined but their
use is discouraged since `"/="` can return a wrong result is one operand is a
NaN (in Ada `"/="` is just the negation of `"="`).

## Transcendental Functions

- Nothing to report.

## Input and Output Functions

- Nothing to report.

## Formatted Output Functions

- Only `mpfr_printf` and `mpfr_sprintf` are (partially, no variadic support)
supported.

## Integer and Remainder Related Functions

- Nothing to report.

## Rounding-Related Functions

- `mpfr_min_prec` can't return 0, it raises an exception instead.
- `mpfr_print_rnd_mode` is not implented. One just simply use the `'Image`
attribute of the `Rounding` mode enumeration.
- The `mpfr_round_nearest_away` is not implemented.

## Miscellaneous Functions

- All the `mpfr_*random` functions are not implemented.
- `MPFR_VERSION*` macros are not implemeted.
- `mpfr_get_patches`, `mpfr_buildopt*` are not implemented.

## Exception Related Functions

- `Set_Emin`/`Set_Emax` raise an exception if emin > emax.
- Flags mask are Ada arrays.