with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Admpfr;           use Admpfr;

procedure Divworst is

   function Eval
     (X, Y, Z, T : in out Mpfloat;
      Rnd        : Rounding) return Long_Integer;

   function Eval
     (X, Y, Z, T : in out Mpfloat;
      Rnd        : Rounding) return Long_Integer is
   begin
      T.Div (X, Y, Rnd);
      Z.Set (T, Rnd);
      Z.Rint_Floor (Z, Rnd);
      return Get_Long_Integer (Z, Rnd);
   end Eval;

   Dmax       : Exponent;
   N, P       : Precision;
   X, Y, Z, T : Mpfloat;

begin

   if Argument_Count /= 2 and then Argument_Count /= 3 then
      Put_Line (Standard_Error, "Usage: divworst <dmax> <n> [<p>]");
      raise Constraint_Error;
   end if;

   Dmax := Exponent'Value (Argument (1));
   N := Precision'Value (Argument (2));
   P := (if Argument_Count = 2 then N else Precision'Value (Argument (3)));

   if P < N then
      Put_Line (Standard_Error,
                "divworst: p must be greater or equal to n");
      raise Constraint_Error;
   end if;

   X.Set_Prec (N);
   Y.Set_Prec (N);
   Z.Set_Prec (N);
   T.Set_Prec (P);

   X.Set (1, -1);
   while X.Get_Exp <= Dmax loop
      X.Nextabove;

      Y.Set (1, -1);
      while Y.Get_Exp = 0 loop
         Y.Nextabove;

         declare
            Rz, Rn : Long_Integer;
         begin
            Z.Sub (X, Y, RNDZ);

            if Z.Get_Ternary_Value = EXACT then
               Rz := Eval (X, Y, Z, T, RNDZ);
               Rn := Eval (X, Y, Z, T, RNDN);
               if Rz /= Rn then
                  Printf ("X = %." & Precision (N - 1)'Image & "Rb ; ", X);
                  Printf ("Y = %." & Precision (N - 1)'Image & "Rb ; ", Z);
                  Put_Line ("Z: " & Rz'Image & " N: " & Rn'Image);
               end if;
            end if;
         end;
      end loop;
   end loop;

end Divworst;