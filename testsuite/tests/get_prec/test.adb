with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Admpfr;         use Admpfr;

procedure Test is
   M : Mpfloat;
   N : Mpfloat (1);
begin
   Set (M, "1");
   Put_Line (Get_Prec (M)'Image);

   Put_Line (Get_Prec (N)'Image);

   begin
      declare
         O : Mpfloat (Precision'Last);
         --  16#7FFF_FFFF_FFFF_FEFF# if Precision is 8 bytes (unix)
      begin
         Put_Line (Get_Prec (O)'Image);
      end;
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
end Test;
