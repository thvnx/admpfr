with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Admpfr;         use Admpfr;

procedure Test is
   M   : Mpfloat;
   LLF : Long_Long_Float;
   LI  : Long_Integer;
begin
   Put_Line (Long_Long_Float'Digits'Image);
   --  This test assumes the 80-bit extended floating-point format is available
   --  (most x86 CPUs but not Apple ones). Should print 18.

   M.Set ("-0");

   LLF := M.Get_Long_Long_Float;
   Put_Line (LLF'Image);
   LLF := M.Get_Long_Long_Float (LI);
   Put_Line (LLF'Image & " " & LI'Image);

   M.Set_Inf (Pos);

   begin
      LLF := M.Get_Long_Long_Float;
   exception
      when F : Failure =>
         Put_Line (Exception_Message (F));
   end;
   begin
      LLF := M.Get_Long_Long_Float (LI);
   exception
      when F : Failure =>
         Put_Line (Exception_Message (F));
   end;

   M.Set_Nan;

   begin
      LLF := M.Get_Long_Long_Float;
   exception
      when F : Failure =>
         Put_Line (Exception_Message (F));
   end;
   begin
      LLF := M.Get_Long_Long_Float (LI);
   exception
      when F : Failure =>
         Put_Line (Exception_Message (F));
   end;

   M.Set ("1234e567");

   begin
      LLF := M.Get_Long_Long_Float;
      Put_Line (LLF'Image);
   exception
      when F : Failure =>
         Put_Line (Exception_Message (F));
   end;
   begin
      LLF := M.Get_Long_Long_Float (LI);
      Put_Line (LLF'Image & " " & LI'Image);
   exception
      when F : Failure =>
         Put_Line (Exception_Message (F));
   end;

end Test;
