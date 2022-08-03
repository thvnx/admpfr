with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;
with Admpfr;         use Admpfr;

procedure Test is
   M   : Mpfloat;
   F   : Float;
   LF  : Long_Float;
   LLF : Long_Long_Float;
   LI  : Long_Integer;
begin
   M.Set ("-0");

   F := M.Get_Float;
   Put_Line (F'Image);
   LF := M.Get_Long_Float;
   Put_Line (LF'Image);
   LLF := M.Get_Long_Long_Float;
   Put_Line (LLF'Image);
   LF := M.Get_Long_Float (LI);
   Put_Line (LF'Image & " " & LI'Image);
   LLF := M.Get_Long_Long_Float (LI);
   Put_Line (LLF'Image & " " & LI'Image);

   M.Set_Inf (Pos);

   begin
      F := M.Get_Float;
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LF := M.Get_Long_Float;
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LLF := M.Get_Long_Long_Float;
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LF := M.Get_Long_Float (LI);
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LLF := M.Get_Long_Long_Float (LI);
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;

   M.Set_Nan;

   begin
      F := M.Get_Float;
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LF := M.Get_Long_Float;
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LLF := M.Get_Long_Long_Float;
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LF := M.Get_Long_Float (LI);
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LLF := M.Get_Long_Long_Float (LI);
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;

   M.Set ("1234e567");

   begin
      F := M.Get_Float;
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LF := M.Get_Long_Float;
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LLF := M.Get_Long_Long_Float;
      Put_Line (LLF'Image);
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LF := M.Get_Long_Float (LI);
      Put_Line (LF'Image & " " & LI'Image);
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;
   begin
      LLF := M.Get_Long_Long_Float (LI);
      Put_Line (LLF'Image & " " & LI'Image);
   exception
      when F : Failure => Put_Line (Exception_Message (F));
   end;

end Test;
