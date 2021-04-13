with
     lace.Environ,
     ada.Text_IO;

procedure test_Environ_general
is
   use lace.Environ,
       ada.Text_IO;

   Error : exception;
begin
   put_Line ("Begin");

   -- Test GLOB expansion.
   --
   declare
      Output : constant String := expand_GLOB ("data/*.txt");
   begin
      if Output /= "data/glob1.txt data/glob2.txt data/glob3.txt"
      then
         raise Error with "expand_GLOB fails: '" & Output & "'";
      end if;
   end;

   put_Line ("Success");
   put_Line ("End");
end test_Environ_general;
