with
     lace.Environ.Paths,
     ada.Text_IO;

procedure test_Environ_Paths
is
   use lace.Environ.Paths,
       ada.Text_IO;

   Error : exception;
begin
   put_Line ("Begin");

   -- Test load of an empty file.
   --
   declare
      Output : constant String := to_File ("data/empty.txt").load;
   begin
      if Output /= ""
      then
         raise Error with "Loading an empty file fails: '" & Output & "'";
      end if;
   end;

   -- Test load of simple text.
   --
   declare
      Output : constant String := to_File ("data/digits.txt").load;
   begin
      if Output /= "0123456789"
      then
         raise Error with "Loading a simple text file fails: '" & Output & "'";
      end if;
   end;

   put_Line ("Success");
   put_Line ("End");
end test_Environ_Paths;
