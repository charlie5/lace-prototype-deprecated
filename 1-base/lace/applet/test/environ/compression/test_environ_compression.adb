with
     lace.Environ,
     ada.Text_IO;


procedure test_Environ_compression
--
-- Displays a string message in a Pure unit.
--
is
   use lace.Environ,
       ada.Text_IO;

   test_Error  : exception;
   digits_Text : constant String := "0123456789";
begin
   put_Line ("Begin");

   verify_Folder ("tmp");
   goto_Folder   ("tmp");

   save       (digits_Text, "digits.txt-original");
   copy_File  ("digits.txt-original", "digits.txt");

   for Each in Format
   loop
      compress   ("digits.txt", Each);
      rid_File   ("digits.txt");
      decompress ("digits.txt" & format_Suffix (Each));

      if load ("digits.txt") /= digits_Text
      then
         raise test_Error with "'" & load ("digits.txt") & "'";
      end if;
   end loop;

   goto_Folder ("..");
   rid_Folder  ("tmp");

   put_Line ("Success");
end test_Environ_compression;
