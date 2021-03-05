with
     lace.Environ,
     ada.Text_IO;

procedure test_Environ_compression
is
   use lace.Environ,
       ada.Text_IO;

   test_Error  : exception;
   digits_Text : constant String := "0123456789";
begin
   put_Line ("Begin");

   verify_Folder ("tmp");
   goto_Folder   ("tmp");


   --- Compress single files.
   --
   save       (digits_Text, "digits.txt-original");
   copy_File  ("digits.txt-original", "digits.txt");

   for Each in compress_Format
   loop
      compress   ("digits.txt", Each);
      rid_File   ("digits.txt");
      decompress ("digits.txt" & format_Suffix (Each));

      if load ("digits.txt") /= digits_Text
      then
         raise test_Error with "'" & load ("digits.txt") & "'";
      end if;

      rid_File ("digits.txt" & format_Suffix (Each));
   end loop;


   --- Compress directories.
   --
   verify_Folder ("archive-original");
   move_Files    ("*",                "archive-original");
   copy_Folder   ("archive-original", "archive");

   for Each in folder_compress_Format
   loop
      compress   ("archive", Each);
      rid_Folder ("archive");
      decompress ("archive" & format_Suffix (Each));

      if   load ("archive/digits.txt")
        /= load ("archive-original/digits.txt")
      then
         raise test_Error with "'" & load ("archive/digits.txt") & "'";
      end if;

      rid_File ("archive" & format_Suffix (Each));
   end loop;


   --- Tidy up
   --
   goto_Folder ("..");
   rid_Folder  ("tmp");

   put_Line ("Success");
end test_Environ_compression;
