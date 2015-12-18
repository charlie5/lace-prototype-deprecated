with
     ada.Characters.latin_1,
     ada.Strings.unbounded,
     ada.Text_IO;


package body lace.Text.Forge
is

   function to_String (Filename : in forge.Filename) return String
   is
      use ada.Strings.Unbounded,
          ada.Text_IO;

      the_File : ada.Text_IO.File_Type;
      Pad      : unbounded_String;
   begin
      open (the_File, in_File, String (Filename));

      while not end_of_File (the_File)
      loop
         append (Pad,  get_Line (the_File) & ada.Characters.Latin_1.LF);
      end loop;

      close (the_File);

      return to_String (Pad);
   end to_String;



   function to_Text (Filename : in forge.Filename) return Item
   is
   begin
      return to_Text (to_String (Filename));
   end to_Text;


end lace.Text.Forge;
