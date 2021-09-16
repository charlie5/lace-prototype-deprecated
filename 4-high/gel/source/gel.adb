package body MMI
is

   function to_Asset (Self : in String) return asset_Name
   is
      the_Name : String (asset_Name'Range);
   begin
      the_Name (1               .. Self'Length)   := Self;
      the_Name (Self'Length + 1 .. the_Name'Last) := (others => ' ');

      return asset_Name (the_Name);
   end to_Asset;



   function to_String (Self : in asset_Name) return String
   is
   begin
      for Each in reverse Self'Range
      loop
         if Self (Each) /= ' '
         then
            return String (Self (1 .. Each));
         end if;
      end loop;

      return "";
   end to_String;

end MMI;
