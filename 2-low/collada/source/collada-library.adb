package body collada.Library
is

   -----------
   --- Utility
   --

   function "+" (From : in ada.Strings.unbounded.unbounded_String) return String
     renames ada.Strings.unbounded.to_String;


   ---------
   --- Input
   --

   function find_in (Self : Inputs;   the_Semantic : in library.Semantic) return Input_t
   is
   begin
      for Each in Self'Range
      loop
         if Self (Each).Semantic = the_Semantic
         then
            return Self (Each);
         end if;
      end loop;

      return null_Input;
   end find_in;


end collada.Library;
