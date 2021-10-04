package body collada.Library
is

   function find_in (Self : Inputs;   the_Semantic : in library.Semantic) return Input_t
   is
   begin
      for i in Self'Range
      loop
         if Self (i).Semantic = the_Semantic
         then
            return Self (i);
         end if;
      end loop;

      return null_Input;
   end find_in;


end collada.Library;
