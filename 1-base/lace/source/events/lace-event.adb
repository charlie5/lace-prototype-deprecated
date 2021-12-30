with
     ada.Strings.Hash;


package body lace.Event
is
   function Hash (the_Kind : in Kind) return ada.Containers.Hash_type
   is
   begin
      return ada.Strings.Hash (String (the_Kind));
   end Hash;

end lace.Event;
