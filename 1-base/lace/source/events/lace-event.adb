with
     ada.strings.Hash;

package body lace.Event
is
   function Hash (the_Kind : in Kind) return ada.containers.Hash_type
   is
   begin
      return ada.strings.Hash (String (the_Kind));
   end Hash;

end lace.Event;
