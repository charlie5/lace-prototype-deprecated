
package body lace.Observer.instant
is


   package body Forge
   is

      function new_Observer (Name : in String) return View
      is
         Self : constant View := new Item;
      begin
         Self.Name := new String' (Name);
         return Self;
      end new_Observer;

   end Forge;



   overriding
   function Name (Self : in Item) return String
   is
   begin
      return Self.Name.all;
   end Name;


end lace.Observer.instant;
