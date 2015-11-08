
package body lace.remote.Observer.deferred
is


   package body Forge
   is

      function to_Observer (Name : in String) return Item
      is
      begin
         return Self : Item := (my_Deferred.item
                                with name => new String' (Name))
         do
            null;
         end return;
      end to_Observer;



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


end lace.remote.Observer.deferred;



