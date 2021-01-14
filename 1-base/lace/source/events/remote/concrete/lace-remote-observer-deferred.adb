package body lace.remote.Observer.deferred
is

   package body Forge
   is
      function to_Observer (Name : in observer_Name) return Item
      is
      begin
         return Self : constant Item := (Deferred.item
                                         with name => new observer_Name'(Name))
         do
            null;
         end return;
      end to_Observer;


      function new_Observer (Name : in observer_Name) return View
      is
         Self : constant View := new Item' (to_Observer (Name));
      begin
         return Self;
      end new_Observer;
   end Forge;


   overriding
   function Name (Self : in Item) return observer_Name
   is
   begin
      return Self.Name.all;
   end Name;

end lace.remote.Observer.deferred;



