package body lace.Observer.deferred
is

   package body Forge
   is
      function to_Observer (Name : in Event.observer_Name) return Item
      is
      begin
         return Self : constant Item := (Deferred.item
                                         with name => to_unbounded_String (Name))
         do
            null;
         end return;
      end to_Observer;


      function new_Observer (Name : in Event.observer_Name) return View
      is
         Self : constant View := new Item' (to_Observer (Name));
      begin
         return Self;
      end new_Observer;

   end Forge;


   overriding
   function Name (Self : in Item) return Event.observer_Name
   is
   begin
      return to_String (Self.Name);
   end Name;

end lace.Observer.deferred;
