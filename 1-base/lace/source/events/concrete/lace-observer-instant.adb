package body lace.Observer.instant
is
   package body Forge
   is
      function new_Observer (Name : in Event.observer_Name) return View
      is
         Self : constant View := new Item;
      begin
         Self.Name := to_unbounded_String (Name);
         return Self;
      end new_Observer;

   end Forge;


   overriding
   function Name (Self : in Item) return Event.observer_Name
   is
   begin
      return to_String (Self.Name);
   end Name;

end lace.Observer.instant;
