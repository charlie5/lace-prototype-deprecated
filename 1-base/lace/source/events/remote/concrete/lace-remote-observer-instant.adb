package body lace.remote.Observer.instant
is
   package body Forge
   is
      function new_Observer (Name : in observer_Name) return View
      is
         Self : constant View := new Item;
      begin
         Self.Name := new observer_Name' (Name);
         return Self;
      end new_Observer;

   end Forge;


   overriding
   function Name (Self : in Item) return observer_Name
   is
   begin
      return Self.Name.all;
   end Name;

end lace.remote.Observer.instant;
