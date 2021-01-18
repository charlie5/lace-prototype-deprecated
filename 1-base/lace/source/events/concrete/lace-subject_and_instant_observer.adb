package body lace.Subject_and_instant_Observer
is

   package body Forge
   is
      function to_Subject_and_Observer (Name : in String) return Item
      is
      begin
         return Self : Item
         do
            Self.Name := to_unbounded_String (Name);
         end return;
      end to_Subject_and_Observer;
   end Forge;


   overriding
   function Name (Self : in Item) return String
   is
   begin
      return to_String (Self.Name);
   end Name;

end lace.Subject_and_instant_Observer;
