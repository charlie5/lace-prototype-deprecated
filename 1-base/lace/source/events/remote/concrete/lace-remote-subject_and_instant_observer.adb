package body lace.remote.Subject_and_instant_Observer
is

   package body Forge
   is
      function to_Subject_and_Observer (Name : in String) return Item
      is
      begin
         return Self : Item
         do
            Self.Name := new String'(Name);
         end return;
      end to_Subject_and_Observer;

   end Forge;


   overriding
   function Name (Self : in Item) return String
   is
   begin
      return Self.Name.all;
   end Name;

end lace.remote.Subject_and_instant_Observer;
