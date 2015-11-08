with
     ada.unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;


package body lace.remote.Subject_and_deferred_Observer
is


   package body Forge
   is

      function to_Subject_and_Observer (Name : in String) return Item
      is
      begin
         return Self : Item
         do
            Self.Name := new String' (Name);
         end return;
      end to_Subject_and_Observer;


      function new_Subject_and_Observer (Name : in String) return View
      is
      begin
         return new Item' (to_Subject_and_Observer (Name));
      end new_Subject_and_Observer;

   end Forge;



   overriding
   procedure destroy (Self : in out Item)
   is
      type String_view is access all String;
      procedure deallocate is new ada.unchecked_Deallocation (String, String_view);

      the_Name : String_view := Self.Name;
   begin
      Deferred.destroy (Deferred.item (Self));
      Subject .destroy (Subject .item (Self));

--        put_Line ("Deallocating " & the_Name.all);
      deallocate (the_Name);
   end destroy;



   overriding
   function Name (Self : in Item) return String
   is
   begin
      return Self.Name.all;
   end Name;


end lace.remote.Subject_and_deferred_Observer;
