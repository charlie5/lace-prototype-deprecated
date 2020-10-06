with
     ada.unchecked_Deallocation;

package body lace.Subject_and_deferred_Observer
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


      function new_Subject_and_Observer (Name : in String) return View
      is
      begin
         return new Item'(to_Subject_and_Observer (Name));
      end new_Subject_and_Observer;
   end Forge;


   overriding
   procedure destroy (Self : in out Item)
   is
      type String_view is access all String;
      procedure deallocate is new ada.unchecked_Deallocation (String, String_view);

      Name : String_view := Self.Name;
   begin
      Deferred.destroy (Deferred.item (Self));
      Subject .destroy (Subject .item (Self));

      deallocate (Name);
   end destroy;


   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;


   overriding
   function Name (Self : in Item) return String
   is
   begin
      return Self.Name.all;
   end Name;

end lace.Subject_and_deferred_Observer;
