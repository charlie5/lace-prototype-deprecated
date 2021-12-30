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
            Self.Name := to_unbounded_String (Name);
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
   begin
      Deferred.destroy (Deferred.item (Self));   -- Destroy base classes.
      Subject .destroy (Subject .item (Self));
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
      return to_String (Self.Name);
   end Name;

end lace.Subject_and_deferred_Observer;
