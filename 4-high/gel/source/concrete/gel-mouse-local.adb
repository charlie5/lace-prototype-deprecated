with
     ada.unchecked_Deallocation;

package body gel.Mouse.local
is

   package body Forge
   is
      function to_Mouse (of_Name : in String) return Item
      is
      begin
         return Self : constant Item := (lace.Subject.local.Forge.to_Subject (of_Name)
                                         with null record)
         do
            null;
         end return;
      end to_Mouse;



      function new_Mouse (of_Name : in String) return View
      is
      begin
         return new Item' (to_Mouse (of_Name));
      end new_Mouse;

   end Forge;



   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destroy;
      deallocate (Self);
   end free;


end gel.Mouse.local;
