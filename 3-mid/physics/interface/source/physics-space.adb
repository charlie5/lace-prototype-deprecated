with
     ada.unchecked_Deallocation;

package body physics.Space
is

   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      Self.destruct;
      deallocate (Self);
   end free;

end physics.Space;
