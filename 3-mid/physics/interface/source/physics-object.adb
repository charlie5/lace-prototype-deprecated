with
     ada.unchecked_Deallocation;


package body physics.Object
is

   procedure free (Self : in out View)
   is
      procedure deallocate is new ada.unchecked_Deallocation (Item'Class, View);
   begin
      if Self /= null then
         Self.destruct;
      end if;

      deallocate (Self);
   end free;

end physics.Object;
