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


   protected
   body safe_Dynamics
   is
      procedure set (To : in Dynamics)
      is
      begin
         Value := To;
      end set;

      function  get   return Dynamics
      is
      begin
         return Value;
      end get;
   end safe_Dynamics;



end physics.Object;
