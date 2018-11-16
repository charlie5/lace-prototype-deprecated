with lace.fast_Pool;


package body impact.d3.solver_Constraint
is


   package Pool is new lace.fast_Pool (Item, View,  20_000);




   function  new_solver_Constraint return View
   is
      Self : constant View := Pool.new_Item;
   begin
--        Self.all := null_Constraint;
      return Self;
   end new_solver_Constraint;


   procedure free (Self : in out View)
   is
   begin
      Pool.free (Self);
   end free;

end impact.d3.solver_Constraint;
