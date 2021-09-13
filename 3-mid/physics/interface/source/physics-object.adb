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

      procedure set (To : in Matrix_4x4)
      is
      begin
         Dynamics := To;
      end set;

      function get return Matrix_4x4
      is
      begin
         return Dynamics;
      end get;

      procedure set_Spin (To : in Matrix_3x3)
      is
         use linear_Algebra_3D;
      begin
         set_Rotation (Dynamics, To);
      end set_Spin;

      function get_Spin return Matrix_3x3
      is
         use linear_Algebra_3D;
      begin
         return get_Rotation (Dynamics);
      end get_Spin;

      procedure set_Site (To : in Vector_3)
      is
         use linear_Algebra_3D;
      begin
         set_Translation (Dynamics, To);
      end set_Site;

      function get_Site return Vector_3
      is
         use linear_Algebra_3D;
      begin
         return get_Translation (Dynamics);
      end get_Site;

   end safe_Dynamics;


end physics.Object;
