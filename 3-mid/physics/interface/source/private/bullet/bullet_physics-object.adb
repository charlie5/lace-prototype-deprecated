with
     bullet_c.Binding,

     bullet_physics.Shape,

     c_math_c.Conversion,
     c_math_c.Vector_3,
     c_math_c.Matrix_3x3,
     c_math_c.Matrix_4x4,

     Swig,
     interfaces.C,

     ada.unchecked_Deallocation,
     ada.Unchecked_Conversion,
     ada.Text_IO;

package body bullet_Physics.Object
is
   use bullet_c.Binding,
       c_math_c.Conversion,
       ada.Text_IO;

   type Any_limited_view is access all lace.Any.limited_item'Class;


   function new_Object (Shape        : in physics.Shape.view;
                        Mass         : in Real;
                        Friction     : in Real;
                        Restitution  : in Real;
                        at_Site      : in Vector_3) return View
                        --  is_Kinematic : in Boolean) return View
   is
      Self : constant View := new Item;
   begin
      Self.define (Shape       => Shape,
                   Mass        => Mass,
                   Friction    => Friction,
                   Restitution => Restitution,
                   at_Site     => at_Site);
      return Self;
   end new_Object;


   overriding
   procedure define (Self : access Item;   Shape       : in physics.Shape.view;
                                           Mass        : in Real;
                                           Friction    : in Real;
                                           Restitution : in Real;
                                           at_Site     : in Vector_3)
   is
      use interfaces.C;
      function to_void_ptr is new ada.unchecked_Conversion (Any_limited_view, Swig.void_ptr);

   begin
      Self.C := b3d_new_Object (c_math_c.Real (Mass),
                                bullet_physics.Shape.view (Shape).C,
                                is_Kinematic => Boolean'Pos (False));
                                --  Boolean'Pos (is_Kinematic));

      b3d_Object_Friction_is    (Self.C, c_float (Friction));
      b3d_Object_Restitution_is (Self.C, c_float (Restitution));
      b3d_Object_user_Data_is   (Self => Self.C.all'Access,
                                 Now  => to_void_ptr (Self.all'Access));

      Self.user_Data_is (Self);
      Self.Site_is (at_Site);
   end define;


   overriding
   procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;


   procedure free (the_Object : in out physics.Object.view)
   is
      procedure deallocate is new ada.unchecked_Deallocation (physics.Object.item'Class,
                                                              physics.Object.view);
   begin
      the_Object.destruct;
      deallocate (the_Object);
   end free;


   function C (Self : in Item) return access bullet_C.Object
   is
   begin
      return Self.C;
   end C;


   overriding
   function Model (Self : in Item) return physics.Model.view
   is
   begin
      return Self.Model;
   end Model;


   overriding
   procedure Model_is (Self : in out Item;   Now : in physics.Model.view)
   is
   begin
      Self.Model := Now;
   end Model_is;


   overriding
   function Shape (Self : in Item) return physics.Shape.view
   is
      c_Shape : constant bullet_c.Pointers.Shape_pointer := b3d_Object_Shape (Self.C);

      function to_Any_view is new ada.unchecked_Conversion (Swig.void_ptr, Any_limited_view);
   begin
      return physics.Shape.view (to_Any_view (b3d_Shape_user_Data (c_Shape)));
   end Shape;


   overriding
   function Scale (Self : in Item) return Vector_3
   is
   begin
      raise Error with "TODO";
      return math.Origin_3D;
   end Scale;


   overriding
   procedure Scale_is (Self : in out Item;   Now : in Vector_3)
   is
   begin
      put_Line ("Scale_is not implemented for bullet_Physics.Object");
      raise Error with "TODO";
   end Scale_is;


   overriding
   procedure update_Dynamics (Self : in out Item)
   is
      Dynamics : constant Matrix_4x4 := Self.Transform;
   begin
      Self.Dynamics.set (Dynamics);
   end update_Dynamics;


   overriding
   function get_Dynamics (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.Dynamics.get;
   end get_Dynamics;


   overriding
   function is_Active (Self : in Item) return Boolean
   is
   begin
      return True;
   end is_Active;


   overriding
   procedure activate (Self : in out Item;   forceActivation : in Boolean := False)
   is
      pragma unreferenced (forceActivation);
   begin
      null;
   end activate;


   overriding
   function Mass (Self : in Item) return Real
   is
   begin
      return Real (b3d_Object_Mass (Self.C));
   end Mass;


   overriding
   function Site (Self : in Item) return Vector_3
   is
      the_Site : constant c_math_c.Vector_3.item := b3d_Object_Site (Self.C);
   begin
      return +the_Site;
   end Site;


   overriding
   procedure Site_is (Self : in out Item;   Now : in Vector_3)
   is
      c_Now : aliased c_math_c.Vector_3.item := +Now;
   begin
      b3d_Object_Site_is (Self.C, c_Now'unchecked_Access);
   end Site_is;


   overriding
   function Spin (Self : in Item) return math.Matrix_3x3
   is
      use type bullet_c.Pointers.Object_pointer;
   begin
      if Self.C /= null
      then
         declare
            the_Spin : constant c_math_c.Matrix_3x3.item := b3d_Object_Spin (Self.C);
         begin
            return +the_Spin;
         end;
      else
         return Self.Dynamics.get_Spin;
      end if;
   end Spin;


   overriding
   procedure Spin_is (Self : in out Item;   Now : in Matrix_3x3)
   is
      use type bullet_c.Pointers.Object_pointer;
   begin
      Self.Dynamics.set_Spin (Now);

      if Self.C /= null
      then
         declare
            c_Now : aliased c_math_c.Matrix_3x3.item := +Now;
         begin
            b3d_Object_Spin_is (Self.C, c_Now'unchecked_Access);
         end;
      end if;
   end Spin_is;


   overriding
   function xy_Spin (Self : in Item) return Radians
   is
   begin
      raise Error with "TODO";
      return 0.0;
   end xy_Spin;


   overriding
   procedure xy_Spin_is (Self : in out Item;   Now : in Radians)
   is
   begin
      raise Error with "TODO";
   end xy_Spin_is;


   overriding
   function Transform (Self : in Item) return Matrix_4x4
   is
      the_Transform : constant c_math_c.Matrix_4x4.item := b3d_Object_Transform (Self.C);
   begin
      return +the_Transform;
   end Transform;


   overriding
   procedure Transform_is (Self : in out Item;   Now : in Matrix_4x4)
   is
      c_Now : aliased c_math_c.Matrix_4x4.item := +Now;
   begin
      b3d_Object_Transform_is (Self.C, c_Now'unchecked_Access);
   end Transform_is;


   overriding
   function Speed (Self : in Item) return math.Vector_3
   is
      the_Speed : constant c_math_c.Vector_3.item := b3d_Object_Speed (Self.C);
   begin
      return +the_Speed;
   end Speed;


   overriding
   procedure Speed_is (Self : in out Item;   Now : in Vector_3)
   is
      c_Now : aliased c_math_c.Vector_3.item := +Now;
   begin
      b3d_Object_Speed_is (Self.C, c_Now'unchecked_Access);
   end Speed_is;


   overriding
   function Gyre (Self : in Item) return math.Vector_3
   is
      the_Gyre : constant c_math_c.Vector_3.item := b3d_Object_Gyre (Self.C);
   begin
      return +the_Gyre;
   end Gyre;


   overriding
   procedure Gyre_is (Self : in out Item;   Now : in Vector_3)
   is
      c_Now : aliased c_math_c.Vector_3.item := +Now;
   begin
      b3d_Object_Gyre_is (Self.C, c_Now'unchecked_Access);
   end Gyre_is;


   overriding
   procedure Friction_is (Self : in out Item;   Now : in Real)
   is
   begin
      b3d_Object_Friction_is (Self.C, +Now);
   end Friction_is;


   overriding
   procedure Restitution_is (Self : in out Item;   Now : in Real)
   is
   begin
      b3d_Object_Restitution_is (Self.C, +Now);
   end Restitution_is;


   --- Forces
   --

   overriding
   procedure apply_Torque (Self : in out Item;   Torque : in Vector_3)
   is
      c_Torque : aliased c_math_c.Vector_3.item := +Torque;
   begin
      b3d_Object_apply_Torque (Self.C, c_Torque'unchecked_Access);
   end apply_Torque;


   overriding
   procedure apply_Torque_impulse (Self : in out Item;   Torque : in Vector_3)
   is
      c_Torque : aliased c_math_c.Vector_3.item := +Torque;
   begin
      b3d_Object_apply_Torque_impulse (Self.C, c_Torque'unchecked_Access);
   end apply_Torque_impulse;


   overriding
   procedure apply_Force (Self : in out Item;   Force : in Vector_3)
   is
      c_Force : aliased c_math_c.Vector_3.item := +Force;
   begin
      b3d_Object_apply_Force (Self.C, c_Force'unchecked_Access);
   end apply_Force;


   --- User data
   --

   overriding
   procedure user_Data_is (Self : in out Item;   Now : access lace.Any.limited_item'Class)
   is
   begin
      Self.user_Data := Now.all'unchecked_Access;
   end user_Data_is;


   overriding
   function user_Data (Self : in Item) return access lace.Any.limited_item'Class
   is
   begin
      return Self.user_Data;
   end user_Data;


end bullet_Physics.Object;
