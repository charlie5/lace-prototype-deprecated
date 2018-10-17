with
     physics.Object,
     float_Math.Algebra.linear.d3,
     interfaces.C;


package body mmi.any_Joint
is
   use Math,
       Interfaces;

   package std_Physics renames Standard.Physics;



   ---------
   --  Forge
   --

   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A, Sprite_B : access mmi.Sprite.Item'Class;
                                           Frame_A,  Frame_B  : in     Math.Matrix_4x4)
   is
      the_Frame_A : aliased constant Matrix_4x4 := Frame_A;
      the_Frame_B : aliased constant Matrix_4x4 := Frame_B;

      type joint_Cast is access all MMI.Joint.Item;

      sprite_A_Solid,
      sprite_B_Solid : std_Physics.Object.view;

   begin
      if Sprite_A /= null then
         sprite_A_Solid := std_Physics.Object.view (Sprite_A.Solid);
      end if;

      if Sprite_B /= null then
         sprite_B_Solid := std_Physics.Object.view (Sprite_B.Solid);
      end if;

      Joint.define (joint_Cast (Self), Sprite_A, Sprite_B);   -- do base class

      Self.Physics := in_Space.new_DoF6_Joint (sprite_A_Solid,
                                               sprite_B_Solid,
                                               the_Frame_A,
                                               the_Frame_B);
   end define;



   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A, Sprite_B : access mmi.Sprite.Item'Class;
                                           pivot_Anchor       : in     math.Vector_3;
                                           pivot_Axis         : in     math.Matrix_3x3)
   is
      use Math.Algebra.linear.d3;

      pivot_in_A : constant Math.Vector_3   := Inverse (Sprite_A.Spin) * (pivot_Anchor - Sprite_A.Site);
      pivot_in_B : constant Math.Vector_3   := Inverse (Sprite_B.Spin) * (pivot_Anchor - Sprite_B.Site);

      axis_in_A  : constant Math.Matrix_3x3 := Sprite_A.Spin * pivot_Axis;
      axis_in_B  : constant Math.Matrix_3x3 := Sprite_B.Spin * pivot_Axis;

      Frame_A    : constant Math.Matrix_4x4 := to_transform_Matrix (axis_in_A, pivot_in_A);
      Frame_B    : constant Math.Matrix_4x4 := to_transform_Matrix (axis_in_B, pivot_in_B);
   begin
      Self.define (in_Space,
                   Sprite_A, Sprite_B,
                   Frame_A,  Frame_B);
   end define;



   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      raise Program_Error with "TBD";
   end destroy;





   --- Attributes
   --

   overriding
   function Frame_A (Self : in Item) return Math.Matrix_4x4
   is
   begin
      return Self.Physics.Frame_A;
   end Frame_A;


   overriding
   function Frame_B (Self : in Item) return Math.Matrix_4x4
   is
   begin
      return Self.Physics.Frame_B;
   end Frame_B;


   overriding
   procedure Frame_A_is (Self : in out Item; Now : in Math.Matrix_4x4)
   is
   begin
      Self.Physics.Frame_A_is (Now);
   end Frame_A_is;


   overriding
   procedure Frame_B_is (Self : in out Item; Now : in Math.Matrix_4x4)
   is
   begin
      Self.Physics.Frame_B_is (Now);
   end Frame_B_is;


   overriding
   function Physics (Self : in Item) return MMI.Joint.Physics_view
   is
   begin
      return MMI.Joint.Physics_view (Self.Physics);
   end Physics;



   overriding
   function Degrees_of_freedom (Self : in Item) return Joint.Degree_of_freedom
   is
      pragma Unreferenced (Self);
   begin
      return 6;
   end Degrees_of_freedom;



   --  Bounds - limits the range of motion for a Degree of freedom.
   --

   overriding
   function is_Bound (Self : in     Item;   for_Degree : in Joint.Degree_of_freedom) return Boolean
   is
   begin
      if for_Degree in Sway .. Surge then
         return False;
      end if;

      return Self.Physics.is_Limited (for_Degree);
   end is_Bound;



   overriding
   function low_Bound (Self : access Item;   for_Degree : in Joint.Degree_of_freedom) return Math.Real
   is
      use type Math.Real;
   begin
      case for_Degree
      is
         when Sway .. Surge =>
            raise Program_Error with "unhandled Degree of freedom: " & Joint.Degree_of_freedom'Image (for_Degree);

         when Pitch .. Roll =>
            return Self.Physics.lower_Limit (for_Degree);
      end case;
   end low_Bound;


   overriding
   procedure low_Bound_is (Self : access Item;   for_Degree : in Joint.Degree_of_freedom;
                                                 Now        : in Math.Real)
   is
      use type Math.Real;
   begin
      Self.Physics.lower_Limit_is (Now, for_Degree);
   end low_Bound_is;


   overriding
   function high_Bound (Self : access Item;   for_Degree : in Joint.Degree_of_freedom) return Math.Real
   is
      use type Math.Real;
   begin
      case for_Degree
      is
         when Sway .. Surge =>
            raise Program_Error with "unhandled Degree of freedom: " & Joint.Degree_of_freedom'Image (for_Degree);

         when Pitch .. Roll =>
            return Self.Physics.upper_Limit (for_Degree);
      end case;
   end high_Bound;


   overriding
   procedure high_Bound_is (Self : access Item;   for_Degree : in Joint.Degree_of_freedom;
                                                  Now        : in Math.Real)
   is
      use type Math.Real;
   begin
      Self.Physics.upper_Limit_is (Now, for_Degree);
   end high_Bound_is;



   --  Extent
   --

   overriding
   function Extent (Self : in     Item;   for_Degree : in Joint.Degree_of_freedom) return Math.Real
   is
   begin
      if for_Degree in Sway .. Surge
      then
         raise Program_Error with "unhandled Degree of freedom: " & Joint.Degree_of_freedom'Image (for_Degree);
      end if;

      return Self.Physics.Extent (for_Degree);
   end Extent;



   --  Motor Velocity
   --

   overriding
   procedure Velocity_is (Self : in     Item;   for_Degree : in Joint.Degree_of_freedom;
                                                Now        : in Math.Real)
   is
   begin
      Self.Physics.Velocity_is (Now, for_Degree);
   end Velocity_is;


end mmi.any_Joint;
