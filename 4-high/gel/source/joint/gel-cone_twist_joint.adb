with
     physics.Object;

package body gel.cone_twist_Joint
is

   procedure define (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                           Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                           Frame_A,  Frame_B  : in     Matrix_4x4)
   is
      A_Frame : aliased constant Matrix_4x4 := Frame_A;
      B_Frame : aliased constant Matrix_4x4 := Frame_B;

      type Joint_cast is access all gel.Joint.item;

      sprite_A_Solid,
      sprite_B_Solid : std_physics.Object.view;

   begin
      if Sprite_A /= null then   sprite_A_Solid := standard.physics.Object.view (Sprite_A.Solid);   end if;
      if Sprite_B /= null then   sprite_B_Solid := standard.physics.Object.view (Sprite_B.Solid);   end if;

      Joint.define (Joint_cast (Self),  Sprite_A, Sprite_B);   -- Define base class.

      Self.Physics := in_Space.new_DoF6_Joint (sprite_A_Solid,
                                               sprite_B_Solid,
                                               A_Frame,
                                               B_Frame);
   end define;



   procedure define (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                           Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                           pivot_Anchor       : in     Vector_3;
                                           pivot_Axis         : in     Matrix_3x3)
   is
      use linear_Algebra_3D;

      pivot_in_A : constant Vector_3   := pivot_Anchor - Sprite_A.Site;
      pivot_in_B : constant Vector_3   := pivot_Anchor - Sprite_B.Site;

      Frame_A    : constant Matrix_4x4 := to_transform_Matrix (pivot_Axis, pivot_in_A);
      Frame_B    : constant Matrix_4x4 := to_transform_Matrix (pivot_Axis, pivot_in_B);
   begin
      Self.define (in_Space,
                   Sprite_A, Sprite_B,
                   Frame_A,  Frame_B);
   end define;



   overriding
   procedure destroy (Self : in out Item)
   is
   begin
      raise Error with "TODO";
   end destroy;


   --------------
   --- Attributes
   --

   overriding
   function Frame_A (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.Physics.Frame_A;
   end Frame_A;


   overriding
   function Frame_B (Self : in Item) return Matrix_4x4
   is
   begin
      return Self.Physics.Frame_B;
   end Frame_B;


   overriding
   procedure Frame_A_is (Self : in out Item; Now : in Matrix_4x4)
   is
   begin
      Self.Physics.Frame_A_is (Now);
   end Frame_A_is;


   overriding
   procedure Frame_B_is (Self : in out Item; Now : in Matrix_4x4)
   is
   begin
      Self.Physics.Frame_B_is (Now);
   end Frame_B_is;


   overriding
   function Physics (Self : in Item) return gel.Joint.Physics_view
   is
   begin
      return joint.Physics_view (Self.Physics);
   end Physics;


   overriding
   function Degrees_of_freedom (Self : in Item) return joint.Degree_of_freedom
   is
      pragma unreferenced (Self);
   begin
      return 6;
   end Degrees_of_freedom;



   --  Bounds - limits the range of motion for a Degree of freedom.
   --

   overriding
   function is_Bound (Self : in Item;   for_Degree : in joint.Degree_of_freedom) return Boolean
   is
   begin
      if for_Degree in Sway .. Surge
      then
         return False;
      end if;

      return Self.Physics.is_Limited (for_Degree);
   end is_Bound;


   overriding
   function low_Bound (Self : access Item;   for_Degree : in joint.Degree_of_freedom) return Real
   is
   begin
      case for_Degree
      is
         when Sway .. Surge =>
            raise Error with "Unhandled Degree of freedom:" & for_Degree'Image;

         when Pitch .. Roll =>
            return Self.Physics.lower_Limit (for_Degree);
      end case;
   end low_Bound;


   overriding
   procedure low_Bound_is (Self : access Item;   for_Degree : in joint.Degree_of_freedom;
                                                 Now        : in Real)
   is
   begin
      Self.Physics.lower_Limit_is (Now, for_Degree);
   end low_Bound_is;


   overriding
   function high_Bound (Self : access Item;   for_Degree : in joint.Degree_of_freedom) return Real
   is
   begin
      case for_Degree
      is
         when Sway .. Surge =>
            raise Error with "Unhandled Degree of freedom:" & for_Degree'Image;

         when Pitch .. Roll =>
            return Self.Physics.upper_Limit (for_Degree);
      end case;
   end high_Bound;


   overriding
   procedure high_Bound_is (Self : access Item;   for_Degree : in joint.Degree_of_freedom;
                                                  Now        : in Real)
   is
   begin
      Self.Physics.upper_Limit_is (Now, for_Degree);
   end high_Bound_is;


   ----------
   --- Extent
   --

   overriding
   function Extent (Self : in Item;   for_Degree : in joint.Degree_of_freedom) return Real
   is
   begin
      if for_Degree in Sway .. Surge
      then
         raise Error with "Unhandled Degree of freedom:" & for_Degree'Image;
      end if;

      return Self.Physics.Extent (for_Degree);
   end Extent;


   ------------------
   --- Motor Velocity
   --

   overriding
   procedure Velocity_is (Self : in Item;   for_Degree : in joint.Degree_of_freedom;
                                            Now        : in Real)
   is
   begin
      Self.Physics.Velocity_is (Now, for_Degree);
   end Velocity_is;


end gel.cone_twist_Joint;
