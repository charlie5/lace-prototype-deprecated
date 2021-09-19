with
     gel.Joint,
     gel.Sprite,

     physics.Joint.DoF6,
     physics.Space;

package GEL.any_Joint
--
--  Allows sprites to be connected via '6 degree of freedom' joint.
--
is
   type Item  is new GEL.Joint.Item with private;
   type View  is access all Item'Class;
   type Views is array (Math.Index range <>) of View;


   Sway  : constant Joint.Degree_of_freedom := 1;
   Heave : constant Joint.Degree_of_freedom := 2;
   Surge : constant Joint.Degree_of_freedom := 3;

   Pitch : constant Joint.Degree_of_freedom := 4;
   Yaw   : constant Joint.Degree_of_freedom := 5;
   Roll  : constant Joint.Degree_of_freedom := 6;


   package std_physics renames standard.Physics;


   ---------
   --- Forge
   --

   procedure define  (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                            Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                            pivot_Anchor       : in     math.Vector_3;
                                            pivot_Axis         : in     math.Matrix_3x3);

   procedure define  (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                            Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                            Frame_A,  Frame_B  : in     math.Matrix_4x4);

   overriding
   procedure destroy (Self : in out Item);


   --------------
   --- Attributes
   --

   overriding
   function  Physics    (Self : in     Item) return gel.Joint.Physics_view;

   overriding
   function  Frame_A    (Self : in     Item) return math.Matrix_4x4;
   overriding
   function  Frame_B    (Self : in     Item) return math.Matrix_4x4;

   overriding
   procedure Frame_A_is (Self : in out Item;   Now : in math.Matrix_4x4);
   overriding
   procedure Frame_B_is (Self : in out Item;   Now : in math.Matrix_4x4);

   overriding
   function  Degrees_of_freedom (Self : in     Item) return joint.Degree_of_freedom;


   --  Bounds - limits the range of motion for a degree of freedom.
   --

   overriding
   function  is_Bound      (Self : in     Item;   for_Degree : in joint.Degree_of_freedom) return Boolean;

   overriding
   function  low_Bound     (Self : access Item;   for_Degree : in joint.Degree_of_freedom) return math.Real;
   overriding
   procedure low_Bound_is  (Self : access Item;   for_Degree : in joint.Degree_of_freedom;
                                                   Now        : in math.Real);
   overriding
   function  high_Bound    (Self : access Item;   for_Degree : in joint.Degree_of_freedom) return math.Real;
   overriding
   procedure high_Bound_is (Self : access Item;   for_Degree : in joint.Degree_of_freedom;
                                                  Now        : in math.Real);
   overriding
   function  Extent        (Self : in     Item;   for_Degree : in joint.Degree_of_freedom) return math.Real;

   overriding
   procedure Velocity_is   (Self : in     Item;   for_Degree : in joint.Degree_of_freedom;
                                                  Now        : in math.Real);

   --------------
   --- Operations
   --

   -- Nil.



private

   type physics_DoF6_Joint_view is access all std_physics.Joint.DoF6.item'Class;


   type Item is new gel.Joint.item with
      record
         Physics : access std_physics.Joint.DoF6.item'Class;
   end record;

end GEL.any_Joint;
