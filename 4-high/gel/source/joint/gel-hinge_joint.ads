with
     gel.Joint,
     gel.Sprite,

     physics.Joint.hinge,
     physics.Space;

package gel.hinge_Joint
--
--  Allows sprites to be connected via a hinge joint.
--
is
   type Item  is new gel.Joint.item with private;
   type View  is access all Item'Class;
   type Views is array (math.Index range <>) of View;


   --  Degrees of freedom.
   --
   Revolve : constant joint.Degree_of_freedom := 1;


   package std_physics renames standard.Physics;
   use Math;


   ---------
   --- Forge
   --

   procedure define (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                           Sprite_A,
                                           Sprite_B           : access gel.Sprite.item'Class;
                                           pivot_Axis         : in     Vector_3;
                                           Anchor_in_A        : in     Vector_3;
                                           Anchor_in_B        : in     Vector_3;
                                           low_Limit,
                                           high_Limit         : in     math.Real;
                                           collide_Conected   : in     Boolean);

   procedure define (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                           Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                           pivot_Axis         : in     Vector_3;
                                           pivot_Anchor       : in     Vector_3);

   procedure define (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                           Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                           pivot_Axis         : in     Vector_3);
   --
   --  Uses midpoint between sprite A and B for the pivot anchor.


   procedure define (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                           Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                           Frame_A,  Frame_B  : in     Matrix_4x4;
                                           low_Limit          : in     Real := to_Radians (-180.0);
                                           high_Limit         : in     Real := to_Radians ( 180.0);
                                           collide_Conected   : in     Boolean);

   procedure define (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                           Sprite_A           : access gel.Sprite.item'Class;
                                           Frame_A            : in     Matrix_4x4);


   overriding
   procedure destroy (Self : in out Item);


   --------------
   --- Attributes
   --

   function  Angle      (Self : in     Item'Class) return Real;

   overriding
   function  Physics    (Self : in     Item) return Joint.Physics_view;

   procedure Limits_are (Self : in out Item'Class;   Low, High         : in Real;
                                                     Softness          : in Real := 0.9;
                                                     bias_Factor       : in Real := 0.3;
                                                     relaxation_Factor : in Real := 1.0);
   overriding
   function  Frame_A    (Self : in     Item) return Matrix_4x4;
   overriding
   function  Frame_B    (Self : in     Item) return Matrix_4x4;

   overriding
   procedure Frame_A_is (Self : in out Item;   Now  : in Matrix_4x4);
   overriding
   procedure Frame_B_is (Self : in out Item;   Now  : in Matrix_4x4);

   overriding
   function Degrees_of_freedom (Self : in Item) return joint.degree_of_Freedom;


   --  Bounds - limits the range of motion for a degree of freedom.
   --

   overriding
   function  low_Bound     (Self : access Item;   for_Degree : in joint.Degree_of_freedom) return Real;
   overriding
   procedure low_Bound_is  (Self : access Item;   for_Degree : in joint.Degree_of_freedom;
                                                  Now        : in Real);
   overriding
   function  high_Bound    (Self : access Item;   for_Degree : in joint.Degree_of_freedom) return Real;
   overriding
   procedure high_Bound_is (Self : access Item;   for_Degree : in joint.Degree_of_freedom;
                                                  Now        : in Real);
   overriding
   function  is_Bound      (Self : in     Item;   for_Degree : in joint.Degree_of_freedom) return Boolean;

   overriding
   function  Extent        (Self : in     Item;   for_Degree : in joint.Degree_of_freedom) return Real;

   overriding
   procedure Velocity_is   (Self : in     Item;   for_Degree : in joint.Degree_of_freedom;
                                                  Now        : in Real);

   --------------
   --- Operations
   --

   -- Nil.



private

   type Item  is new gel.Joint.item with
      record
         Physics : access std_physics.Joint.hinge.item'Class;

         low_Bound,
         high_Bound        : Real;

         Softness          : Real;
         bias_Factor       : Real;
         relaxation_Factor : Real;
      end record;

   procedure apply_Limits (Self : in out Item);

end gel.hinge_Joint;
