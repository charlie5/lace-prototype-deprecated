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



   ---------
   --  Forge
   --

   use type math.Degrees;

   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A,
                                           Sprite_B           : access gel.Sprite.item'class;
                                           pivot_Axis         : in     math.Vector_3;
                                           Anchor_in_A        : in     math.Vector_3;
                                           Anchor_in_B        : in     math.Vector_3;
                                           low_Limit,
                                           high_Limit         : in     math.Real;
                                           collide_Conected   : in     Boolean);

   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A, Sprite_B : access gel.Sprite.item'class;
                                           pivot_Axis         : in     math.Vector_3;
                                           pivot_Anchor       : in     math.Vector_3);

   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A, Sprite_B : access gel.Sprite.item'class;
                                           pivot_Axis         : in     math.Vector_3);
   --
   --  Uses midpoint between sprite A and B for pivot_Anchor.


   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A, Sprite_B : access gel.Sprite.item'class;
                                           Frame_A,  Frame_B  : in     math.Matrix_4x4;
                                           low_Limit          : in     math.Real := math.to_Radians (-180.0);
                                           high_Limit         : in     math.Real := math.to_Radians ( 180.0);
                                           collide_Conected   : in     Boolean);

   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A           : access gel.Sprite.item'class;
                                           Frame_A            : in     math.Matrix_4x4);


   overriding
   procedure destroy (Self : in out Item);



   --------------
   --  Attributes
   --

   function  Angle      (Self : in     Item'Class) return math.Real;

   overriding
   function  Physics    (Self : in     Item) return Joint.Physics_view;

   procedure Limits_are (Self : in out Item'Class;   Low, High         : in math.Real;
                                                     Softness          : in math.Real := 0.9;
                                                     bias_Factor       : in math.Real := 0.3;
                                                     relaxation_Factor : in math.Real := 1.0);

   overriding
   function  Frame_A    (Self : in     Item) return math.Matrix_4x4;
   overriding
   function  Frame_B    (Self : in     Item) return math.Matrix_4x4;


   overriding
   procedure Frame_A_is (Self : in out Item;   Now  : in math.Matrix_4x4);
   overriding
   procedure Frame_B_is (Self : in out Item;   Now  : in math.Matrix_4x4);


   overriding
   function Degrees_of_freedom (Self : in Item) return joint.degree_of_Freedom;



   --  Bounds - limits the range of motion for a Degree of freedom.
   --

   overriding
   function  low_Bound     (Self : access Item;   for_Degree : in     joint.Degree_of_freedom) return math.Real;
   overriding
   procedure low_Bound_is  (Self : access Item;   for_Degree : in     joint.Degree_of_freedom;
                                                  Now        : in     math.Real);

   overriding
   function  high_Bound    (Self : access Item;   for_Degree : in     joint.Degree_of_freedom) return math.Real;
   overriding
   procedure high_Bound_is (Self : access Item;   for_Degree : in     joint.Degree_of_freedom;
                                                  Now        : in     math.Real);

   overriding
   function  is_Bound      (Self : in     Item;   for_Degree : in     joint.Degree_of_freedom) return Boolean;

   overriding
   function  Extent        (Self : in     Item;   for_Degree : in     joint.Degree_of_freedom) return math.Real;


   overriding
   procedure Velocity_is   (Self : in     Item;   for_Degree : in     joint.Degree_of_freedom;
                                                  Now        : in     math.Real);


   --------------
   --  Operations
   --

   -- Nil.




private

   type Item  is new gel.Joint.item with
      record
         Physics : access standard.physics.Joint.hinge.item'Class;

         low_Bound,
         high_Bound        : math.Real;

         Softness          : math.Real;
         bias_Factor       : math.Real;
         relaxation_Factor : math.Real;
      end record;

   procedure apply_Limits (Self : in out Item);


end gel.hinge_Joint;
