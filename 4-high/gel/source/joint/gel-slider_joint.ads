with
     gel.Joint,
     gel.Sprite,

     physics.Joint.slider,
     physics.Space;

package gel.slider_Joint
--
--  Allows sprites to be connected via a slider joint.
--
is
   type Item is new gel.Joint.Item with private;
   type View is access all Item'Class;
   type Views is array (math.Index range <>) of View;


   Sway  : constant Joint.Degree_of_freedom := 1;     -- TODO: These are duplicated.
   Heave : constant Joint.Degree_of_freedom := 2;
   Surge : constant Joint.Degree_of_freedom := 3;

   Pitch : constant Joint.Degree_of_freedom := 4;
   Yaw   : constant Joint.Degree_of_freedom := 5;
   Roll  : constant Joint.Degree_of_freedom := 6;


   package std_physics renames standard.Physics;
   use Math;


   ---------
   --- Forge
   --

   procedure define  (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                            Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                            pivot_Anchor       : in     Vector_3;
                                            pivot_Axis         : in     Matrix_3x3);

   procedure define  (Self : access Item;   in_Space           : in     std_physics.Space.view;
                                            Sprite_A, Sprite_B : access gel.Sprite.item'Class;
                                            Frame_A,  Frame_B  : in     Matrix_4x4);
   overriding
   procedure destroy (Self : in out Item);


   --------------
   --- Attributes
   --

   overriding
   function  Physics    (Self : in     Item) return gel.Joint.Physics_view;

   overriding
   function  Frame_A    (Self : in     Item) return Matrix_4x4;
   overriding
   function  Frame_B    (Self : in     Item) return Matrix_4x4;

   overriding
   procedure Frame_A_is (Self : in out Item; Now : in Matrix_4x4);
   overriding
   procedure Frame_B_is (Self : in out Item; Now : in Matrix_4x4);

   overriding
   function  Degrees_of_freedom (Self : in Item) return Joint.Degree_of_freedom;


   --  Bounds - limits the range of motion for a Degree of freedom.
   --

   overriding
   function  is_Bound      (Self : in     Item;   for_Degree : in Joint.Degree_of_freedom) return Boolean;

   overriding
   function  low_Bound     (Self : access Item;   for_Degree : in Joint.Degree_of_freedom) return Real;
   overriding
   procedure low_Bound_is  (Self : access Item;   for_Degree : in Joint.Degree_of_freedom;
                                                  Now        : in Real);
   overriding
   function  high_Bound    (Self : access Item;   for_Degree : in Joint.Degree_of_freedom) return Real;
   overriding
   procedure high_Bound_is (Self : access Item;   for_Degree : in Joint.Degree_of_freedom;
                                                  Now        : in Real);
   overriding
   function  Extent        (Self : in     Item;   for_Degree : in Joint.Degree_of_freedom) return Real;
   overriding
   procedure Velocity_is   (Self : in     Item;   for_Degree : in Joint.Degree_of_freedom;
                                                  Now        : in Real);

   --------------
   --- Operations
   --

   -- Nil.



private

   type physics_slider_Joint_view is access all std_physics.Joint.slider.item'Class;


   type Item is new gel.Joint.Item with
      record
         Physics : access std_physics.Joint.slider.item'Class;
   end record;

end gel.slider_Joint;
