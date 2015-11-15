with
     mmi.Joint,
     mmi.Sprite,

     physics.Joint.slider,
     physics.Space;


package mmi.slider_Joint
--
--  Allows sprites to be connected via '6 degree of freedom' joint.
--
is

   type Item is new mmi.Joint.Item with private;
   type View is access all Item'Class;
   type Views is array (Math.Index range <>) of View;



   Sway  : constant Joint.Degree_of_freedom := 1;
   Heave : constant Joint.Degree_of_freedom := 2;
   Surge : constant Joint.Degree_of_freedom := 3;

   Pitch : constant Joint.Degree_of_freedom := 4;
   Yaw   : constant Joint.Degree_of_freedom := 5;
   Roll  : constant Joint.Degree_of_freedom := 6;



   --  Forge
   --

   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A, Sprite_B : access mmi.Sprite.Item'Class;
                                           pivot_Anchor       : in     math.Vector_3;
                                           pivot_Axis         : in     math.Matrix_3x3);

   procedure define (Self : access Item;   in_Space           : in     Standard.physics.Space.view;
                                           Sprite_A, Sprite_B : access mmi.Sprite.Item'Class;
                                           Frame_A,  Frame_B  : in     Math.Matrix_4x4);

   overriding
   procedure destroy (Self : in out Item);




   --  Attributes
   --

   overriding
   function  Physics            (Self : in     Item) return MMI.Joint.Physics_view;

   overriding
   function  Frame_A            (Self : in     Item) return Math.Matrix_4x4;
   overriding
   function  Frame_B            (Self : in     Item) return Math.Matrix_4x4;

   overriding
   procedure Frame_A_is         (Self : in out Item; Now : in Math.Matrix_4x4);
   overriding
   procedure Frame_B_is         (Self : in out Item; Now : in Math.Matrix_4x4);

   overriding
   function  Degrees_of_freedom (Self : in     Item) return Joint.Degree_of_freedom;



   --  Bounds - limits the range of motion for a Degree of freedom.
   --

   overriding
   function  is_Bound      (Self : in     Item;   for_Degree : in Joint.Degree_of_freedom) return Boolean;

   overriding
   function  low_Bound     (Self : access Item;   for_Degree : in Joint.Degree_of_freedom) return math.Real;
   overriding
   procedure low_Bound_is  (Self : access Item;   for_Degree : in Joint.Degree_of_freedom;
                                                  Now        : in Math.Real);

   overriding
   function high_Bound     (Self : access Item;   for_Degree : in Joint.Degree_of_freedom) return math.Real;
   overriding
   procedure high_Bound_is (Self : access Item;   for_Degree : in Joint.Degree_of_freedom;
                                                  Now        : in Math.Real);

   overriding
   function Extent         (Self : in     Item;   for_Degree : in Joint.Degree_of_freedom) return math.Real;

   overriding
   procedure Velocity_is   (Self : in     Item;   for_Degree : in Joint.Degree_of_freedom;
                                                  Now        : in Math.Real);




   --  Operations
   --

   -- Nil.




private

   type physics_slider_Joint_view is access all Standard.Physics.Joint.slider.Item'Class;


   type Item is new MMI.Joint.Item with
      record
         Physics : access standard.physics.Joint.Slider.item'Class;
   end record;


end mmi.slider_Joint;
