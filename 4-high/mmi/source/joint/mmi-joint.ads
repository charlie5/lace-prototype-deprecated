with
     physics.Joint,
     lace.Any;

limited
with
     mmi.Sprite;


package mmi.Joint
--
--  Allows sprites to be connected via a joint.
--  A joint constrains the motion of the sprites which it connects.
--
is

   type Item  is abstract new lace.Any.limited_Item with private;
   type View  is access all Item'Class;
   type Views is array (math.Index range <>) of View;

   null_Joints : constant Joint.views;



   type Physics_view is access all physics.Joint.Item'Class;

   function to_MMI (the_Joint : Physics.Joint.view) return mmi.Joint.view;


   subtype Degree_of_freedom is physics.Joint.Degree_of_freedom;




   --- Forge
   --
   procedure define  (Self : access Item;   Sprite_A, Sprite_B : access mmi.Sprite.item'class);

   procedure destroy (Self : in out Item)    is abstract;
   procedure free    (Self : in out View);



   --- Attributes
   --

   function  Sprite_A          (Self : in     Item'Class) return access mmi.Sprite.item'Class;
   function  Sprite_B          (Self : in     Item'Class) return access mmi.Sprite.item'Class;


   function  Frame_A           (Self : in     Item)       return math.Matrix_4x4   is abstract;
   function  Frame_B           (Self : in     Item)       return math.Matrix_4x4   is abstract;


   procedure Frame_A_is        (Self : in out Item;   Now  : in math.Matrix_4x4)   is abstract;
   procedure Frame_B_is        (Self : in out Item;   Now  : in math.Matrix_4x4)   is abstract;



   function Degrees_of_freedom (Self : in     Item)       return degree_of_Freedom    is abstract;
   --
   --  Returns the number of possible DoF's for this joint.



   function Physics            (Self : in     Item)       return Physics_view   is abstract;



   --  bounds - limits the range of motion for a Degree of freedom.
   --

   function  low_Bound         (Self : access Item;   for_Degree : in     Degree_of_freedom) return math.Real   is abstract;
   procedure low_Bound_is      (Self : access Item;   for_Degree : in     Degree_of_freedom;
                                                      Now        : in     math.Real)                            is abstract;

   function  high_Bound        (Self : access Item;   for_Degree : in     Degree_of_freedom) return math.Real   is abstract;
   procedure high_Bound_is     (Self : access Item;   for_Degree : in     Degree_of_freedom;
                                                      Now        : in     math.Real)                            is abstract;


   function  is_Bound          (Self : in     Item;   for_Degree : in     Degree_of_freedom) return Boolean     is abstract;
   --
   --  Returns true if an upper or lower bound has been set for the given Degree of freedom.

   function  Extent            (Self : in     Item;   for_Degree : in     Degree_of_freedom) return math.Real   is abstract;
   --
   --  Angle about axis for rotational joints, or, spatial distance along an exis, in the case of sliders, etc.


   procedure Velocity_is       (Self : in     Item;   for_Degree : in     Degree_of_freedom;
                                                      Now        : in     math.Real        )                    is abstract;

   function  reaction_Force    (Self : in     Item'Class) return math.Vector_3;
   function  reaction_Torque   (Self : in     Item'Class) return math.Real;




   --- Operations
   --

   -- Nil.




   ----------
   --- Hinges
   --

   function  local_Anchor_on_A    (Self : in     Item)     return math.Vector_3;
   function  local_Anchor_on_B    (Self : in     Item)     return math.Vector_3;

   procedure local_Anchor_on_A_is (Self :    out Item;   Now : in math.Vector_3);
   procedure local_Anchor_on_B_is (Self :    out Item;   Now : in math.Vector_3);





private

   type Item  is abstract new lace.Any.limited_Item with
      record
         Sprite_A          : access mmi.Sprite.item'Class;
         Sprite_B          : access mmi.Sprite.item'Class;

         local_Anchor_on_A :        math.Vector_3;
         local_Anchor_on_B :        math.Vector_3;
      end record;


   null_Joints : constant Joint.views (1 .. 0) := (others => null);


end mmi.Joint;
