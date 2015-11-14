with
     physics.Shape,
     lace.Any;


package physics.Object
--
-- Provide an interface for physical objects.
--
is
   use Math;


   type Item is  limited interface
             and lace.Any.limited_Item;

   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;


   procedure user_Data_is (Self : in out Item;   Now : access lace.Any.limited_Item'Class)   is abstract;
   function  user_Data    (Self : in     Item)  return access lace.Any.limited_Item'Class    is abstract;

   function  Mass         (Self : in     Item)  return Real                                  is abstract;



   ----------
   --- Forge
   --

   procedure destruct (Self : in out Item)   is abstract;
   procedure free     (Self : in out View);



   ---------------
   --- Attributes
   --

   --- Shape
   --

   function  Shape        (Self : in     Item)     return physics.Shape.view   is abstract;

   function  Scale        (Self : in     Item)     return Vector_3           is abstract;
   procedure Scale_is     (Self : in out Item;   Now : in Vector_3)          is abstract;


   --- Dynamics
   --

   procedure activate       (Self : in out Item;   force_Activation : in Boolean := False)   is abstract;


   function  Site           (Self : in     Item)     return Vector_3           is abstract;
   procedure Site_is        (Self : in out Item;   Now : in Vector_3)          is abstract;

   function  Spin           (Self : in     Item)     return math.Matrix_3x3    is abstract;
   procedure Spin_is        (Self : in out Item;   Now : in math.Matrix_3x3)   is abstract;

   function  xy_Spin        (Self : in     Item)     return math.Radians       is abstract;
   procedure xy_Spin_is     (Self : in out Item;   Now : in math.Radians)      is abstract;

   function  Transform      (Self : in     Item)     return math.Matrix_4x4    is abstract;
   procedure Transform_is   (Self : in out Item;   Now : in math.Matrix_4x4)   is abstract;

   function  Speed          (Self : in     Item)     return math.Vector_3      is abstract;
   procedure Speed_is       (Self : in out Item;   Now : in math.Vector_3)     is abstract;

   function  Gyre           (Self : in     Item)     return math.Vector_3      is abstract;
   procedure Gyre_is        (Self : in out Item;   Now : in math.Vector_3)     is abstract;

   function  is_Active      (Self : in     Item)     return Boolean            is abstract;


   procedure Friction_is    (Self : in out Item;   Now : in math.Real)         is abstract;
   procedure Restitution_is (Self : in out Item;   Now : in math.Real)         is abstract;



   ---------------
   --- Operations
   --

   --- Forces
   --

   procedure apply_Torque         (Self : in out Item;   Torque : in math.Vector_3)   is abstract;
   procedure apply_Torque_impulse (Self : in out Item;   Torque : in math.Vector_3)   is abstract;

   procedure apply_Force          (Self : in out Item;   Force  : in math.Vector_3)   is abstract;



end physics.Object;
