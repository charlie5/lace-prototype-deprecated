with
     physics.Shape,
     physics.Model,
     lace.Any;

package physics.Object
--
-- Provide an interface for physics objects.
--
is
   type Item is  limited interface
             and lace.Any.limited_item;

   type View  is access all Item'Class;
   type Views is array (Positive range <>) of View;


   ----------
   --- Forge
   --
   procedure define (Self : access Item;   Shape       : in physics.Shape.view;
                                           Mass        : in Real;
                                           Friction    : in Real;
                                           Restitution : in Real;
                                           at_Site     : in Vector_3) is abstract;

   procedure destruct (Self : in out Item)   is abstract;
   procedure free     (Self : in out View);


   ---------------
   --- Attributes
   --

   procedure user_Data_is (Self : in out Item;   Now : access lace.Any.limited_item'Class)   is abstract;
   function  user_Data    (Self : in     Item)  return access lace.Any.limited_item'Class    is abstract;

   function  Mass         (Self : in     Item)  return Real                                  is abstract;

   function  Model        (Self : in     Item)     return physics.Model.view   is abstract;
   procedure Model_is     (Self : in out Item;   Now : in physics.Model.view)  is abstract;

   function  Shape        (Self : in     Item)     return physics.Shape.view   is abstract;

   function  Scale        (Self : in     Item)     return Vector_3             is abstract;
   procedure Scale_is     (Self : in out Item;   Now : in Vector_3)            is abstract;


   --- Dynamics
   --

   protected
   type safe_Dynamics
   is
      procedure set (To : in Matrix_4x4);
      function  get   return Matrix_4x4;

      procedure set_Spin (To : in Matrix_3x3);
      function  get_Spin   return Matrix_3x3;

      procedure set_Site (To : in Vector_3);
      function  get_Site   return Vector_3;

   private
      Dynamics : Matrix_4x4 := Identity_4x4;
   end safe_Dynamics;


   procedure update_Dynamics (Self : in out Item)                     is abstract;
   function     get_Dynamics (Self : in     Item) return Matrix_4x4   is abstract;

   procedure activate        (Self : in out Item;   force_Activation : in Boolean := False)   is abstract;


   function  Site            (Self : in     Item)     return Vector_3      is abstract;
   procedure Site_is         (Self : in out Item;   Now : in Vector_3)     is abstract;

   function  Spin            (Self : in     Item)     return Matrix_3x3    is abstract;
   procedure Spin_is         (Self : in out Item;   Now : in Matrix_3x3)   is abstract;

   function  xy_Spin         (Self : in     Item)     return Radians       is abstract;
   procedure xy_Spin_is      (Self : in out Item;   Now : in Radians)      is abstract;

   function  Transform       (Self : in     Item)     return Matrix_4x4    is abstract;
   procedure Transform_is    (Self : in out Item;   Now : in Matrix_4x4)   is abstract;

   function  Speed           (Self : in     Item)     return Vector_3      is abstract;
   procedure Speed_is        (Self : in out Item;   Now : in Vector_3)     is abstract;

   function  Gyre            (Self : in     Item)     return Vector_3      is abstract;
   procedure Gyre_is         (Self : in out Item;   Now : in Vector_3)     is abstract;

   function  is_Active       (Self : in     Item)     return Boolean       is abstract;

   procedure Friction_is     (Self : in out Item;   Now : in Real)         is abstract;
   procedure Restitution_is  (Self : in out Item;   Now : in Real)         is abstract;


   ---------------
   --- Operations
   --

   --- Forces
   --

   procedure apply_Torque         (Self : in out Item;   Torque : in Vector_3)   is abstract;
   procedure apply_Torque_impulse (Self : in out Item;   Torque : in Vector_3)   is abstract;

   procedure apply_Force          (Self : in out Item;   Force  : in Vector_3)   is abstract;


end physics.Object;
