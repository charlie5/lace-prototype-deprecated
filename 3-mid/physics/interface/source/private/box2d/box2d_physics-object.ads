with
     physics.Object,
     physics.Shape,
     physics.Model,
     box2d_C;

private
with
     lace.Any;


package box2d_Physics.Object
--
--  Provides glue between a physics object and a Box2D object.
--
is
   use Math;

   type Item is new physics.Object.item with private;
   type View is access all Item'Class;

   function new_Object (Shape : in physics.Shape.view) return Object.view;

   overriding
   procedure define (Self : access Item;   Mass        : in Real;
                                           Friction    : in Real;
                                           Restitution : in Real;
                                           at_Site     : in Vector_3);



   -- old
--     function  new_Object (Shape       : in physics.Shape.view;
--                           Mass        : in Real;
--                           Friction    : in Real;
--                           Restitution : in Real;
--                           at_Site     : in Vector_3) return Object.view;


   procedure free (the_Object : in out physics.Object.view);


   function C (Self : in Item) return access box2d_C.Object;

   procedure Shape_is (Self : in out Item;   Now : in Physics.Shape.view);

   overriding
   function  Model        (Self : in     Item)     return physics.Model.view;
   overriding
   procedure Model_is     (Self : in out Item;   Now : in physics.Model.view);

   overriding
   procedure update_Dynamics (Self : in out Item);
   overriding
   function     get_Dynamics (Self : in     Item) return Matrix_4x4;


private

   type Item is new physics.Object.item with
      record
         C         : access box2d_C.Object;
         Shape     :        physics.Shape.view;
         Model     :        physics.Model.view;
         user_Data : access lace.Any.limited_Item'Class;

         Dynamics  : physics.Object.safe_Dynamics;
      end record;


   overriding
   procedure destruct       (Self : in out Item);


   overriding
   function  Shape          (Self : in     Item)     return physics.Shape.view;


   overriding
   function  Scale          (Self : in     Item)     return Vector_3;
   overriding
   procedure Scale_is       (Self : in out Item;   Now : in Vector_3);


   overriding
   procedure activate       (Self : in out Item;   forceActivation : in Boolean := False);


   overriding
   function  Mass           (Self : in     Item)     return Real;


   overriding
   function  is_Active      (Self : in     Item)     return Boolean;


   overriding
   function  Site           (Self : in     Item)     return Vector_3;
   overriding
   procedure Site_is        (Self : in out Item;   Now : in Vector_3);


   overriding
   function  Spin           (Self : in     Item)     return math.Matrix_3x3;
   overriding
   procedure Spin_is        (Self : in out Item;   Now : in math.Matrix_3x3);

   overriding
   function  xy_Spin        (Self : in     Item)     return math.Radians;
   overriding
   procedure xy_Spin_is     (Self : in out Item;   Now : in math.Radians);

   overriding
   function  Transform      (Self : in     Item)     return math.Matrix_4x4;
   overriding
   procedure Transform_is   (Self : in out Item;   Now : in math.Matrix_4x4);

   overriding
   function  Speed          (Self : in     Item)     return math.Vector_3;
   overriding
   procedure Speed_is       (Self : in out Item;   Now : in math.Vector_3);

   overriding
   function  Gyre           (Self : in     Item)     return math.Vector_3;
   overriding
   procedure Gyre_is        (Self : in out Item;   Now : in math.Vector_3);

   overriding
   procedure Friction_is    (Self : in out Item;   Now : in math.Real);

   overriding
   procedure Restitution_is (Self : in out Item;   Now : in math.Real);



   --- Forces
   --

   overriding
   procedure apply_Torque         (Self : in out Item;   Torque : in math.Vector_3);

   overriding
   procedure apply_Torque_impulse (Self : in out Item;   Torque : in math.Vector_3);

   overriding
   procedure apply_Force          (Self : in out Item;   Force  : in math.Vector_3);


   --- User data
   --

   overriding
   procedure user_Data_is (Self : in out Item;   Now : access lace.Any.limited_Item'Class);

   overriding
   function  user_Data    (Self : in     Item)  return access lace.Any.limited_Item'Class;


end box2d_Physics.Object;
