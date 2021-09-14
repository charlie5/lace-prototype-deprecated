with
     physics.Joint.DoF6,
     physics.Joint.cone_twist,
     physics.Joint.slider,
     physics.Joint.hinge,
     physics.Joint.ball,

     physics.Object,

     box2d_C.Pointers,

     lace.Any;

package box2d_Physics.Joint
--
--  Provides glue between a physics joint and a Box2D joint.
--
is
   type Item is abstract limited new physics.Joint.item with     -- TODO: Make private.
      record
         C         :        box2d_c.Pointers.Joint_Pointer;
         user_Data : access lace.Any.limited_item'Class;
      end record;

   type View is access all Item'Class;


   use Math;

   function new_Dof6_Joint       (Object_A,   Object_B     : in physics.Object.view;
                                  Frame_A,    Frame_B      : in Matrix_4x4) return physics.Joint.DoF6.view;

   function new_ball_Joint       (Object_A,   Object_B     : in physics.Object.view;
                                  Pivot_in_A, Pivot_in_B   : in Vector_3) return physics.Joint.ball.view;

   function new_slider_Joint     (Object_A,   Object_B     : in physics.Object.view;
                                  Frame_A,    Frame_B      : in Matrix_4x4) return physics.Joint.slider.view;

   function new_cone_twist_Joint (Object_A,   Object_B     : in physics.Object.view;
                                  Frame_A,    Frame_B      : in Matrix_4x4) return physics.Joint.cone_twist.view;

   function new_hinge_Joint      (in_Space                 : in box2d_c.Pointers.Space_Pointer;
                                  Object_A,    Object_B    : in physics.Object.view;
                                  Anchor_in_A, Anchor_in_B : in Vector_3;
                                  low_Limit,   high_Limit  : in math.Real;
                                  collide_Conected         : in Boolean) return physics.Joint.hinge.view;

   function new_hinge_Joint      (in_Space                 : in box2d_c.Pointers.Space_Pointer;
                                  Object_A,   Object_B     : in physics.Object.view;
                                  Frame_A,    Frame_B      : in Matrix_4x4;
                                  low_Limit,  high_Limit   : in math.Real;
                                  collide_Conected         : in Boolean) return physics.Joint.hinge.view;

   function new_hinge_Joint      (Object_A                 : in physics.Object.view;
                                  Frame_A                  : in Matrix_4x4) return physics.Joint.hinge.view;


   procedure free (the_Joint : in out physics.Joint.view);

   --  procedure set_b2d_user_Data (Self : in View);



private

   overriding
   function  reaction_Force  (Self : in     Item) return Vector_3;

   overriding
   function  reaction_Torque (Self : in     Item) return Real;


   overriding
   procedure user_Data_is    (Self : in out Item;   Now : access lace.Any.limited_Item'Class);

   overriding
   function  user_Data       (Self : in     Item)  return access lace.Any.limited_Item'Class;


   use physics.Joint;

   --------
   --  DoF6
   --
   type DoF6 is new Item
                and physics.Joint.DoF6.item with
      record
         null;
      end record;

   overriding
   procedure destruct    (Self : in out DoF6);

   overriding
   function  Object_A    (Self : in     DoF6) return physics.Object.view;
   overriding
   function  Object_B    (Self : in     DoF6) return physics.Object.view;

   overriding
   function  Frame_A     (Self : in     DoF6) return Matrix_4x4;
   overriding
   function  Frame_B     (Self : in     DoF6) return Matrix_4x4;

   overriding
   procedure Frame_A_is  (Self : in out DoF6;   Now : in Matrix_4x4);
   overriding
   procedure Frame_B_is  (Self : in out DoF6;   Now : in Matrix_4x4);

   overriding
   function  is_Limited  (Self : in     DoF6;   DoF : Degree_of_freedom) return Boolean;

   overriding
   procedure Velocity_is (Self : in out DoF6;   Now : in Real;
                                                DoF : in Degree_of_freedom);
   overriding
   function  Extent      (Self : in     DoF6;   DoF : in Degree_of_freedom) return Real;
   overriding
   procedure desired_Extent_is (Self : in out DoF6;   Now : in Real;
                                                      DoF : in Degree_of_freedom);
   overriding
   function  lower_Limit (Self : in     DoF6;   DoF : in Degree_of_freedom) return Real;
   overriding
   function  upper_Limit (Self : in     DoF6;   DoF : in Degree_of_freedom) return Real;

   overriding
   procedure lower_Limit_is (Self : in out DoF6;   Now : in Real;
                                                   DoF : in Degree_of_freedom);
   overriding
   procedure upper_Limit_is (Self : in out DoF6;   Now : in Real;
                                                   DoF : in Degree_of_freedom);

   ----------
   --  Slider
   --
   type Slider is new Item
                  and physics.Joint.Slider.item with
      record
         null;
      end record;

   overriding
   procedure destruct    (Self : in out Slider);

   overriding
   function  Object_A    (Self : in     Slider) return physics.Object.view;
   overriding
   function  Object_B    (Self : in     Slider) return physics.Object.view;

   overriding
   function  Frame_A     (Self : in     Slider) return Matrix_4x4;
   overriding
   function  Frame_B     (Self : in     Slider) return Matrix_4x4;

   overriding
   procedure Frame_A_is  (Self : in out Slider;   Now : in Matrix_4x4);
   overriding
   procedure Frame_B_is  (Self : in out Slider;   Now : in Matrix_4x4);

   overriding
   function  is_Limited  (Self : in     Slider;   DoF : Degree_of_freedom) return Boolean;

   overriding
   procedure Velocity_is (Self : in out Slider;   Now : in Real;
                                                  DoF : in Degree_of_freedom);
   overriding
   function  Extent      (Self : in     Slider;   DoF : in Degree_of_freedom) return Real;
   overriding
   procedure desired_Extent_is (Self : in out Slider;   Now : in Real;
                                                        DoF : in Degree_of_freedom);
   overriding
   function  lower_Limit (Self : in     Slider;   DoF : in Degree_of_freedom) return Real;
   overriding
   function  upper_Limit (Self : in     Slider;   DoF : in Degree_of_freedom) return Real;

   overriding
   procedure lower_Limit_is (Self : in out Slider;   Now : in Real;
                                                     DoF : in Degree_of_freedom);
   overriding
   procedure upper_Limit_is (Self : in out Slider;   Now : in Real;
                                                     DoF : in Degree_of_freedom);

   --------------
   --  cone_Twist
   --
   type cone_Twist is new Item
                      and physics.Joint.cone_Twist.item with
      record
         null;
      end record;

   overriding
   procedure destruct    (Self : in out cone_Twist);

   overriding
   function  Object_A    (Self : in     cone_Twist) return physics.Object.view;
   overriding
   function  Object_B    (Self : in     cone_Twist) return physics.Object.view;

   overriding
   function  Frame_A     (Self : in     cone_Twist) return Matrix_4x4;
   overriding
   function  Frame_B     (Self : in     cone_Twist) return Matrix_4x4;

   overriding
   procedure Frame_A_is  (Self : in out cone_Twist;   Now : in Matrix_4x4);
   overriding
   procedure Frame_B_is  (Self : in out cone_Twist;   Now : in Matrix_4x4);

   overriding
   function  is_Limited  (Self : in     cone_Twist;   DoF : Degree_of_freedom) return Boolean;

   overriding
   procedure Velocity_is (Self : in out cone_Twist;   Now : in Real;
                                                      DoF : in Degree_of_freedom);
   overriding
   function  Extent      (Self : in     cone_Twist;   DoF : in Degree_of_freedom) return Real;
   overriding
   procedure desired_Extent_is (Self : in out cone_Twist;   Now : in Real;
                                                            DoF : in Degree_of_freedom);
   overriding
   function  lower_Limit (Self : in     cone_Twist;   DoF : in Degree_of_freedom) return Real;
   overriding
   function  upper_Limit (Self : in     cone_Twist;   DoF : in Degree_of_freedom) return Real;

   overriding
   procedure lower_Limit_is (Self : in out cone_Twist;   Now : in Real;
                                                         DoF : in Degree_of_freedom);
   overriding
   procedure upper_Limit_is (Self : in out cone_Twist;   Now : in Real;
                                                         DoF : in Degree_of_freedom);

   --------
   --  Ball
   --
   type Ball is new Item
                and physics.Joint.Ball.item with
      record
         null;
      end record;

   overriding
   procedure destruct    (Self : in out Ball);

   overriding
   function  Object_A    (Self : in     Ball) return physics.Object.view;
   overriding
   function  Object_B    (Self : in     Ball) return physics.Object.view;

   overriding
   function  Frame_A     (Self : in     Ball) return Matrix_4x4;
   overriding
   function  Frame_B     (Self : in     Ball) return Matrix_4x4;

   overriding
   procedure Frame_A_is  (Self : in out Ball;   Now : in Matrix_4x4);
   overriding
   procedure Frame_B_is  (Self : in out Ball;   Now : in Matrix_4x4);

   overriding
   function  is_Limited  (Self : in     Ball;   DoF : Degree_of_freedom) return Boolean;

   overriding
   procedure Velocity_is (Self : in out Ball;   Now : in Real;
                                                DoF : in Degree_of_freedom);
   overriding
   function  Extent      (Self : in     Ball;   DoF : in Degree_of_freedom) return Real;
   overriding
   procedure desired_Extent_is (Self : in out Ball;   Now : in Real;
                                                      DoF : in Degree_of_freedom);
   overriding
   function  lower_Limit (Self : in     Ball;   DoF : in Degree_of_freedom) return Real;
   overriding
   function  upper_Limit (Self : in     Ball;   DoF : in Degree_of_freedom) return Real;

   overriding
   procedure lower_Limit_is (Self : in out Ball;   Now : in Real;
                                                   DoF : in Degree_of_freedom);
   overriding
   procedure upper_Limit_is (Self : in out Ball;   Now : in Real;
                                                   DoF : in Degree_of_freedom);


   ---------
   --  Hinge
   --
   type Hinge is new Item
                 and physics.Joint.hinge.item with
      record
         null;
      end record;

   overriding
   procedure destruct    (Self : in out Hinge);

   overriding
   function  Object_A    (Self : in     Hinge) return physics.Object.view;
   overriding
   function  Object_B    (Self : in     Hinge) return physics.Object.view;

   overriding
   function  Frame_A     (Self : in     Hinge) return Matrix_4x4;
   overriding
   function  Frame_B     (Self : in     Hinge) return Matrix_4x4;

   overriding
   procedure Frame_A_is  (Self : in out Hinge;   Now : in Matrix_4x4);
   overriding
   procedure Frame_B_is  (Self : in out Hinge;   Now : in Matrix_4x4);

   overriding
   function  is_Limited  (Self : in     Hinge;   DoF : Degree_of_freedom) return Boolean;

   overriding
   procedure Velocity_is (Self : in out Hinge;   Now : in Real;
                                                 DoF : in Degree_of_freedom);
   overriding
   function  Extent      (Self : in     Hinge;   DoF : in Degree_of_freedom) return Real;
   overriding
   procedure desired_Extent_is (Self : in out Hinge;   Now : in Real;
                                                       DoF : in Degree_of_freedom);
   overriding
   procedure Limits_are  (Self : in out Hinge;   Low, High        : in Real;
                                                 Softness         : in Real := 0.9;
                                                 biasFactor       : in Real := 0.3;
                                                 relaxationFactor : in Real := 1.0);
   overriding
   function  lower_Limit (Self : in     Hinge)   return Real;
   overriding
   function  upper_Limit (Self : in     Hinge)   return Real;

   overriding
   function  Angle       (Self : in     Hinge)   return Real;


end box2d_Physics.Joint;
