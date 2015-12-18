-- This file is generated by SWIG. Please do *not* modify by hand.
--
with bullet_c.Pointers;
with bullet_c.ray_Collision;
with c_math_c;
with c_math_c.Matrix_3x3;
with c_math_c.Matrix_4x4;
with c_math_c.Pointers;
with c_math_c.Triangle;
with c_math_c.Vector_2;
with c_math_c.Vector_3;
with Interfaces.C;
with Swig;
with Interfaces.C;

package bullet_c.Binding is

   function b3d_new_Box
     (half_Extents : in c_math_c.Vector_3.Pointer)
      return bullet_c.Pointers.Shape_Pointer;

   function b3d_new_Capsule
     (Radii  : in c_math_c.Vector_2.Pointer;
      Height : in c_math_c.Real) return bullet_c.Pointers.Shape_Pointer;

   function b3d_new_Cone
     (Radius : in c_math_c.Real;
      Height : in c_math_c.Real) return bullet_c.Pointers.Shape_Pointer;

   function b3d_new_convex_Hull
     (Points      : in c_math_c.Vector_3.Pointer;
      point_Count : in Interfaces.C.int)
      return bullet_c.Pointers.Shape_Pointer;

   function b3d_new_Mesh
     (Points         : in c_math_c.Vector_3.Pointer;
      point_Count    : in Interfaces.C.int;
      Triangles      : in c_math_c.Triangle.Pointer;
      triangle_Count : in Interfaces.C.int)
      return bullet_c.Pointers.Shape_Pointer;

   function b3d_new_Cylinder
     (half_Extents : in c_math_c.Vector_3.Pointer)
      return bullet_c.Pointers.Shape_Pointer;

   function b3d_new_Heightfield
     (Width      : in Interfaces.C.int;
      Depth      : in Interfaces.C.int;
      Heights    : in c_math_c.Pointers.Real_Pointer;
      min_Height : in c_math_c.Real;
      max_Height : in c_math_c.Real;
      Scale      : in c_math_c.Vector_3.Pointer)
      return bullet_c.Pointers.Shape_Pointer;

   function b3d_new_multiSphere
     (Positions    : in c_math_c.Vector_3.Pointer;
      Radii        : in c_math_c.Pointers.Real_Pointer;
      sphere_Count : in Interfaces.C.int)
      return bullet_c.Pointers.Shape_Pointer;

   function b3d_new_Plane
     (Normal : in c_math_c.Vector_3.Pointer;
      Offset : in c_math_c.Real) return bullet_c.Pointers.Shape_Pointer;

   function b3d_new_Sphere
     (Radius : in c_math_c.Real) return bullet_c.Pointers.Shape_Pointer;

   function b3d_Shape_user_Data
     (Self : in bullet_c.Pointers.Shape_Pointer) return Swig.void_ptr;

   procedure b3d_Shape_user_Data_is
     (Self : in bullet_c.Pointers.Shape_Pointer;
      Now  : in Swig.void_ptr);

   function b3d_new_Object
     (Mass         : in c_math_c.Real;
      the_Shape    : in bullet_c.Pointers.Shape_Pointer;
      is_Kinematic : in Interfaces.C.int)
      return bullet_c.Pointers.Object_Pointer;

   function b3d_Object_Shape
     (Self : in bullet_c.Pointers.Object_Pointer)
      return bullet_c.Pointers.Shape_Pointer;

   function b3d_Object_user_Data
     (Self : in bullet_c.Pointers.Object_Pointer) return Swig.void_ptr;

   procedure b3d_Object_user_Data_is
     (Self : in bullet_c.Pointers.Object_Pointer;
      Now  : in Swig.void_ptr);

   function b3d_Object_Mass
     (Self : in bullet_c.Pointers.Object_Pointer) return c_math_c.Real;

   procedure b3d_Object_Friction_is
     (Self : in bullet_c.Pointers.Object_Pointer;
      Now  : in c_math_c.Real);

   procedure b3d_Object_Restitution_is
     (Self : in bullet_c.Pointers.Object_Pointer;
      Now  : in c_math_c.Real);

   function b3d_Object_Site
     (Self : in bullet_c.Pointers.Object_Pointer)
      return c_math_c.Vector_3.Item;

   procedure b3d_Object_Site_is
     (Self : in bullet_c.Pointers.Object_Pointer;
      Now  : in c_math_c.Vector_3.Pointer);

   function b3d_Object_Spin
     (Self : in bullet_c.Pointers.Object_Pointer)
      return c_math_c.Matrix_3x3.Item;

   procedure b3d_Object_Spin_is
     (Self : in bullet_c.Pointers.Object_Pointer;
      Now  : in c_math_c.Matrix_3x3.Pointer);

   function b3d_Object_Transform
     (Self : in bullet_c.Pointers.Object_Pointer)
      return c_math_c.Matrix_4x4.Item;

   procedure b3d_Object_Transform_is
     (Self : in bullet_c.Pointers.Object_Pointer;
      Now  : in c_math_c.Matrix_4x4.Pointer);

   function b3d_Object_Speed
     (Self : in bullet_c.Pointers.Object_Pointer)
      return c_math_c.Vector_3.Item;

   procedure b3d_Object_Speed_is
     (Self : in bullet_c.Pointers.Object_Pointer;
      Now  : in c_math_c.Vector_3.Pointer);

   function b3d_Object_Gyre
     (Self : in bullet_c.Pointers.Object_Pointer)
      return c_math_c.Vector_3.Item;

   procedure b3d_Object_Gyre_is
     (Self : in bullet_c.Pointers.Object_Pointer;
      Now  : in c_math_c.Vector_3.Pointer);

   procedure b3d_Object_apply_Force
     (Self  : in bullet_c.Pointers.Object_Pointer;
      Force : in c_math_c.Vector_3.Pointer);

   procedure b3d_Object_apply_Torque
     (Self   : in bullet_c.Pointers.Object_Pointer;
      Torque : in c_math_c.Vector_3.Pointer);

   procedure b3d_Object_apply_Torque_impulse
     (Self   : in bullet_c.Pointers.Object_Pointer;
      Torque : in c_math_c.Vector_3.Pointer);

   function b3d_new_hinge_Joint
     (Object_A : in bullet_c.Pointers.Object_Pointer;
      Object_B : in bullet_c.Pointers.Object_Pointer;
      Frame_A  : in c_math_c.Matrix_4x4.Pointer;
      Frame_B  : in c_math_c.Matrix_4x4.Pointer)
      return bullet_c.Pointers.Joint_Pointer;

   function b3d_new_space_hinge_Joint
     (Object_A : in bullet_c.Pointers.Object_Pointer;
      Frame_A  : in c_math_c.Matrix_4x4.Pointer)
      return bullet_c.Pointers.Joint_Pointer;

   function b3d_new_DoF6_Joint
     (Object_A : in bullet_c.Pointers.Object_Pointer;
      Object_B : in bullet_c.Pointers.Object_Pointer;
      Frame_A  : in c_math_c.Matrix_4x4.Pointer;
      Frame_B  : in c_math_c.Matrix_4x4.Pointer)
      return bullet_c.Pointers.Joint_Pointer;

   function b3d_new_cone_twist_Joint
     (Object_A : in bullet_c.Pointers.Object_Pointer;
      Object_B : in bullet_c.Pointers.Object_Pointer;
      Frame_A  : in c_math_c.Matrix_4x4.Pointer;
      Frame_B  : in c_math_c.Matrix_4x4.Pointer)
      return bullet_c.Pointers.Joint_Pointer;

   function b3d_new_slider_Joint
     (Object_A : in bullet_c.Pointers.Object_Pointer;
      Object_B : in bullet_c.Pointers.Object_Pointer;
      Frame_A  : in c_math_c.Matrix_4x4.Pointer;
      Frame_B  : in c_math_c.Matrix_4x4.Pointer)
      return bullet_c.Pointers.Joint_Pointer;

   function b3d_new_ball_Joint
     (Object_A   : in bullet_c.Pointers.Object_Pointer;
      Object_B   : in bullet_c.Pointers.Object_Pointer;
      Pivot_in_A : in c_math_c.Vector_3.Pointer;
      Pivot_in_B : in c_math_c.Vector_3.Pointer)
      return bullet_c.Pointers.Joint_Pointer;

   function b3d_Joint_user_Data
     (Self : in bullet_c.Pointers.Joint_Pointer) return Swig.void_ptr;

   procedure b3d_Joint_user_Data_is
     (Self : in bullet_c.Pointers.Joint_Pointer;
      Now  : in Swig.void_ptr);

   function b3d_Joint_Object_A
     (Self : in bullet_c.Pointers.Joint_Pointer)
      return bullet_c.Pointers.Object_Pointer;

   function b3d_Joint_Object_B
     (Self : in bullet_c.Pointers.Joint_Pointer)
      return bullet_c.Pointers.Object_Pointer;

   function b3d_Joint_Frame_A
     (Self : in bullet_c.Pointers.Joint_Pointer)
      return c_math_c.Matrix_4x4.Item;

   function b3d_Joint_Frame_B
     (Self : in bullet_c.Pointers.Joint_Pointer)
      return c_math_c.Matrix_4x4.Item;

   procedure b3d_Joint_Frame_A_is
     (Self : in bullet_c.Pointers.Joint_Pointer;
      Now  : in c_math_c.Matrix_4x4.Pointer);

   procedure b3d_Joint_Frame_B_is
     (Self : in bullet_c.Pointers.Joint_Pointer;
      Now  : in c_math_c.Matrix_4x4.Pointer);

   function b3d_Joint_is_Limited
     (Self : in bullet_c.Pointers.Joint_Pointer;
      DoF  : in Interfaces.C.int) return Swig.bool;

   function b3d_Joint_Extent
     (Self : in bullet_c.Pointers.Joint_Pointer;
      DoF  : in Interfaces.C.int) return Swig.bool;

   procedure b3d_Joint_Velocity_is
     (Self : in bullet_c.Pointers.Joint_Pointer;
      DoF  : in Interfaces.C.int;
      Now  : in c_math_c.Real);

   procedure b3d_Joint_hinge_Limits_are
     (Self              : in bullet_c.Pointers.Joint_Pointer;
      Lower             : in c_math_c.Real;
      Upper             : in c_math_c.Real;
      Softeness         : in c_math_c.Real;
      bias_Factor       : in c_math_c.Real;
      relaxation_Factor : in c_math_c.Real);

   procedure b3d_Joint_6DoF_lower_Limit_is
     (Self : in bullet_c.Pointers.Joint_Pointer;
      DoF  : in Interfaces.C.int;
      Now  : in c_math_c.Real);

   procedure b3d_Joint_6DoF_upper_Limit_is
     (Self : in bullet_c.Pointers.Joint_Pointer;
      DoF  : in Interfaces.C.int;
      Now  : in c_math_c.Real);

   function b3d_Joint_6DoF_lower_Limit
     (Self : in bullet_c.Pointers.Joint_Pointer;
      DoF  : in Interfaces.C.int) return c_math_c.Real;

   function b3d_Joint_6DoF_upper_Limit
     (Self : in bullet_c.Pointers.Joint_Pointer;
      DoF  : in Interfaces.C.int) return c_math_c.Real;

   function b3d_new_Space return bullet_c.Pointers.Space_Pointer;

   procedure b3d_Space_add_Object
     (Self       : in bullet_c.Pointers.Space_Pointer;
      the_Object : in bullet_c.Pointers.Object_Pointer);

   procedure b3d_Space_rid_Object
     (Self       : in bullet_c.Pointers.Space_Pointer;
      the_Object : in bullet_c.Pointers.Object_Pointer);

   procedure b3d_Space_add_Joint
     (Self      : in bullet_c.Pointers.Space_Pointer;
      the_Joint : in bullet_c.Pointers.Joint_Pointer);

   procedure b3d_Space_Gravity_is
     (Self : in bullet_c.Pointers.Space_Pointer;
      Now  : in c_math_c.Vector_3.Pointer);

   procedure b3d_Space_evolve
     (Self : in bullet_c.Pointers.Space_Pointer;
      By   : in Interfaces.C.C_float);

   function b3d_Space_cast_Ray
     (Self : in bullet_c.Pointers.Space_Pointer;
      From : in c_math_c.Vector_3.Pointer;
      To   : in c_math_c.Vector_3.Pointer) return bullet_c.ray_Collision.Item;

private

   pragma Import (C, b3d_new_Box, "Ada_b3d_new_Box");
   pragma Import (C, b3d_new_Capsule, "Ada_b3d_new_Capsule");
   pragma Import (C, b3d_new_Cone, "Ada_b3d_new_Cone");
   pragma Import (C, b3d_new_convex_Hull, "Ada_b3d_new_convex_Hull");
   pragma Import (C, b3d_new_Mesh, "Ada_b3d_new_Mesh");
   pragma Import (C, b3d_new_Cylinder, "Ada_b3d_new_Cylinder");
   pragma Import (C, b3d_new_Heightfield, "Ada_b3d_new_Heightfield");
   pragma Import (C, b3d_new_multiSphere, "Ada_b3d_new_multiSphere");
   pragma Import (C, b3d_new_Plane, "Ada_b3d_new_Plane");
   pragma Import (C, b3d_new_Sphere, "Ada_b3d_new_Sphere");
   pragma Import (C, b3d_Shape_user_Data, "Ada_b3d_Shape_user_Data");
   pragma Import (C, b3d_Shape_user_Data_is, "Ada_b3d_Shape_user_Data_is");
   pragma Import (C, b3d_new_Object, "Ada_b3d_new_Object");
   pragma Import (C, b3d_Object_Shape, "Ada_b3d_Object_Shape");
   pragma Import (C, b3d_Object_user_Data, "Ada_b3d_Object_user_Data");
   pragma Import (C, b3d_Object_user_Data_is, "Ada_b3d_Object_user_Data_is");
   pragma Import (C, b3d_Object_Mass, "Ada_b3d_Object_Mass");
   pragma Import (C, b3d_Object_Friction_is, "Ada_b3d_Object_Friction_is");
   pragma Import
     (C,
      b3d_Object_Restitution_is,
      "Ada_b3d_Object_Restitution_is");
   pragma Import (C, b3d_Object_Site, "Ada_b3d_Object_Site");
   pragma Import (C, b3d_Object_Site_is, "Ada_b3d_Object_Site_is");
   pragma Import (C, b3d_Object_Spin, "Ada_b3d_Object_Spin");
   pragma Import (C, b3d_Object_Spin_is, "Ada_b3d_Object_Spin_is");
   pragma Import (C, b3d_Object_Transform, "Ada_b3d_Object_Transform");
   pragma Import (C, b3d_Object_Transform_is, "Ada_b3d_Object_Transform_is");
   pragma Import (C, b3d_Object_Speed, "Ada_b3d_Object_Speed");
   pragma Import (C, b3d_Object_Speed_is, "Ada_b3d_Object_Speed_is");
   pragma Import (C, b3d_Object_Gyre, "Ada_b3d_Object_Gyre");
   pragma Import (C, b3d_Object_Gyre_is, "Ada_b3d_Object_Gyre_is");
   pragma Import (C, b3d_Object_apply_Force, "Ada_b3d_Object_apply_Force");
   pragma Import (C, b3d_Object_apply_Torque, "Ada_b3d_Object_apply_Torque");
   pragma Import
     (C,
      b3d_Object_apply_Torque_impulse,
      "Ada_b3d_Object_apply_Torque_impulse");
   pragma Import (C, b3d_new_hinge_Joint, "Ada_b3d_new_hinge_Joint");
   pragma Import
     (C,
      b3d_new_space_hinge_Joint,
      "Ada_b3d_new_space_hinge_Joint");
   pragma Import (C, b3d_new_DoF6_Joint, "Ada_b3d_new_DoF6_Joint");
   pragma Import (C, b3d_new_cone_twist_Joint, "Ada_b3d_new_cone_twist_Joint");
   pragma Import (C, b3d_new_slider_Joint, "Ada_b3d_new_slider_Joint");
   pragma Import (C, b3d_new_ball_Joint, "Ada_b3d_new_ball_Joint");
   pragma Import (C, b3d_Joint_user_Data, "Ada_b3d_Joint_user_Data");
   pragma Import (C, b3d_Joint_user_Data_is, "Ada_b3d_Joint_user_Data_is");
   pragma Import (C, b3d_Joint_Object_A, "Ada_b3d_Joint_Object_A");
   pragma Import (C, b3d_Joint_Object_B, "Ada_b3d_Joint_Object_B");
   pragma Import (C, b3d_Joint_Frame_A, "Ada_b3d_Joint_Frame_A");
   pragma Import (C, b3d_Joint_Frame_B, "Ada_b3d_Joint_Frame_B");
   pragma Import (C, b3d_Joint_Frame_A_is, "Ada_b3d_Joint_Frame_A_is");
   pragma Import (C, b3d_Joint_Frame_B_is, "Ada_b3d_Joint_Frame_B_is");
   pragma Import (C, b3d_Joint_is_Limited, "Ada_b3d_Joint_is_Limited");
   pragma Import (C, b3d_Joint_Extent, "Ada_b3d_Joint_Extent");
   pragma Import (C, b3d_Joint_Velocity_is, "Ada_b3d_Joint_Velocity_is");
   pragma Import
     (C,
      b3d_Joint_hinge_Limits_are,
      "Ada_b3d_Joint_hinge_Limits_are");
   pragma Import
     (C,
      b3d_Joint_6DoF_lower_Limit_is,
      "Ada_b3d_Joint_6DoF_lower_Limit_is");
   pragma Import
     (C,
      b3d_Joint_6DoF_upper_Limit_is,
      "Ada_b3d_Joint_6DoF_upper_Limit_is");
   pragma Import
     (C,
      b3d_Joint_6DoF_lower_Limit,
      "Ada_b3d_Joint_6DoF_lower_Limit");
   pragma Import
     (C,
      b3d_Joint_6DoF_upper_Limit,
      "Ada_b3d_Joint_6DoF_upper_Limit");
   pragma Import (C, b3d_new_Space, "Ada_b3d_new_Space");
   pragma Import (C, b3d_Space_add_Object, "Ada_b3d_Space_add_Object");
   pragma Import (C, b3d_Space_rid_Object, "Ada_b3d_Space_rid_Object");
   pragma Import (C, b3d_Space_add_Joint, "Ada_b3d_Space_add_Joint");
   pragma Import (C, b3d_Space_Gravity_is, "Ada_b3d_Space_Gravity_is");
   pragma Import (C, b3d_Space_evolve, "Ada_b3d_Space_evolve");
   pragma Import (C, b3d_Space_cast_Ray, "Ada_b3d_Space_cast_Ray");

end bullet_c.Binding;
