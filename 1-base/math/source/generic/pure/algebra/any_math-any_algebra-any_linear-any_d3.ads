generic
package any_math.any_Algebra.any_linear.any_d3
is
   pragma Pure;


   ------------
   --- Vector_3
   --

   function  Distance              (Self        : in Vector_3;   To : in Vector_3) return Real;
   function  Midpoint              (Self, Other : in Vector_3)                     return Vector_3;
   function  Interpolated          (v0,   v1    : in Vector_3;   rt : in Real)     return Vector_3;
   function  Angle_between_preNorm (U, V        : in Vector_3)    return Radians;
   --
   -- Given that the vectors 'U' and 'V' are already normalized, returns a positive angle between 0 and 180 degrees.

   function  Angle                 (Point_1,
                                    Point_2,
                                    Point_3     : in Vector_3) return Radians;
   --
   -- Returns the angle between the vector Point_1 to Point_2 and the vector Point_3 to Point_2.



   --------------
   --- Matrix_3x3
   --

   z_Up_to_y_Up : constant Matrix_3x3;    -- Provides a rotation which may be multiplied
   y_Up_to_z_Up : constant Matrix_3x3;    -- by a vector to change co-ordinate systems.


   function  to_Matrix         (Row_1,
                                Row_2,
                                Row_3 : in     Vector_3)   return Matrix_3x3;

   function  forward_Direction (Self  : in     Matrix_3x3) return Vector_3;
   function  up_Direction      (Self  : in     Matrix_3x3) return Vector_3;
   function  right_Direction   (Self  : in     Matrix_3x3) return Vector_3;

   procedure Re_Orthonormalize (Self  : in out Matrix_3x3);



   -------------
   --- Rotations
   --

   function x_Rotation_from (the_Angle : in Radians)    return Matrix_3x3;
   function y_Rotation_from (the_Angle : in Radians)    return Matrix_3x3;
   function z_Rotation_from (the_Angle : in Radians)    return Matrix_3x3;

   function xyz_Rotation    (x_Angle,
                             y_Angle,
                             z_Angle   : in Real)       return Matrix_3x3;

   function xyz_Rotation    (Angles    : in Vector_3)   return Matrix_3x3;

   function to_Rotation     (Axis           : in     Vector_3;
                             Angle          : in     Real) return Matrix_3x3;
   function to_Rotation     (Axis_x,
                             Axis_y,
                             Axis_z         : in     Real;
                             rotation_Angle : in     Real) return Matrix_3x3;
   --
   --  Returns a rotation matrix describing a given rotation about an axis.



   ---------
   --- Euler
   --

   type Euler is new Vector_3;
   --
   --  1: Roll
   --  2: Pitch
   --  3: Yaw


   function to_Rotation (Self : in Euler) return Matrix_3x3;
   --
   --  The euler angles are used to produce a rotation matrix. The euler
   --  angles are applied in ZYX order. That is, a vector is first rotated
   --  about X then Y and then Z.



   -----------
   --- General
   --

   function  Look_at               (Eye, Center, Up           : in Vector_3) return Matrix_4x4;
   function  to_Perspective        (FoVy, Aspect, zNear, zFar : in Real)     return Matrix_4x4;
   function  to_viewport_Transform (Origin, Extent            : in Vector_2) return Matrix_4x4;



   -------------
   --- Transform
   --

   function to_translation_Matrix (Translation : in Vector_3)       return Matrix_4x4;
   function to_Transform          (From        : in Matrix_4x4)     return Transform_3d;

   function "*" (Left : in Transform_3d;   Right : in Vector_3)     return Vector_3;
   function "*" (Left : in Vector_3;       Right : in Transform_3d) return Vector_3;

   function "*" (Left : in Transform_3d;   Right : in Transform_3d) return Transform_3d;
   function "*" (Left : in Vector_3;       Right : in Matrix_4x4)   return Vector_3;

   function Invert   (Self : in Transform_3d)                       return Transform_3d;
   function invXform (Self : in Transform_3d;   inVec : Vector_3)   return Vector_3;



   --------------
   --- Quaternion
   --

   procedure setFromMatrix3x3T (Self :    out Quaternion;   the_Matrix : in Matrix_3x3);

   function  to_Quaternion (Self : in Matrix_3x3) return Quaternion;
   function  to_Matrix     (Self : in Quaternion) return Matrix_3x3;

   function  Norm     (Self : in Quaternion) return Real;
   function  Versor   (Self : in Quaternion) return Quaternion;               -- Produces the unit quaternion of Self.

   function  Farthest (Self : in Quaternion;   qd : in Quaternion) return Quaternion;
   function  Invert   (Self : in Quaternion) return Quaternion;

   function  Angle    (Self : in Quaternion) return Real;
   function  Axis     (Self : in Quaternion) return Vector_3;

   function  "*"      (Left, Right : in Quaternion) return Real;              -- Dot   product.
   function  "*"      (Left, Right : in Quaternion) return Quaternion;        -- Cross product.


   function  "+"      (Left, Right : in Quaternion) return Quaternion;
   function  "-"      (Left, Right : in Quaternion) return Quaternion;
   function  "-"      (Self        : in Quaternion) return Quaternion;

   function  "*"      (Left : in Quaternion;      Right : in Vector_3)   return Quaternion;
   function  "*"      (Left : in Vector_3;        Right : in Quaternion) return Quaternion;

   function  "*"      (Left : in Quaternion;      Right : in Real)       return Quaternion;

   function  Slerp    (Initial,
                       Desired : in Quaternion;   Time  : in Real)       return Quaternion;



   ------------
   --- Vector_4
   --

   function "/"          (Left,
                          Right : in Vector_4)                 return Vector_4;

   function maxAxis4     (Self  : in Vector_4)                 return Integer;
   function closestAxis4 (Self  : in Vector_4)                 return Integer;


   function  to_transform_Matrix (Self        : in Transform_3d) return Matrix_4x4;

   function  to_transform_Matrix (Rotation    : in Matrix_3x3;
                                  Translation : in Vector_3)   return Matrix_4x4;
   function  to_rotate_Matrix    (Rotation    : in Matrix_3x3) return Matrix_4x4;
   function  to_translate_Matrix (Translation : in Vector_3)   return Matrix_4x4;
   function  to_scale_Matrix     (Scale       : in Vector_3)   return Matrix_4x4;



   ----------------------
   --- Transform Matrices
   --

   function  get_Rotation      (Self : in     Matrix_4x4)    return Matrix_3x3;
   procedure set_Rotation      (Self : in out Matrix_4x4;   To : in Matrix_3x3);

   function  get_Translation   (Self : in     Matrix_4x4)    return Vector_3;
   procedure set_Translation   (Self : in out Matrix_4x4;   To : in Vector_3);

   function  inverse_Rotation  ( rotation_Matrix : in Matrix_3x3) return Matrix_3x3;
   function  inverse_Transform (transform_Matrix : in Matrix_4x4) return Matrix_4x4;



   --------------
   --- un-Project
   --

   type Rectangle is
      record
         Min : Integers (1 .. 2);   -- Bottom left corner.
         Max : Integers (1 .. 2);   -- Upper right corner.
      end record;

   function  unProject (Win         : in Vector_3;
                        modelMatrix : in Matrix_4x4;
                        projMatrix  : in Matrix_4x4;
                        viewport    : in Rectangle) return Vector_3;





private

   z_Up_to_y_Up : constant Matrix_3x3 := ((1.0,  0.0,  0.0),
                                          (0.0,  0.0,  1.0),
                                          (0.0, -1.0,  0.0));

   y_Up_to_z_Up : constant Matrix_3x3 := ((1.0,  0.0,  0.0),
                                          (0.0,  0.0, -1.0),
                                          (0.0,  1.0,  0.0));

   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");

end any_math.any_Algebra.any_linear.any_d3;
