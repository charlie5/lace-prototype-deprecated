generic
package any_math.any_Algebra.any_linear.any_d3
is
   pragma Pure;

   ------------
   --- Vector_3
   --

   function  Distance     (From, To : in Vector_3)   return Real;
   function  Midpoint     (From, To : in Vector_3)   return Vector_3;
   function  Interpolated (From, To : in Vector_3;
                           Percent  : in unit_Percentage) return Vector_3;

   function  Angle_between_pre_Norm (U, V : in Vector_3) return Radians;
   --
   -- Given that the vectors 'U' and 'V' are already normalized, returns a positive angle between 0 and 180 degrees.

   function  Angle (Point_1,
                    Point_2,
                    Point_3 : in Vector_3) return Radians;
   --
   -- Returns the angle between the vector Point_1 to Point_2 and the vector Point_3 to Point_2.


   --------------
   --- Matrix_3x3
   --

   z_Up_to_y_Up : constant Matrix_3x3;    -- Provides a rotation which may be multiplied
   y_Up_to_z_Up : constant Matrix_3x3;    -- by a vector to change co-ordinate systems.


   function  to_Matrix         (Row_1,
                                Row_2,
                                Row_3   : in     Vector_3)   return Matrix_3x3;

   function  forward_Direction (Matrix  : in     Matrix_3x3) return Vector_3;
   function  up_Direction      (Matrix  : in     Matrix_3x3) return Vector_3;
   function  right_Direction   (Matrix  : in     Matrix_3x3) return Vector_3;

   procedure re_Orthonormalise (Matrix  : in out Matrix_3x3);


   -------------
   --- Rotations
   --

   function x_Rotation_from (Angle : in Radians)  return Matrix_3x3;
   function y_Rotation_from (Angle : in Radians)  return Matrix_3x3;
   function z_Rotation_from (Angle : in Radians)  return Matrix_3x3;

   function xyz_Rotation (x_Angle,
                          y_Angle,
                          z_Angle  : in Real)     return Matrix_3x3;

   function xyz_Rotation (Angles   : in Vector_3) return Matrix_3x3;

   function to_Rotation  (Axis     : in Vector_3;
                          Angle    : in Real)     return Matrix_3x3;
   function to_Rotation  (Axis_x,
                          Axis_y,
                          Axis_z   : in Real;
                          Rotation : in Radians)  return Matrix_3x3;
   --
   --  Returns a rotation matrix describing a given rotation about an axis.
   -- (TODO: Make this obsolescent and use the vector Axis version instead.)


   ---------
   --- Euler
   --

   type Euler is new Vector_3;
   --
   --  1: Roll
   --  2: Pitch
   --  3: Yaw


   function to_Rotation (Angles : in Euler) return Matrix_3x3;
   --
   --  The euler angles are used to produce a rotation matrix. The euler
   --  angles are applied in ZYX order. That is, a vector is first rotated
   --  about X, then Y and then Z.


   -----------
   --- General
   --

   function Look_at (Eye,
                     Center,
                     Up     : in Vector_3) return Matrix_4x4;

   function to_Viewport_Transform (Origin,
                                   Extent : in Vector_2) return Matrix_4x4;

   function to_Perspective (FoVy   : in Degrees;
                            Aspect,
                            zNear,
                            zFar   : in Real) return Matrix_4x4;


   -------------
   --- Transform
   --

   function to_Translation_Matrix (Translation : in Vector_3)       return Matrix_4x4;
   function to_Transform          (Matrix      : in Matrix_4x4)     return Transform_3d;

   function "*" (Left : in Transform_3d;   Right : in Vector_3)     return Vector_3;
   function "*" (Left : in Vector_3;       Right : in Transform_3d) return Vector_3;

   function "*" (Left : in Transform_3d;   Right : in Transform_3d) return Transform_3d;
   function "*" (Left : in Vector_3;       Right : in Matrix_4x4)   return Vector_3;

   function Invert            (Transform : in Transform_3d)                         return Transform_3d;
   function inverse_Transform (Transform : in Transform_3d;   Vector : in Vector_3) return Vector_3;


   --------------
   --- Quaternion
   --

   procedure set_from_Matrix_3x3 (Quat :    out Quaternion;   Matrix : in Matrix_3x3);

   function  to_Quaternion (Matrix : in Matrix_3x3) return Quaternion;
   function  to_Matrix     (Quat   : in Quaternion) return Matrix_3x3;

   function  Norm     (Quat : in Quaternion) return Real;
   function  Versor   (Quat : in Quaternion) return Quaternion;               -- Produces the unit quaternion of Quat.

   function  Farthest (Quat : in Quaternion;   qd : in Quaternion) return Quaternion;   -- TODO: Document this.
   function  Invert   (Quat : in Quaternion)    return Quaternion;

   function  Angle    (Quat : in Quaternion) return Radians;
   function  Axis     (Quat : in Quaternion) return Vector_3;

   function  "*"      (Left, Right : in Quaternion) return Real;              -- Dot   product.
   function  "*"      (Left, Right : in Quaternion) return Quaternion;        -- Cross product.

   function  "+"      (Left, Right : in Quaternion) return Quaternion;
   function  "-"      (Left, Right : in Quaternion) return Quaternion;
   function  "-"      (Quat        : in Quaternion) return Quaternion;

   function  "*"      (Left  : in Quaternion;   Right   : in Vector_3)   return Quaternion;
   function  "*"      (Left  : in Vector_3;     Right   : in Quaternion) return Quaternion;

   function  "*"      (Left  : in Quaternion;   Right   : in Real)       return Quaternion;

   function  Interpolated (From,
                           To      : in Quaternion;
                           Percent : in unit_Percentage) return Quaternion;
   --
   -- Return the quaternion which is the result of spherical linear interpolation (Slerp) between Initial and Final.
   -- Percent is the ratio between 'From' and 'To' to interpolate.
   -- If Percent =   0.0 the result is Initial.
   -- If Percent = 100.0 the result is Final.
   -- Interpolates assuming constant velocity.


   ------------
   --- Vector_4
   --

   function  "/" (Left, Right     : in Vector_4) return Vector_4;

   function  max_Axis     (Vector : in Vector_4) return Integer;
   function  closest_Axis (Vector : in Vector_4) return Integer;

   function  to_transform_Matrix (Transform   : in Transform_3d) return Matrix_4x4;
   function  to_transform_Matrix (Rotation    : in Matrix_3x3;
                                  Translation : in Vector_3)     return Matrix_4x4;
   function  to_rotate_Matrix    (Rotation    : in Matrix_3x3)   return Matrix_4x4;
   function  to_translate_Matrix (Translation : in Vector_3)     return Matrix_4x4;
   function  to_scale_Matrix     (Scale       : in Vector_3)     return Matrix_4x4;


   ----------------------
   --- Transform Matrices
   --

   function  get_Rotation      (Transform : in     Matrix_4x4)    return Matrix_3x3;
   procedure set_Rotation      (Transform : in out Matrix_4x4;   To : in Matrix_3x3);

   function  get_Translation   (Transform : in     Matrix_4x4)    return Vector_3;
   procedure set_Translation   (Transform : in out Matrix_4x4;   To : in Vector_3);

   function  inverse_Rotation  (Rotation  : in     Matrix_3x3) return Matrix_3x3;
   function  inverse_Transform (Transform : in     Matrix_4x4) return Matrix_4x4;


   --------------
   --- un-Project
   --

   type Rectangle is
      record
         Min : Integers (1 .. 2);   -- Bottom left corner.
         Max : Integers (1 .. 2);   -- Upper right corner.
      end record;

   function unProject (From       : in Vector_3;
                       Model      : in Matrix_4x4;
                       Projection : in Matrix_4x4;
                       Viewport   : in Rectangle) return Vector_3;
   --
   -- Maps the 'From' window space coordinates into object space coordinates using Model, Projection and Viewport.



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
