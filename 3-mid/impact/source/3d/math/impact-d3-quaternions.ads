

package impact.d3.Quaternions
--
--  The impact.d3.Quaternion provides subprograms to perform linear algebra rotations in combination with impact.d3.Matrix, impact.d3.Vector and impact.d3.Transform.
--
is
   use Math;

   BT_EULER_DEFAULT_ZYX : constant Boolean := False;


   function to_Quaternion (x, y, z, w : in Real               ) return Quaternion;
   function to_Quaternion (Axis : in Vector_3;   Angle : in Real) return Quaternion;

   function to_Quaternion (Yaw, Pitch, Roll : in Real         ) return Quaternion;
   --
   --  yaw Angle   around Y unless BT_EULER_DEFAULT_ZYX is true then Z
   --  pitch Angle around X unless BT_EULER_DEFAULT_ZYX is true then Y
   --  roll Angle  around Z unless BT_EULER_DEFAULT_ZYX is true then X



   function x (Self : in Quaternion) return Real;
   function y (Self : in Quaternion) return Real;
   function z (Self : in Quaternion) return Real;
   function w (Self : in Quaternion) return Real;



   procedure setRotation (Self : out Quaternion;   Axis  : in Vector_3;
                                                   Angle : in Real);



   procedure setEuler (Self : out Quaternion;   Yaw, Pitch, Roll : in Real);
   --
   --  yaw   Angle around Y
   --  pitch Angle around X
   --  roll  Angle around Z


   procedure setEulerZYX (Self : out Quaternion;   Yaw, Pitch, Roll : in Real);
   --
   --  yaw   Angle around Z
   --  pitch Angle around Y
   --  roll  Angle around X



   function multiply (Left, Right : in Quaternion) return Quaternion;


   function dot (Left, Right : in Quaternion) return Real;

   function length2 (Self : in Quaternion) return Real;   -- Return the length squared of the quaternion.
   function length  (Self : in Quaternion) return Real;   -- Return the length         of the quaternion.


   function  normalize  (Self : access Quaternion) return Quaternion;   -- Normalize the quaternion such that  x^2 + y^2 + z^2 +w^2 = 1  and return it.
   function  normalized (Self : in     Quaternion) return Quaternion;

   procedure normalize  (Self : in out Quaternion);


--     function "*" (Left : in Quaternion;   Right : in Real) return Quaternion;   -- Scale a quaternion.
--     function "/" (Left : in Quaternion;   Right : in Real) return Quaternion;   -- Inversely scale a quaternion.


   function Angle    (Left, Right : in Quaternion) return Real;                   -- Return the angle between Left and Right.

   function getAngle (Self : in Quaternion) return Real;                   -- Return the angle of rotation represented by Self.
   function getAxis  (Self : in Quaternion) return Vector_3;                 -- Return the axis of the rotation represented by Self.


   function inverse  (Self : in Quaternion) return Quaternion;               -- Return the inverse of Self.


--     function "+" (Left, Right : in Quaternion) return Quaternion;
--     function "-" (Left, Right : in Quaternion) return Quaternion;

   function "-" (Self : in Quaternion) return Quaternion;                       -- Return the negative of this quaternion


   function farthest (Self : in Quaternion;   qd : in Quaternion) return Quaternion;
   function nearest  (Self : in Quaternion;   qd : in Quaternion) return Quaternion;


   function slerp  (Self : in Quaternion;   q : in Quaternion;
                                            t : in Real  ) return Quaternion;
   --
   --  Return the quaternion which is the result of Spherical Linear Interpolation between Self and q.
   --  Slerp interpolates assuming constant velocity.
   --
   --  t   The ratio between Self and q to interpolate.
   --     If t = 0.0 the result is Self, if t=1.0 the result is q.



   function getIdentity return Quaternion;



   function "*" (Left : in Quaternion;   Right : in Vector_3)   return Quaternion;
   function "*" (Left : in Vector_3;     Right : in Quaternion) return Quaternion;




   function quatRotate (rotation : in Quaternion;   v : in Vector_3) return Vector_3;

   function shortestArcQuat           (v1, v2 : in     Vector_3) return Quaternion;   -- Game Programming Gems 2.10.   Make sure v1,v2 are normalized.
   function shortestArcQuatNormalize2 (v1, v2 : access Vector_3) return Quaternion;


end impact.d3.Quaternions;
