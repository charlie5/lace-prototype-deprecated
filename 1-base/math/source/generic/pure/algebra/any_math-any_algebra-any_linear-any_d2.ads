generic
package any_math.any_Algebra.any_linear.any_d2
is
   pragma Pure;


   ------------
   --  Vector_2
   --

   function  Distance                 (Self        : in Vector_2;   To : in Vector_2) return Real;
   function  Midpoint                 (Self, Other : in Vector_2)                     return Vector_2;
   function  Interpolated             (v0,   v1    : in Vector_2;   rt : in Real)     return Vector_2;
   function  angle_Between_preNorm    (U           : in Vector_2;   V  : in Vector_2) return Real;



   --------------
   --  Matrix_2x2
   --
   function           to_Matrix       (Row_1,
                                       Row_2       : in Vector_2)   return Matrix_2x2;
   function  to_rotation_Matrix       (Angle       : in Radians)    return Matrix_2x2;

   function  up_Direction             (Self        : in Matrix_2x2) return Vector_2;
   function  right_Direction          (Self        : in Matrix_2x2) return Vector_2;



   -------------
   --  Transform
   --

   function              to_Transform (Rotation    : in Matrix_2x2;
                                       Translation : in Vector_2)     return Matrix_3x3;
   function              to_Transform (From        : in Transform_2d) return Matrix_3x3;
   function  to_translation_Transform (Translation : in Vector_2)     return Matrix_3x3;
   function     to_rotation_Transform (Rotation    : in Matrix_2x2)   return Matrix_3x3;
   function     to_rotation_Transform (Angle       : in Radians )     return Matrix_3x3;
   function        to_scale_Transform (Scale       : in Vector_2)     return Matrix_3x3;


   function  to_Transform_2d          (From        : in Matrix_3x3)   return Transform_2d;
   function  to_Transform_2d          (Rotation    : in Radians;
                                       Translation : in Vector_2)     return Transform_2d;


   function  "*" (Left : in Vector_2;       Right : in Transform_2d)  return Vector_2;
   function  "*" (Left : in Vector_2;       Right : in Matrix_3x3)    return Vector_2;


   function  Invert                   (Self : in Transform_2d)                     return Transform_2d;
   function  invXform                 (Self : in Transform_2d;   inVec : Vector_2) return Vector_2;


   function  get_Rotation             (Self : in     Matrix_3x3)      return Matrix_2x2;
   procedure set_Rotation             (Self : in out Matrix_3x3;     To : in Matrix_2x2);

   function  get_Translation          (Self : in     Matrix_3x3)      return Vector_2;
   procedure set_Translation          (Self : in out Matrix_3x3;     To : in Vector_2);

end any_math.any_Algebra.any_linear.any_d2;
