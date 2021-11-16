generic
package any_Math.any_Algebra.any_linear.any_d2
is
   pragma Pure;

   -----------
   -- Vector_2
   --
   function  Interpolated           (From, To : in Vector_2;   Percent : in unit_Percentage) return Vector_2;
   function  Distance               (From, To : in Vector_2) return Real;
   function  Midpoint               (From, To : in Vector_2) return Vector_2;
   function  Angle_between_pre_Norm (U, V     : in Vector_2) return Radians;
   --
   -- Given that the vectors 'U' and 'V' are already normalized, returns a positive angle between 0 and 180 degrees.


   -------------
   -- Matrix_2x2
   --
   function  to_Matrix          (Row_1,
                                 Row_2 : in Vector_2)   return Matrix_2x2;
   function  to_rotation_Matrix (Angle : in Radians)    return Matrix_2x2;

   function  up_Direction       (Self  : in Matrix_2x2) return Vector_2;
   function  right_Direction    (Self  : in Matrix_2x2) return Vector_2;


   ------------
   -- Transform
   --
   function              to_Transform (Rotation    : in Matrix_2x2;
                                       Translation : in Vector_2)     return Matrix_3x3;
   function              to_Transform (From        : in Transform_2d) return Matrix_3x3;
   function  to_translation_Transform (Translation : in Vector_2)     return Matrix_3x3;
   function     to_rotation_Transform (Rotation    : in Matrix_2x2)   return Matrix_3x3;
   function     to_rotation_Transform (Angle       : in Radians )     return Matrix_3x3;
   function        to_scale_Transform (Scale       : in Vector_2)     return Matrix_3x3;

   function  to_Transform_2d (From        : in Matrix_3x3) return Transform_2d;
   function  to_Transform_2d (Rotation    : in Radians;
                              Translation : in Vector_2)   return Transform_2d;

   function  "*" (Left : in Vector_2;   Right : in Transform_2d) return Vector_2;
   function  "*" (Left : in Vector_2;   Right : in Matrix_3x3)   return Vector_2;

   function  Invert            (Transform : in Transform_2d)                         return Transform_2d;
   function  inverse_Transform (Transform : in Transform_2d;   Vector : in Vector_2) return Vector_2;

   function  get_Rotation    (Transform : in     Matrix_3x3)    return Matrix_2x2;
   procedure set_Rotation    (Transform : in out Matrix_3x3;   To : in Matrix_2x2);

   function  get_Translation (Transform : in     Matrix_3x3)    return Vector_2;
   procedure set_Translation (Transform : in out Matrix_3x3;   To : in Vector_2);

end any_Math.any_Algebra.any_linear.any_d2;
