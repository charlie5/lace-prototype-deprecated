with
     cached_Rotation;


package body any_Math.any_fast_Rotation
is

   function to_Matrix_2x2 (m11, m12,
                           m21, m22 : Real) return Matrix_2x2
   is
   begin
      return (1 => (m11, m12),
              2 => (m21, m22));
   end to_Matrix_2x2;


   package the_Cache is new cached_Rotation (Float_type                 => any_Math.Real,
                                             Matrix_2x2_type            => any_Math.Matrix_2x2,
                                             float_elementary_Functions => any_math.Functions,
                                             to_Matrix_2x2              => to_Matrix_2x2,
                                             slot_Count                 => 10_000);

   function to_Rotation (Angle : in Real) return access constant Matrix_2x2
   is
   begin
      return the_Cache.to_Rotation (Angle);
   end to_Rotation;


end any_Math.any_fast_Rotation;
