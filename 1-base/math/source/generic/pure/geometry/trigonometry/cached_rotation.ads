with
     ada.Numerics.generic_elementary_Functions;


generic
   type Float_type      is digits <>;
   type Matrix_2x2_type is private;

   with package  float_elementary_Functions is new ada.Numerics.generic_elementary_Functions (Float_type);
   with function to_Matrix_2x2 (m11, m12,
                                m21, m22 : Float_type) return Matrix_2x2_type;

   slot_Count : Standard.Positive;

package cached_Rotation
--
-- Caches 2x2 rotation matrices of angles for speed at the cost of precision.
--
is
   pragma Optimize (Time);

   function to_Rotation (Angle : in Float_type) return access constant Matrix_2x2_type;



private

   pragma Inline_Always (to_Rotation);

end cached_Rotation;
