generic
package any_Math.any_fast_Rotation
is

   function to_Rotation (Angle : in Real) return access constant Matrix_2x2;

private

   pragma Inline_Always (to_Rotation);

end any_Math.any_fast_Rotation;
