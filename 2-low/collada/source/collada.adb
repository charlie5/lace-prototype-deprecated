package body Collada
is

   function get_Matrix (From : in Float_array;   Which : in Positive) return Matrix_4x4
   is
      First      : constant Positive       := (Which - 1) * 16 + 1;
      the_Vector : constant math.Vector_16 := math.Vector_16 (From (First .. First + 15));
   begin
      return math.to_Matrix_4x4 (the_Vector);
   end get_Matrix;



   function matrix_Count (From : in Float_array) return Natural
   is
   begin
      return From'Length / 16;
   end matrix_Count;

end Collada;
