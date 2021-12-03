package openGL.Conversions
is

   function to_Vector_4 (From : in rgba_Color)  return Vector_4;
   function to_Vector_4 (From : in lucid_Color) return Vector_4;

   function to_Vector_3 (From : in rgb_Color)   return Vector_3;
   function to_Vector_3 (From : in Color)       return Vector_3;

end openGL.Conversions;
