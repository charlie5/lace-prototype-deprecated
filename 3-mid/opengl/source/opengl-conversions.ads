package openGL.Conversions
is

   function to_Vector_4 (From : in rgba_Color)  return Vector_4;
   function to_Vector_4 (From : in lucid_Color) return Vector_4;

   function to_lucid_Color (From : in rgba_Color) return lucid_Color;
   function "+"            (From : in rgba_Color) return lucid_Color renames to_lucid_Color;

end openGL.Conversions;
