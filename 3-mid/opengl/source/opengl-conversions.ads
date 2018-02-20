package openGL.Conversions
is

   function to_Vector_4 (From : in lucid_Color) return Vector_4;
   function to_Vector_4 (From : in light_Color) return Vector_4;

   function to_light_Color (From : in lucid_Color) return light_Color;
   function "+"            (From : in lucid_Color) return light_Color renames to_light_Color;

end openGL.Conversions;
