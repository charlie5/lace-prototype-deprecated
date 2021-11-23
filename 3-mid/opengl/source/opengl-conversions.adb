package body openGL.Conversions
is

   function to_Vector_4 (From : in lucid_Color) return Vector_4
   is
   begin
      return (Real (to_Primary (From.Primary.Red)),
              Real (to_Primary (From.Primary.Green)),
              Real (to_Primary (From.Primary.Blue)),
              Real (to_Primary (From.Opacity)));
   end to_Vector_4;



   function to_Vector_4 (From : in light_Color) return Vector_4
   is
   begin
      return (Real (From.Red),
              Real (From.Green),
              Real (From.Blue),
              Real (From.Opacity));
   end to_Vector_4;



   function to_light_Color (From : in lucid_Color) return light_Color
   is
   begin
      return (to_Primary (From.Primary.Red),
              to_Primary (From.Primary.Green),
              to_Primary (From.Primary.Blue),
              Opaqueness (to_Primary (From.Opacity)));
   end to_light_Color;


end openGL.Conversions;
