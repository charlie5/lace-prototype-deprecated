package body openGL.Conversions
is

   function to_Vector_4 (From : in rgba_Color) return Vector_4
   is
   begin
      return (Real (to_Primary (From.Primary.Red)),
              Real (to_Primary (From.Primary.Green)),
              Real (to_Primary (From.Primary.Blue)),
              Real (to_Primary (From.Opacity)));
   end to_Vector_4;



   function to_Vector_4 (From : in lucid_Color) return Vector_4
   is
   begin
      return (Real (From.Primary.Red),
              Real (From.Primary.Green),
              Real (From.Primary.Blue),
              Real (From.Opacity));
   end to_Vector_4;



   function to_lucid_Color (From : in rgba_Color) return lucid_Color
   is
   begin
      return (Primary => (to_Primary (From.Primary.Red),
                          to_Primary (From.Primary.Green),
                          to_Primary (From.Primary.Blue)),
              Opacity => Opaqueness (to_Primary (From.Opacity)));
   end to_lucid_Color;


end openGL.Conversions;
