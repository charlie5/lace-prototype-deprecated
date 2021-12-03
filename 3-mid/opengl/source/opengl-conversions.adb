package body openGL.Conversions
is

   function to_Vector_4 (From : in rgba_Color) return Vector_4
   is
   begin
      return (Real (to_Primary (From.Primary.Red)),
              Real (to_Primary (From.Primary.Green)),
              Real (to_Primary (From.Primary.Blue)),
              Real (to_Primary (From.Alpha)));
   end to_Vector_4;



   function to_Vector_4 (From : in lucid_Color) return Vector_4
   is
   begin
      return (Real (From.Primary.Red),
              Real (From.Primary.Green),
              Real (From.Primary.Blue),
              Real (From.Opacity));
   end to_Vector_4;



   function to_Vector_3 (From : in rgb_Color) return Vector_3
   is
   begin
      return (Real (to_Primary (From.Red)),
              Real (to_Primary (From.Green)),
              Real (to_Primary (From.Blue)));
   end to_Vector_3;



   function to_Vector_3 (From : in Color) return Vector_3
   is
   begin
      return (Real (From.Red),
              Real (From.Green),
              Real (From.Blue));
   end to_Vector_3;



end openGL.Conversions;
