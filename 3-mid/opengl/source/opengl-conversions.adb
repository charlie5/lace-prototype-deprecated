package body openGL.Conversions
is

   function to_Vector_4 (From : in lucid_Color) return Vector_4
   is
   begin
      return (to_Real (From.Primary.Red),
              to_Real (From.Primary.Green),
              to_Real (From.Primary.Blue),
              to_Real (From.Opacity));
   end to_Vector_4;



   function to_Vector_4 (From : in light_Color) return Vector_4
   is
   begin
      return (From.Red,
              From.Green,
              From.Blue,
              From.Opacity);
   end to_Vector_4;


end openGL.Conversions;
