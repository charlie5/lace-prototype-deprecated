with
     short_Math,
     float_Math.Geometry,
     long_Math,

     ada.text_IO;


procedure launch_basic_math_Demo
--
-- A simple demonstration of the Math packages.
--
is
   package   Math renames float_Math;
   use       Math, math.Geometry;

   procedure log (Message : in String) renames ada.text_IO.put_Line;

begin
   declare
      Value : Real := 0.0;
   begin
      log (Image (Value, 5));
      Value := Value + 1.0;
      log (Image (Value, 5));
   end;

   declare
      use Vectors;
      Value : Vector_3 := math.Origin_3d;
   begin
      log (Image (Value));
      Value := Value + (1.0, 1.0, 1.0);
      log (Image (Value));
   end;
end launch_basic_math_Demo;
