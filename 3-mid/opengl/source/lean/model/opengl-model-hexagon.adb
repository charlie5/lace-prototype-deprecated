package body openGL.Model.hexagon
is


   function vertex_Sites (Radius : in Real) return Sites
   is
      use openGL, linear_Algebra_3d;

      the_Site  :          Vector_3   := (Radius, 0.0, 0.0);
      Rotation  : constant Matrix_3x3 := y_Rotation_from (to_Radians (60.0));

      the_Sites :          Sites;

   begin
      for Each in the_Sites'Range
      loop
         the_Sites (Each) := the_Site;
         the_Site         := Rotation * the_Site;
      end loop;

      return the_Sites;
   end vertex_Sites;


end openGL.Model.hexagon;
