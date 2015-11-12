package body openGL.Model.Hexagon_Column
is

   overriding
   function  Bounds (Self : in Item) return openGL.Bounds
   is
   begin
      return Self.Bounds;
   end Bounds;



   function vertex_Sites (Self : in Item'Class) return Sites
   is
      pragma Unreferenced (Self);

      use linear_Algebra_3d;

      the_Site  :          Vector_3   := (1.0, 0.0, 0.0);
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


end openGL.Model.Hexagon_Column;
