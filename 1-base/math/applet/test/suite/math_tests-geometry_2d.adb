with
     Ahven,
     float_Math.Geometry.d2;


package body math_Tests.Geometry_2d
is

   use Ahven,
       float_Math;


   function almost_Equal (Left, Right : in Real) return Boolean
   is
      Tolerance : constant := 0.000_001;
   begin
      return abs (Left - Right) <= Tolerance;
   end almost_Equal;




   procedure Polygon_is_convex_Test
   is
      use float_Math.Geometry.d2;

      the_Poly : Polygon := (vertex_Count => 4,
                             vertices     => ((-1.0, -1.0),
                                              ( 1.0, -1.0),
                                              ( 1.0,  1.0),
                                              (-1.0,  1.0)));
   begin
      assert (is_Convex (the_Poly),
              "T1 => " & Image (the_Poly) & " should be convex ... failed !");

      the_Poly.Vertices (3) := (0.0, 0.0);
      assert (is_Convex (the_Poly),
              "T2 => " & Image (the_Poly) & " should be convex ... failed !");

      the_Poly.Vertices (3) := (0.0, 0.1);
      assert (is_Convex (the_Poly),
              "T3 => " & Image (the_Poly) & " should be convex ... failed !");

      the_Poly.Vertices (3) := (0.0, -0.1);
      assert (not is_Convex (the_Poly),
              "T4 => " & Image (the_Poly) & " should not be convex ... failed !");
   end Polygon_is_convex_Test;



   procedure triangle_Area_Test
   is
      use float_Math.Geometry.d2;

      the_Tri : Triangle := (vertices => (( 0.0,  0.0),
                                          ( 1.0,  0.0),
                                          ( 1.0,  1.0)));
   begin
      assert (almost_Equal (Area (the_Tri), 0.5),
              "T1 =>  & Image (the_Tri) &  area should be 0.5 ... failed !   " & Image (Area (the_Tri), 12));


      the_Tri := (vertices => ((-0.11073643,  -0.179634809),
                               (-0.0553682148, 0.410182595),
                               (-0.0276841074, 0.705091298)));
      assert (Area (the_Tri) >= 0.0,
              "T2 =>  & Image (the_Tri) &  area should be positive ... failed !");


      the_Tri := (vertices => ((-1.0, -1.0),
                               ( 1.0, -1.0),
                               ( 1.0, -0.999999)));
      assert (Area (the_Tri) > 0.0,
              "T3 =>  & Image (the_Tri) &  area should be positive ... failed !");

      the_Tri := (vertices => ((-0.11073643,  -0.179634809),
                               (-0.0276841074, 0.705091298),
                               (-0.0553682148, 0.410182595)));
      assert (Area (the_Tri) >= 0.0,
              "T4 =>  & Image (the_Tri) &  area should be positive ... failed !");

      -- tbd: Add tests for degenerate triangles.
   end triangle_Area_Test;



   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.set_Name ("Geometry (2D) Tests");

      Framework.add_test_Routine (T, Polygon_is_convex_Test'Access, "Polygon is convex Test");
      Framework.add_test_Routine (T, triangle_Area_Test    'Access, "Triangle area Test");
   end Initialize;


end math_Tests.Geometry_2d;
