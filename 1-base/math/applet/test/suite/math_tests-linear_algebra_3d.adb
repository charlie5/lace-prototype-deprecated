with
     Ahven,
     float_Math.Algebra.linear.d3;

--  with Ada.Text_IO; use Ada.Text_IO;


package body math_Tests.linear_Algebra_3d
is

   use Ahven,
       float_Math;


   function almost_Equal (Left, Right : in Real) return Boolean
   is
      Tolerance : constant := 0.00_000_1;
   begin
      return abs (Left - Right) <= Tolerance;
   end almost_Equal;



   function almost_Equal (Left, Right : in Vector_3) return Boolean
   is
   begin
      return almost_Equal (Left (1), Right (1))
        and  almost_Equal (Left (2), Right (2))
        and  almost_Equal (Left (3), Right (3));
   end almost_Equal;



   function almost_Equal (Left, Right : in Quaternion) return Boolean
   is
   begin
      return almost_Equal (Left.R,     Right.R)
        and  almost_Equal (Left.V (1), Right.V (1))
        and  almost_Equal (Left.V (2), Right.V (2))
        and  almost_Equal (Left.V (3), Right.V (3));
   end almost_Equal;



   procedure translation_Matrix_Test
   is
      use float_Math.Algebra.linear.d3;

      From : constant Vector_3 := (0.0, 0.0, 0.0);
      To   :          Vector_3;

   begin
      To := From * to_translation_Matrix ((1.0, 0.0, 0.0));

      assert (To (1) = 1.0,  Image (To) & "  translation (a) failed !");
      assert (To (2) = 0.0,  Image (To) & "  translation (b) failed !");
      assert (To (3) = 0.0,  Image (To) & "  translation (c) failed !");

      To := From * to_translation_Matrix ((0.0, 1.0, 0.0));

      assert (To (1) = 0.0,  Image (To) & "  translation (d) failed !");
      assert (To (2) = 1.0,  Image (To) & "  translation (e) failed !");
      assert (To (3) = 0.0,  Image (To) & "  translation (f) failed !");


      To := From * to_translation_Matrix ((-1.0, 0.0, 0.0));

      assert (To (1) = -1.0,  Image (To) & "  translation (g) failed !");
      assert (To (2) =  0.0,  Image (To) & "  translation (h) failed !");
      assert (To (3) =  0.0,  Image (To) & "  translation (i) failed !");

      To := From * to_translation_Matrix ((0.0, -1.0, 0.0));

      assert (To (1) =  0.0,  Image (To) & "  translation (j) failed !");
      assert (To (2) = -1.0,  Image (To) & "  translation (k) failed !");
      assert (To (3) =  0.0,  Image (To) & "  translation (l) failed !");


      To := From * to_translation_Matrix ((1.0, 1.0, 0.0));

      assert (To (1) =  1.0,  Image (To) & "  translation (m) failed !");
      assert (To (2) =  1.0,  Image (To) & "  translation (n) failed !");
      assert (To (3) =  0.0,  Image (To) & "  translation (o) failed !");

      To := From * to_translation_Matrix ((-1.0, -1.0, 0.0));

      assert (To (1) = -1.0,  Image (To) & "  translation (p) failed !");
      assert (To (2) = -1.0,  Image (To) & "  translation (q) failed !");
      assert (To (3) =  0.0,  Image (To) & "  translation (r) failed !");
   end translation_Matrix_Test;



   procedure rotation_Matrix_Test
   is
      use float_Math.Algebra.linear.d3;

      From : constant Vector_3 := (1.0, 0.0, 0.0);
      To   :          Vector_3;

   begin
      To := From * z_Rotation_from (to_Radians (90.0));

      assert (almost_Equal (To, (0.0, -1.0, 0.0)),
              Image (To, 16) & "  rotation (90) failed !");

      To := From * z_Rotation_from (to_Radians (-90.0));

      assert (almost_Equal (To, (0.0, 1.0, 0.0)),
              Image (To, 16) & "  rotation (-90) failed !");

      To := From * z_Rotation_from (to_Radians (180.0));

      assert (almost_Equal (To, (-1.0, 0.0, 0.0)),
              Image (To, 16) & "  rotation (180) failed !");

      To := From * z_Rotation_from (to_Radians (-180.0));

      assert (almost_Equal (To, (-1.0, 0.0, 0.0)),
              Image (To, 16) & "  rotation (-180) failed !");

      To := From * z_Rotation_from (to_Radians (270.0));

      assert (almost_Equal (To, (0.0, 1.0, 0.0)),
              Image (To, 16) & "  rotation (270) failed !");

      To := From * z_Rotation_from (to_Radians (-270.0));

      assert (almost_Equal (To, (0.0, -1.0, 0.0)),
              Image (To, 16) & "  rotation (-270) failed !");
   end rotation_Matrix_Test;



   procedure transform_Test
   is
      use float_Math.Algebra.linear.d3;

      From : constant Vector_3 := (1.0, 0.0, 0.0);
      To   :          Vector_3;

      Transform : Transform_3d := (rotation    => z_Rotation_from (to_Radians (90.0)),
                                   translation => (0.0, 0.0, 0.0));

   begin
      To := From * Transform;

      assert (almost_Equal (To, (0.0, 1.0, 0.0)),
              Image (To, 16) & "  transform () failed !");

      Transform.Translation := (1.0, 0.0, 0.0);
      To := From * Transform;

      assert (almost_Equal (To, (1.0, 1.0, 0.0)),
              Image (To, 16) & "  transform () failed !");
   end transform_Test;



   procedure quaternion_interpolation_Test
   is
      use float_Math.Algebra.linear.d3;

      Initial : constant Quaternion := to_Quaternion (z_Rotation_from (to_Radians ( 90.0)));
      Desired : constant Quaternion := to_Quaternion (z_Rotation_from (to_Radians (180.0)));

   begin
--        put_Line (Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, 0.0)))));
--        put_Line (Degrees'Image (to_Degrees (Angle (Initial))));

      assert (almost_Equal (Interpolated (Initial, Desired,   0.0), Initial),   "almost_Equal (Interpolated (Initial, Desired, 0.0), Initial) ... failed !");
      assert (almost_Equal (Interpolated (Initial, Desired, 100.0), Desired),   "almost_Equal (Interpolated (Initial, Desired, 1.0), Desired) ... failed !");

--        new_Line;
--        put_Line ("0.01   " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.01))))));
--        put_Line ("0.1    " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.1))))));
--        put_Line ("0.2    " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.2))))));
--        put_Line ("0.3    " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.3))))));
--        put_Line ("0.4    " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.4))))));
--        put_Line ("0.5    " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.5))))));
--        put_Line ("0.6    " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.6))))));
--        put_Line ("0.7    " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.7))))));
--        put_Line ("0.8    " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.8))))));
--        put_Line ("0.9    " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.9))))));
--        put_Line ("0.99   " & Degrees'Image (to_Degrees (Angle (Interpolated (Initial, Desired, to_Percentage (0.99))))));

--        put_Line (Degrees'Image (to_Degrees (to_Radians (90.0))));

      assert (almost_Equal (Angle (Interpolated (Initial, Desired, 50.0)),
                            to_Radians (135.0)),
              "Angle (Interpolated (Initial, Desired, 0.5)) = to_Radians (135.0) ... failed !");
   end quaternion_interpolation_Test;



   procedure inverse_transform_Test
   is
      use float_Math.Algebra.linear.d3;

      From      : constant Vector_3 := (1.0, 1.0, 1.0);
      To        :          Vector_3;

      Transform : constant Matrix_4x4 := to_transform_Matrix (Rotation    => z_Rotation_from (to_Radians (90.0)),
                                                              Translation => (5.0, 5.0, 5.0));
   begin
      To := From * Transform;
      To := To   * inverse_Transform (Transform);

      assert (almost_Equal (To, From),
              Image (To, 16) & "  inverse_Transform failed !");
   end inverse_transform_Test;



   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.set_Name ("Linear Algebra (3D) Tests");

      Framework.add_test_Routine (T,       translation_Matrix_Test'Access,       "translation_Matrix_Test");
      Framework.add_test_Routine (T,          rotation_Matrix_Test'Access,          "rotation_Matrix_Test");
      Framework.add_test_Routine (T,                transform_Test'Access,                "transform_Test");
      Framework.add_test_Routine (T,        inverse_transform_Test'Access,        "inverse_transform_Test");
      Framework.add_test_Routine (T, quaternion_interpolation_Test'Access, "quaternion_interpolation_Test");
   end Initialize;


end math_Tests.linear_Algebra_3d;
