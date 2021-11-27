with
     Ahven,
     float_Math.Algebra.linear.d2;



package body math_Tests.linear_Algebra_2d
is

   use Ahven,
       float_Math;


   function almost_Equal (Left, Right : in Real) return Boolean
   is
      Tolerance : constant := 0.000_000_1;
   begin
      return abs (Left - Right) <= Tolerance;
   end almost_Equal;




   procedure translation_Matrix_Test
   is
      use
          float_Math.Algebra.linear.d2;

      From : constant Vector_2 := (0.0, 0.0);
      To   : Vector_2;

   begin
      To := From * to_translation_Transform ((1.0, 0.0));

      assert (To (1) = 1.0,  Image (To) & "  translation () failed !");
      assert (To (2) = 0.0,  Image (To) & "  translation () failed !");

      To := From * to_translation_Transform ((0.0, 1.0));

      assert (To (1) = 0.0,  Image (To) & "  translation () failed !");
      assert (To (2) = 1.0,  Image (To) & "  translation () failed !");


      To := From * to_translation_Transform ((-1.0, 0.0));

      assert (To (1) = -1.0,  Image (To) & "  translation () failed !");
      assert (To (2) =  0.0,  Image (To) & "  translation () failed !");

      To := From * to_translation_Transform ((0.0, -1.0));

      assert (To (1) =  0.0,  Image (To) & "  translation () failed !");
      assert (To (2) = -1.0,  Image (To) & "  translation () failed !");


      To := From * to_translation_Transform ((1.0, 1.0));

      assert (To (1) =  1.0,  Image (To) & "  translation () failed !");
      assert (To (2) =  1.0,  Image (To) & "  translation () failed !");

      To := From * to_translation_Transform ((-1.0, -1.0));

      assert (To (1) = -1.0,  Image (To) & "  translation () failed !");
      assert (To (2) = -1.0,  Image (To) & "  translation () failed !");
   end translation_Matrix_Test;



   procedure rotation_Matrix_Test
   is
      use
          float_Math.Algebra.linear.d2;

      From : constant Vector_2 := (1.0, 0.0);
      To   : Vector_2;

   begin
      To := From * to_rotation_Matrix (to_Radians (90.0));

      assert (almost_Equal (To (1),  0.0),   Image (To, 16) & "  rotation (90a) failed !");
      assert (almost_Equal (To (2),  1.0),   Image (To, 16) & "  rotation (90b) failed !");

      To := From * to_rotation_Matrix (to_Radians (-90.0));

      assert (almost_Equal (To (1),  0.0),   Image (To, 16) & "  rotation (-90a) failed !");
      assert (almost_Equal (To (2), -1.0),   Image (To, 16) & "  rotation (-90b) failed !");


      To := From * to_rotation_Matrix (to_Radians (180.0));

      assert (almost_Equal (To (1), -1.0),   Image (To, 16) & "  rotation (180a) failed !");
      assert (almost_Equal (To (2),  0.0),   Image (To, 16) & "  rotation (180b) failed !");

      To := From * to_rotation_Matrix (to_Radians (-180.0));

      assert (almost_Equal (To (1), -1.0),   Image (To, 16) & "  rotation (-180a) failed !");
      assert (almost_Equal (To (2),  0.0),   Image (To, 16) & "  rotation (-180b) failed !");


      To := From * to_rotation_Matrix (to_Radians (270.0));

      assert (almost_Equal (To (1),  0.0),   Image (To, 16) & "  rotation (270a) failed !");
      assert (almost_Equal (To (2), -1.0),   Image (To, 16) & "  rotation (270b) failed !");

      To := From * to_rotation_Matrix (to_Radians (-270.0));

      assert (almost_Equal (To (1),  0.0),   Image (To, 16) & "  rotation (-270) failed !");
      assert (almost_Equal (To (2),  1.0),   Image (To, 16) & "  rotation (-270) failed !");
   end rotation_Matrix_Test;



   procedure transform_Test
   is
      use
          float_Math.Algebra.linear.d2;

      From : constant Vector_2 := (1.0, 0.0);
      To   : Vector_2;

      Transform : Transform_2d := to_Transform_2d (rotation    => to_Radians (90.0),
                                                   translation => (0.0, 0.0));

   begin
      To := From * Transform;

      assert (almost_Equal (To (1),  0.0),   Image (To, 16) & "  transform (a) failed !");
      assert (almost_Equal (To (2),  1.0),   Image (To, 16) & "  transform (b) failed !");


      Transform.Translation := (1.0, 0.0);
      To := From * Transform;

      assert (almost_Equal (To (1),  1.0),   Image (To, 16) & "  transform (c) failed !");
      assert (almost_Equal (To (2),  1.0),   Image (To, 16) & "  transform (d) failed !");
   end transform_Test;



   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.set_Name ("Linear Algebra (2D) Tests");

      Framework.add_test_Routine (T, translation_Matrix_Test'Access, "translation_Matrix_Test");
      Framework.add_test_Routine (T,    rotation_Matrix_Test'Access,    "rotation_Matrix_Test");
      Framework.add_test_Routine (T,          transform_Test'Access,          "transform_Test");
   end Initialize;


end math_Tests.linear_Algebra_2d;
