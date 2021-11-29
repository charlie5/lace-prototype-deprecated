with Ahven,
     float_Math;

--  with Ada.Text_IO; use Ada.Text_IO;


package body math_Tests
is

   use Ahven;




   procedure counter_Test
   is
      use float_Math;
      use type Counter;
      Count : Counter := 0;
   begin
      increment (Count);      assert (Count = 1,  "increment () failed !");
      decrement (Count);      assert (Count = 0,  "decrement () failed !");

      increment (Count, 5);   assert (Count = 5,  "increment (by) failed !");
      decrement (Count, 5);   assert (Count = 0,  "decrement (by) failed !");
   end counter_Test;





   procedure integer_Test
   is
      use float_Math;
   begin
      declare
         Age : Integer := 0;
      begin
         increment (Age);      assert (Age = 1,  "increment () ... failed !");
         decrement (Age);      assert (Age = 0,  "decrement () ... failed !");

         increment (Age, 5);   assert (Age = 5,  "increment (by) ... failed !");
         decrement (Age, 5);   assert (Age = 0,  "decrement (by) ... failed !");
      end;

      declare
         A : Integer := 1;
         B : Integer := 2;
      begin
         swap (A, B);          assert (A = 2 and B = 1, "swap () ... failed !");
      end;
   end integer_Test;




   procedure real_Test
   is
      use float_Math;
   begin
      --- almost_Zero
      --
      begin
         assert (    almost_Zero (0.0                        ),  "almost_Zero (0.0) ... failed !");

         assert (    almost_Zero (0.0 + Real'Base'Model_Small),  "almost_Zero (0.0 + Real'Base'Model_Small) ... failed !");
         assert (not almost_Zero (0.0 + Real'Base'Model_Small
                                      + Real'Base'Model_Small),  "not almost_Zero (0.0 + Real'Base'Model_Small + Real'Base'Model_Small) ... failed !");

         assert (    almost_Zero (0.0 - Real'Base'Model_Small),  "almost_Zero (0.0 - Real'Base'Model_Small) ... failed !");
         assert (not almost_Zero (0.0 - Real'Base'Model_Small
                                      - Real'Base'Model_Small),  "not almost_Zero (0.0 - Real'Base'Model_Small - Real'Base'Model_Small) ... failed !");
      end;

      --- Clamped
      --
      begin
         assert (Clamped ( 0.0, -1.0, 1.0) =  0.0,  "Clamped (a) ... failed !");
         assert (Clamped ( 2.0, -1.0, 1.0) =  1.0,  "Clamped (b) ... failed !");
         assert (Clamped (-2.0, -1.0, 1.0) = -1.0,  "Clamped (c) ... failed !");
      end;


      --- clamp
      --
      declare
         the_Real : Real;
      begin
         the_Real :=  0.0;   clamp (the_Real, -1.0, 1.0);   assert (the_real =  0.0,  "clamp (a) ... failed !");
         the_Real :=  2.0;   clamp (the_Real, -1.0, 1.0);   assert (the_real =  1.0,  "clamp (b) ... failed !");
         the_Real := -2.0;   clamp (the_Real, -1.0, 1.0);   assert (the_real = -1.0,  "clamp (c) ... failed !");
      end;


      --- Image
      --
      declare
         the_Real : constant Real := 1.1111_1111;
      begin
         assert (Image (the_Real,  0) = " 1",           "Image (a) ... failed ! ... '" & Image (the_Real,  0) & "'");
         assert (Image (the_Real,  1) = " 1.1",         "Image (b) ... failed ! ... '" & Image (the_Real,  1) & "'");
         assert (Image (the_Real,  8) = " 1.11111116",  "Image (c) ... failed ! ... '" & Image (the_Real,  8) & "'");   -- tbd: why end in '6' ?
      end;

   end real_Test;



   procedure angle_Test
   is
      use float_Math;
   begin
      --- to_Radians
      --
      assert (to_Radians (  0.0) =   0.0,                "to_Radians (a) ... failed ! ... " & Image (to_Radians (  0.0), 12));
      assert (to_Radians ( 90.0) =  90.0 * Pi / 180.0,   "to_Radians (b) ... failed ! ... " & Image (to_Radians ( 90.0), 12));
      assert (to_Radians (180.0) = 180.0 * Pi / 180.0,   "to_Radians (c) ... failed ! ... " & Image (to_Radians (180.0), 12));
      assert (to_Radians (270.0) = 270.0 * Pi / 180.0,   "to_Radians (d) ... failed ! ... " & Image (to_Radians (270.0), 12));
      assert (to_Radians (360.0) = 360.0 * Pi / 180.0,   "to_Radians (e) ... failed ! ... " & Image (to_Radians (360.0), 12));


      --- to_Degrees
      --
      assert (to_Degrees ( 0.0)       =   0.0,   "to_Degrees (a) ... failed ! ... " & Degrees'Image (to_Degrees (  0.0)));
      assert (to_Degrees ( Pi / 2.00) =  90.0,   "to_Degrees (b) ... failed ! ... " & Degrees'Image (to_Degrees (  0.0)));
      assert (to_Degrees ( Pi)        = 180.0,   "to_Degrees (c) ... failed ! ... " & Degrees'Image (to_Degrees (  0.0)));
      assert (to_Degrees ( Pi * 2.0)  = 360.0,   "to_Degrees (d) ... failed ! ... " & Degrees'Image (to_Degrees (  0.0)));
   end angle_Test;




   procedure vector_Test
   is
      use float_Math;
   begin
      --- Sum & Average
      --
      assert (Sum     ((0.0, 1.0, 2.0, 3.0))  =  6.0,   "Sum     () ... failed ! ... " & Image (Sum     ((0.0, 1.0, 2.0, 3.0))));
      assert (Average ((0.0, 1.0, 2.0, 3.0))  =  1.5,   "Average () ... failed ! ... " & Image (Average ((0.0, 1.0, 2.0, 3.0))));

   end vector_Test;






   overriding
   procedure Initialize (T : in out Test) is
   begin
      T.set_Name ("Core Math Tests");

      Framework.add_test_Routine (T, counter_Test'Access, "counter_Test");
      Framework.add_test_Routine (T, integer_Test'Access, "integer_Test");
      Framework.add_test_Routine (T, real_Test   'Access, "real_Test");
      Framework.add_test_Routine (T, angle_Test  'Access, "angle_Test");
      Framework.add_test_Routine (T, vector_Test 'Access, "vector_Test");
   end Initialize;





end math_Tests;
