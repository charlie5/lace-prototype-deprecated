with ahven.Text_Runner,
     ahven.Framework,
     math_Tests.linear_Algebra_2d,
     math_Tests.linear_Algebra_3d,
     math_Tests.Geometry_2d;


procedure launch_math_Testsuite
is
   S : constant ahven.Framework.test_Suite_access := ahven.Framework.create_Suite ("All Math Tests");

begin
   S.add_Test (new math_Tests                  .Test);
   S.add_Test (new math_Tests.linear_Algebra_2d.Test);
   S.add_Test (new math_Tests.linear_Algebra_3d.Test);
   S.add_Test (new math_Tests.Geometry_2d      .Test);

   ahven.text_Runner.run           (S);
   ahven.Framework  .release_Suite (S);
end launch_math_Testsuite;
