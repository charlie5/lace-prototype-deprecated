with
     float_Math.Algebra.linear.d3;

package Impact
--
--  Provides 2/3d physics simulations.
--
is
   pragma Pure;


   package Math              renames float_Math;
   package linear_Algebra_3d renames float_Math.Algebra.linear.d3;

   BT_LARGE_FLOAT : constant math.Real := 1.0e18;


   type Any is abstract tagged null record;     -- Equivalent of 'void' in C.

end Impact;
