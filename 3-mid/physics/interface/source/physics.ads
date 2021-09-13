with
     float_Math.Geometry.D2,
     float_Math.Geometry.D3,
     float_Math.Algebra.linear.D3;

package Physics
--
--  Provides a physics interface for 2D/3D simulations.
--
is
   pragma Pure;

   package Math              renames float_Math;
   package Geometry_2D       renames math.Geometry.d2;
   package Geometry_3D       renames math.Geometry.d3;
   package linear_Algebra_3D renames math.Algebra.linear.d3;

   use Math;

   type Vector_2_array is array (Positive range <>) of Vector_2;
   type Vector_3_array is array (Positive range <>) of Vector_3;

   type Heightfield    is array (Positive range <>,
                                 Positive range <>) of aliased Real;


   type space_Kind is (Bullet, Box2D);

   max_Models : constant := 2**32 - 1;
   type model_Id is range 0 .. max_Models;

   null_model_Id : constant physics.model_Id;


   unsupported_Error : exception;
   --
   -- Raised when a shape or joint is not supported in a space.


private

   null_model_Id : constant physics.model_Id := 0;

end Physics;
