with
     float_Math.Geometry.d2,
     float_Math.Geometry.d3,
     float_Math.Algebra.linear.d3;

package Physics
--
--  Provides a physics interface for 2/3d simulations.
--
is
   pragma Pure;


   package Math        renames float_Math;
   package Geometry_2D renames math.Geometry.d2;
   package Geometry_3D renames math.Geometry.d3;
   package linear_Algebra_3d renames math.Algebra.linear.d3;

   type Vector_2_array is array (Positive range <>) of math.Vector_2;
   type Vector_3_array is array (Positive range <>) of math.Vector_3;

   type Heightfield    is array (Positive range <>,
                                 Positive range <>) of aliased math.Real;



   type space_Kind is (Bullet,    Box2d);


   max_physics_Models  : constant := 2**32 - 1;
   type model_Id is range 0 .. max_physics_Models;

   null_physics_model_Id  : constant physics.model_Id;


   unsupported_Error : exception;
   --
   -- Raised when a shape, joint is not supported in a space.


private

   null_physics_model_Id  : constant physics.model_Id  := 0;

end Physics;
