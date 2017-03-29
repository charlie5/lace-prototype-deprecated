with
     float_Math.Geometry.d3;


package Physics
--
--  Provides a physics interface for 2/3d simulations.
--
is
   pragma Pure;


   package Math        renames float_Math;
   package Geometry_3D renames Math.Geometry.d3;

   type Vector_2_array is array (Positive range <>) of math.Vector_2;
   type Vector_3_array is array (Positive range <>) of math.Vector_3;

   type Heightfield    is array (Positive range <>,
                                 Positive range <>) of aliased math.Real;



   type space_Kind is (Bullet,    Box2d);



   unsupported_Error : exception;
   --
   -- Raised when a shape, joint is not supported in a space.

end Physics;
