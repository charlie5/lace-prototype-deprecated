with
     float_Math;


package Physics
--
--  Provides a physics interface for 2/3d simulations.
--
is
   pragma Pure;


   package Math
     renames float_Math;

   type Vector_2_array is array (Positive range <>) of math.Vector_2;
   type Vector_3_array is array (Positive range <>) of math.Vector_3;

   type Heightfield    is array (Positive range <>,
                                 Positive range <>) of aliased math.Real;


   unsupported_Error : exception;
   --
   -- Raised when a shape, joint is not supported in a space.

end Physics;
