with
     float_Math.Geometry.d3;


package bullet_Physics
--
-- Provides an implementation of the physics interface using a binding to the Bullet3D C library.
--
is
   pragma Pure;

   package Math        renames float_Math;
   package Geometry_3D renames Math.Geometry.d3;

end bullet_Physics;
