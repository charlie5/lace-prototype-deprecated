with
     float_Math;

package box2d_Physics
--
-- Provides an implementation of the physics interface using a binding to the Box2D C library.
--
is
   pragma Pure;

   package Math renames float_Math;

   Error : exception;

end box2d_Physics;
