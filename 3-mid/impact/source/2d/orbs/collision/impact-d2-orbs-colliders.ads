with impact.d2.orbs.Shape,
     impact.d2.orbs.Collision,
     impact.d2.Math;


package impact.d2.orbs.Colliders
--
--
--
is
   use impact.d2.Math;


   --   Compute the collision manifold between two circles.
   --
   procedure b2CollideCircles (manifold : access collision.b2Manifold;   circleA : access constant Shape.Item'Class;   xfA : in b2Transform;
                                                                         circleB : access constant Shape.Item'Class;   xfB : in b2Transform);




   --   Determine if two generic shapes overlap.
   --
   function b2TestOverlap (shapeA, shapeB : in impact.d2.orbs.Shape.view;
                           xfA,    xfB    : in b2Transform   ) return Boolean;




end impact.d2.orbs.Colliders;
