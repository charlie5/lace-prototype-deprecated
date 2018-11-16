with impact.d2.Shape.circle,
     impact.d2.Shape.polygon,
     impact.d2.Collision,
     impact.d2.Math;


package impact.d2.Colliders
--
--
--
is
   use impact.d2.Shape.circle,
       impact.d2.Shape.polygon,
       impact.d2.Math;


   --   Compute the collision manifold between two circles.
   --
   procedure b2CollideCircles (manifold : access collision.b2Manifold;   circleA : access constant b2CircleShape'Class;   xfA : in b2Transform;
                                                                         circleB : access constant b2CircleShape'Class;   xfB : in b2Transform);


   --   Compute the collision manifold between a polygon and a circle.
   --
   procedure b2CollidePolygonAndCircle (manifold : access collision.b2Manifold;   polygon : access constant b2PolygonShape'Class;   xfA : in b2Transform;
                                                                                  circle  : access constant b2CircleShape'Class;    xfB : in b2Transform);



   --   Compute the collision manifold between two polygons.
   --
   procedure b2CollidePolygons (manifold : access collision.b2Manifold;   polygonA : access constant b2PolygonShape'Class;   xfA : in b2Transform;
                                                                          polygonB : access constant b2PolygonShape'Class;   xfB : in b2Transform);


--  void b2CollidePolygons(b2Manifold* manifold,
--                                             const b2PolygonShape* polygon1, const b2Transform& xf1,
--                                             const b2PolygonShape* polygon2, const b2Transform& xf2);






   --   Determine if two generic shapes overlap.
   --
   function b2TestOverlap (shapeA, shapeB : in impact.d2.Shape.view;
                           xfA,    xfB    : in b2Transform   ) return Boolean;




end impact.d2.Colliders;
