with impact.d2.orbs.Distance;



package body impact.d2.orbs.Colliders
is

   use type int32;



   --   Determine if two generic shapes overlap.
   --
   function b2TestOverlap (shapeA, shapeB : in impact.d2.orbs.Shape.view;
                           xfA,    xfB    : in b2Transform   ) return Boolean
   is
      use impact.d2.orbs.Distance;

      input  :         b2DistanceInput;
      cache  : aliased b2SimplexCache;
      output : aliased b2DistanceOutput;
   begin
      set (input.proxyA, shapeA);
      set (input.proxyB, shapeB);

      input.transformA := xfA;
      input.transformB := xfB;
      input.useRadii := True;

      cache.count := 0;

      b2Distance (output'Access, cache'Access, input);

      return output.distance < 10.0 * b2_epsilon;
   end b2TestOverlap;




   --   Compute the collision manifold between two circles.
   --
   procedure b2CollideCircles (manifold : access collision.b2Manifold;   circleA : access constant Shape.Item'Class;   xfA : in b2Transform;
                                                                         circleB : access constant Shape.Item'Class;   xfB : in b2Transform)
   is
      pA      : constant b2Vec2  := b2Mul (xfA, (0.0, 0.0));
      pB      : constant b2Vec2  := b2Mul (xfB, (0.0, 0.0));

      d       : constant b2Vec2  := pB - pA;
      distSqr : constant float32 := b2Dot (d, d);
      rA      : constant float32 := circleA.m_radius;
      rB      : constant float32 := circleB.m_radius;
      radius  : constant float32 := rA + rB;
   begin
      manifold.pointCount := 0;

      if distSqr > radius * radius then
         return;
      end if;

      manifold.Kind        := collision.e_circles;
      manifold.localPoint  := (0.0, 0.0);
      manifold.localNormal := (0.0, 0.0);
      manifold.pointCount  := 1;

      manifold.points (1).localPoint := (0.0, 0.0);
      manifold.points (1).id.key     := 0;
   end b2CollideCircles;


end impact.d2.orbs.Colliders;
