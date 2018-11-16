with impact.d2.Distance;



package body impact.d2.Collision
is

   procedure dummy is begin null; end dummy;





   function  IsValid (Self : in b2AABB) return Boolean
   is
      d     : b2Vec2  := Self.upperBound - Self.lowerBound;
      valid : Boolean := d.x >= 0.0 and then d.y >= 0.0;
   begin
      valid := valid and then IsValid (Self.lowerBound) and then IsValid (Self.upperBound);
      return valid;
   end IsValid;



   function  getCenter (Self : in b2AABB) return b2Vec2
   is
   begin
      return 0.5 * (Self.lowerBound + Self.upperBound);
   end getCenter;



   function  getExtents (Self : in b2AABB) return b2Vec2
   is
   begin
      return 0.5 * (Self.upperBound + Self.lowerBound);
   end getExtents;



   function  getPerimeter (Self : in     b2AABB) return float32
   is
      wx : constant float32 := Self.upperBound.x - Self.lowerBound.x;
      wy : constant float32 := Self.upperBound.y - Self.lowerBound.y;
   begin
      return 2.0 * (wx + wy);
   end getPerimeter;




   procedure combine    (Self : in out b2AABB;   aabb1, aabb2 : in b2AABB)
   is
   begin
      Self.lowerBound := b2Min (aabb1.lowerBound, aabb2.lowerBound);
      Self.upperBound := b2Max (aabb1.upperBound, aabb2.upperBound);
   end combine;



   function  Contains   (Self : in b2AABB;   aabb : in b2AABB) return Boolean
   is
      result : Boolean := True;
   begin
      result := result and then Self.lowerBound.x <= aabb.lowerBound.x;
      result := result and then Self.lowerBound.y <= aabb.lowerBound.y;
      result := result and then aabb.upperBound.x <= Self.upperBound.x;
      result := result and then aabb.upperBound.y <= Self.upperBound.y;

      return result;
   end Contains;





   --  From Real-time Collision Detection, p179.
   --
   function  rayCast    (Self : in b2AABB;   output : access b2RayCastOutput;
                                             input  : in     b2RayCastInput) return Boolean
   is
      tmin   : float32 := -b2_maxFloat;
      tmax   : float32 :=  b2_maxFloat;

      p      : constant b2Vec2  := input.p1;
      d      : constant b2Vec2  := input.p2 - input.p1;
      absD   : constant b2Vec2  := b2Abs (d);

      normal : b2Vec2;
   begin
      for i in int32'(1) .. 2 loop

         if Element (absD, i) < b2_epsilon then
            --  Parallel.
            if Element (p, i) < Element (Self.lowerBound, i) or else Element (Self.upperBound, i) < Element (p, i) then
               return False;
            end if;
         else
            declare
               inv_d : constant float32 := 1.0 / Element (d, i);
               t1 : float32 := (Element (Self.lowerBound, i) - Element (p, i)) * inv_d;
               t2 : float32 := (Element (Self.upperBound, i) - Element (p, i)) * inv_d;

               --  Sign of the normal vector.
               s : float32 := -1.0;
            begin

               if t1 > t2 then
                  b2Swap (t1, t2);
                  s := 1.0;
               end if;

               --  Push the min up
               if t1 > tmin then
                  SetZero (normal);
                  set_Element (normal, i, s);
                  tmin := t1;
               end if;

               --  Pull the max down
               tmax := float32'Min (tmax, t2);

               if tmin > tmax then
                  return False;
               end if;
            end;
         end if;

      end loop;

              -- Does the ray start inside the box?
              -- Does the ray intersect beyond the max fraction?

      if tmin < 0.0 or else input.maxFraction < tmin then
         return False;
      end if;

      --  Intersection.
      output.fraction := tmin;
      output.normal   := normal;

      return True;
   end rayCast;










--   Compute the collision manifold between two circles.
--  void b2CollideCircles(b2Manifold* manifold,
--                                            const b2CircleShape* circle1, const b2Transform& xf1,
--                                            const b2CircleShape* circle2, const b2Transform& xf2);


--   Compute the collision manifold between a polygon and a circle.
--  void b2CollidePolygonAndCircle(b2Manifold* manifold,
--                                                             const b2PolygonShape* polygon, const b2Transform& xf1,
--                                                             const b2CircleShape* circle, const b2Transform& xf2);


--   Compute the collision manifold between two polygons.
--  void b2CollidePolygons(b2Manifold* manifold,
--                                             const b2PolygonShape* polygon1, const b2Transform& xf1,
--                                             const b2PolygonShape* polygon2, const b2Transform& xf2);


--   Clipping for contact manifolds.
--  int32 b2ClipSegmentToLine(b2ClipVertex vOut[2], const b2ClipVertex vIn[2],
--                                                          const b2Vec2& normal, float32 offset);


--   Determine if two generic shapes overlap.
--  bool b2TestOverlap(const b2Shape* shapeA, const b2Shape* shapeB,
--                                     const b2Transform& xfA, const b2Transform& xfB);






   function b2TestOverlap (a, b : in b2AABB) return Boolean
   is
      d1 : b2Vec2 := b.lowerBound - a.upperBound;
      d2 : b2Vec2 := a.lowerBound - b.upperBound;
   begin
      if d1.x > 0.0 or else d1.y > 0.0 then
         return False;
      end if;

      if d2.x > 0.0 or else d2.y > 0.0 then
         return False;
      end if;

      return True;

   end b2TestOverlap;





   function b2TestOverlap (shapeA, shapeB : access Shape.b2Shape'Class;
                           xfA,    xfB    : in     b2Transform      ) return Boolean
   is
      use impact.d2.Distance;

      input  :         b2DistanceInput;
      cache  : aliased b2SimplexCache;
      output : aliased b2DistanceOutput;
   begin
--        Set (input.proxyA,  shapeA.all'Access);
--        Set (input.proxyB,  shapeB.all'Access);

      input.transformA := xfA;
      input.transformB := xfB;
      input.useRadii   := True;

      cache.count := 0;

      b2Distance (output'Access, cache'Access, input);

      return output.distance < 10.0 * b2_epsilon;
   end b2TestOverlap;





   procedure Initialize (Self : in out b2WorldManifold;   manifold : in b2Manifold;
                                                          xfA      : in b2Transform;   radiusA : in float32;
                                                          xfB      : in b2Transform;   radiusB : in float32)
   is
      use type int32;

      PointA, PointB,
      cA,     cB,
      planePoint,
      clipPoint     : b2Vec2;
   begin
      if manifold.pointCount = 0 then
         return;
      end if;


      case manifold.kind is
         when e_circles =>
            Self.normal := (1.0, 0.0);

            pointA := b2Mul (xfA, manifold.localPoint);
            pointB := b2Mul (xfB, manifold.points (1).localPoint);

            if b2DistanceSquared (pointA, pointB) > b2_epsilon * b2_epsilon then
               Self.normal := pointB - pointA;
               normalize (Self.normal);
            end if;

            cA := pointA + radiusA * Self.normal;
            cB := pointB - radiusB * Self.normal;

            Self.points (1) := 0.5 * (cA + cB);

         when e_faceA =>
            Self.normal := b2Mul (xfA.R, manifold.localNormal);
            planePoint  := b2Mul (xfA,   manifold.localPoint);

            for i in 1 .. manifold.pointCount loop
               clipPoint := b2Mul (xfB, manifold.points (i).localPoint);

               cA := clipPoint +  (radiusA - b2Dot (clipPoint - planePoint, Self.normal)) * Self.normal;
               cB := clipPoint -  radiusB * Self.normal;

               Self.points (i) := 0.5 * (cA + cB);
            end loop;


         when e_faceB =>
            Self.normal := b2Mul (xfB.R, manifold.localNormal);
            planePoint  := b2Mul (xfB,   manifold.localPoint);

            for i in 1 .. manifold.pointCount loop
               clipPoint := b2Mul (xfA, manifold.points (i).localPoint);

               cB := clipPoint + (radiusB - b2Dot (clipPoint - planePoint, Self.normal)) * Self.normal;
               cA := clipPoint - radiusA * Self.normal;

               Self.points (i) := 0.5 * (cA + cB);
            end loop;

            Self.normal := -Self.normal;   -- Ensure normal points from A to B.
      end case;
   end Initialize;





--  #include <Box2D/Collision/b2Collision.h>
--  #include <Box2D/Collision/b2Distance.h>
--

   procedure b2GetPointStates (state1,    state2    : access          b2PointStates;
                               manifold1, manifold2 : access constant b2Manifold)
   is
      use type uint32;
      Id : b2ContactId;
   begin
      for i in b2PointStates'Range loop
         state1 (i) := b2_nullState;
         state2 (i) := b2_nullState;
      end loop;

      --  Detect persists and removes.
      --
      for i in 1 .. manifold1.pointCount loop
         id         := manifold1.points (i).id;
         state1 (i) := b2_removeState;

         for j in 1 .. manifold2.pointCount loop
            if manifold2.points (j).id.key = id.key then
               state1 (i) := b2_persistState;
               exit;
            end if;
         end loop;

      end loop;


      --  Detect persists and adds.
      --
      for i in 1 .. manifold2.pointCount loop
         id         := manifold2.points (i).id;
         state2 (i) := b2_addState;

         for j in 1 .. manifold1.pointCount loop
            if manifold1.points (j).id.key = id.key then
               state2 (i) := b2_persistState;
               exit;
            end if;
         end loop;

      end loop;

   end b2GetPointStates;





   --  Sutherland-Hodgman clipping.
   --
   function b2ClipSegmentToLine (vOut   : access b2ClipVertices;
                                 vIn    : in     b2ClipVertices;
                                 normal : in     b2Vec2      ;
                                 offset : in     float32     ) return int32
   is
      use type int32;
      numOut    : int32   := 1;    -- Start with no output points

      --  Calculate the distance of end points to the line
      distance0 : constant float32 := b2Dot (normal,  vIn (1).v) - offset;
      distance1 : constant float32 := b2Dot (normal,  vIn (2).v) - offset;

      interp    : float32;
   begin
      --  If the points are behind the plane
      if distance0 <= 0.0 then
         vOut (numOut) := vIn (1);
         numOut        := numOut + 1;
      end if;

      if distance1 <= 0.0 then
         vOut (numOut) := vIn (2);
         numOut        := numOut + 1;
      end if;


      if distance0 * distance1 < 0.0 then                               -- If the points are on different sides of the plane
         --  Find intersection point of edge and plane
         --
         interp          := distance0 / (distance0 - distance1);
         vOut (numOut).v := vIn (1).v  +  interp * (vIn (2).v - vIn (1).v);

         if distance0 > 0.0 then
            vOut (numOut).id := vIn (1).id;
         else
            vOut (numOut).id := vIn (2).id;
         end if;

         numOut := numOut + 1;
      end if;

      return numOut - 1;
   end b2ClipSegmentToLine;


end impact.d2.Collision;
