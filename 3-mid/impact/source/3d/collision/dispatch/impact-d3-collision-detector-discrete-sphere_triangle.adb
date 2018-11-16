with impact.d3.Transform;
with impact.d3.Vector;
with impact.d3.Scalar;



package body impact.d3.collision.Detector.discrete.sphere_triangle
is


   function to_sphere_triangle_Detector (sphere                   : access impact.d3.Shape.convex.internal.sphere  .Item'Class;
                                         triangle                 : access impact.d3.Shape.convex.internal.polyhedral.triangle.Item'Class;
                                         contactBreakingThreshold : in     math.Real               ) return Item
   is
      Self : Item;
   begin
      Self.m_sphere                   := sphere;
      Self.m_triangle                 := triangle;
      Self.m_contactBreakingThreshold := contactBreakingThreshold;


      return Self;
   end to_sphere_triangle_Detector;






   overriding procedure getClosestPoints (Self : in out Item;   input       : in     ClosestPointInput;
                                                     output      : in out Result'Class;
                                                     swapResults : in     Boolean          := False)
   is
      use impact.d3.Transform, math.Vectors;
      use linear_Algebra_3d;

      transformA : Transform_3d renames input.m_transformA;
      transformB : Transform_3d renames input.m_transformB;

      point,
      normal : aliased math.Vector_3;

      timeOfImpact : aliased math.Real := 1.0;
      depth        : aliased math.Real := 0.0;

      sphereInTr   : Transform_3d;
   begin
      --        output.m_distance = impact.d3.Scalar(BT_LARGE_FLOAT);

      --  move sphere into triangle space
      --
      sphereInTr := inverseTimes (transformB, transformA);

      if Self.collide (sphereInTr.Translation,
                       point'Access, normal'Access, depth'Access,
                       timeOfImpact'Access,
                       Self.m_contactBreakingThreshold)
      then

         if swapResults then
            declare
               normalOnB : constant math.Vector_3 :=  transformB.Rotation * normal;
               normalOnA : constant math.Vector_3 := -normalOnB;
               pointOnA  : constant math.Vector_3 :=  transformB * point + normalOnB*depth;
            begin
               output.addContactPoint (normalOnA, pointOnA,  depth);
            end;
         else
            output.addContactPoint (transformB.Rotation * normal,  transformB * point,   depth);
         end if;
      end if;
   end getClosestPoints;










   --  See also geometrictools.com
   --  Basic idea: D = |p - (lo + t0*lv)| where t0 = lv . (p - lo) / lv . lv
   --
   function SegmentSqrDistance (from, to, p : in     math.Vector_3;
                                nearest     : access math.Vector_3) return math.Real
   is
      use impact.d3.Vector, math.Vectors;

      diff  : math.Vector_3 := p  - from;
      v     : constant math.Vector_3 := to - from;

      t     : math.Real     := dot (v, diff);
      dotVV : math.Real;

   begin
      if t > 0.0 then
         dotVV := dot (v, v);

         if t < dotVV then
            t    := t / dotVV;
            diff := diff - t*v;
         else
            t    := 1.0;
            diff := diff - v;
         end if;
      else
         t := 0.0;
      end if;

      nearest.all := from + t*v;

      return dot (diff, diff);
   end SegmentSqrDistance;






   function collide (Self : in Item;   sphereCenter             : in     math.Vector_3;
                                       point                    : access math.Vector_3;
                                       resultNormal             : access math.Vector_3;
                                       depth                    : access math.Real;
                                       timeOfImpact             : access math.Real;
                                       contactBreakingThreshold : in     math.Real) return Boolean
   is
      pragma Unreferenced (timeOfImpact);
      use impact.d3.Scalar, impact.d3.Vector,  math.Vectors;

      vertex_1 : math.Vector_3 renames Self.m_triangle.getVertexPtr (1).all;
      vertex_2 : math.Vector_3 renames Self.m_triangle.getVertexPtr (2).all;
      vertex_3 : math.Vector_3 renames Self.m_triangle.getVertexPtr (3).all;

      radius               : math.Real     := Self.m_sphere.getRadius;
      radiusWithThreshold  : math.Real     := radius + contactBreakingThreshold;

      normal               : aliased math.Vector_3 :=  Normalized (cross (vertex_2 - vertex_1,
                                                                          vertex_3 - vertex_1));

      p1ToCentre           : constant math.Vector_3 := sphereCenter - vertex_1;
      distanceFromPlane    : math.Real     := dot (p1ToCentre, normal);

      isInsideContactPlane : Boolean       := distanceFromPlane < radiusWithThreshold;

      hasContact           : Boolean       := False;
      contactPoint         : math.Vector_3;

   begin
      if distanceFromPlane < 0.0 then

         --  triangle facing the other way
         distanceFromPlane := -distanceFromPlane;
         normal            := -normal;
      end if;

      isInsideContactPlane := distanceFromPlane < radiusWithThreshold;

      --  Check for contact / intersection
      --
      hasContact := False;

      if isInsideContactPlane then

         if Self.facecontains (sphereCenter, Vector_3_array (Self.m_triangle.m_vertices1), normal'Access) then   -- Inside the contact wedge - touches a point on the shell plane
            hasContact   := True;
            contactPoint := sphereCenter  -  normal * distanceFromPlane;

         else                                                    -- Could be inside one of the contact capsules
            declare
               contactCapsuleRadiusSqr :         math.Real    := radiusWithThreshold * radiusWithThreshold;
               nearestOnEdge           : aliased math.Vector_3;
            begin
               for i in 1 .. Self.m_triangle.getNumEdges
               loop
                  declare
                     pa, pb      : math.Vector_3;
                     distanceSqr : math.Real;
                  begin
                     Self.m_triangle.getEdge (i,  pa, pb);

                     distanceSqr := SegmentSqrDistance (pa, pb,  sphereCenter,  nearestOnEdge'Access);

                     if distanceSqr < contactCapsuleRadiusSqr then   -- Yep, we're inside a capsule.
                        hasContact   := True;
                        contactPoint := nearestOnEdge;
                     end if;
                  end;
               end loop;
            end;
         end if;

      end if;


      if hasContact then
         declare
            use math.Functions;

            contactToCentre : constant math.Vector_3 := sphereCenter - contactPoint;
            distanceSqr     : constant math.Real     := length2 (contactToCentre);
            distance        : math.Real;
         begin
            if distanceSqr < radiusWithThreshold * radiusWithThreshold then

               if distanceSqr > SIMD_EPSILON then
                  distance         :=  sqRt (distanceSqr);
                  resultNormal.all :=  contactToCentre;       normalize (resultNormal.all);
                  point.all        :=  contactPoint;
                  depth.all        := -(radius - distance);
               else
                  distance         :=  0.0;
                  resultNormal.all :=  normal;
                  point.all        :=  contactPoint;
                  depth.all        := -radius;
               end if;

               return True;
            end if;
         end;
      end if;


      return False;
   end collide;










   function pointInTriangle (Self : in Item;   vertices : in     Vector_3_array;
                                               normal   : in     math.Vector_3;
                             p        : access math.Vector_3) return Boolean
   is
      pragma Unreferenced (Self);
      use impact.d3.Vector,  math.Vectors;

      p1           : math.Vector_3 renames vertices (1);
      p2           : math.Vector_3 renames vertices (2);
      p3           : math.Vector_3 renames vertices (3);

      edge1        : constant math.Vector_3 := p2 - p1;
      edge2        : constant math.Vector_3 := p3 - p2;
      edge3        : constant math.Vector_3 := p1 - p3;

      p1_to_p      : constant math.Vector_3 := p.all - p1;
      p2_to_p      : constant math.Vector_3 := p.all - p2;
      p3_to_p      : constant math.Vector_3 := p.all - p3;

      edge1_normal : constant math.Vector_3 := cross (edge1, normal);
      edge2_normal : constant math.Vector_3 := cross (edge2, normal);
      edge3_normal : constant math.Vector_3 := cross (edge3, normal);

      r1, r2, r3   : math.Real;

   begin
      r1 := dot (edge1_normal, p1_to_p);
      r2 := dot (edge2_normal, p2_to_p);
      r3 := dot (edge3_normal, p3_to_p);

      return    (r1 >  0.0 and then r2 >  0.0 and then r3 >  0.0)
        or else (r1 <= 0.0 and then r2 <= 0.0 and then r3 <= 0.0);
   end pointInTriangle;







   function facecontains (Self : in Item;   p        : in     math.Vector_3;
                                               vertices : in     Vector_3_array;
                          normal   : access math.Vector_3) return Boolean
   is
      lp      : aliased math.Vector_3 := p;
      lnormal : constant math.Vector_3 := normal.all;
   begin
      return Self.pointInTriangle (vertices, lnormal, lp'Access);
   end facecontains;




end impact.d3.collision.Detector.discrete.sphere_triangle;
