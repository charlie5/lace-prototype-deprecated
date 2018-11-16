with impact.d2.Distance;



package body impact.d2.Colliders
is

   use type int32;



   --   Determine if two generic shapes overlap.
   --
   function b2TestOverlap (shapeA, shapeB : in impact.d2.Shape.view;
                           xfA,    xfB    : in b2Transform   ) return Boolean
   is
      use impact.d2.Distance;

      input  :         b2DistanceInput;
      cache  : aliased b2SimplexCache;
      output : aliased b2DistanceOutput;
   begin
--        set (input.proxyA, shapeA);
--        set (input.proxyB, shapeB);

      input.transformA := xfA;
      input.transformB := xfB;
      input.useRadii := True;

      cache.count := 0;

      b2Distance (output'Access, cache'Access, input);

      return output.distance < 10.0 * b2_epsilon;
   end b2TestOverlap;




   --   Compute the collision manifold between two circles.
   --
   procedure b2CollideCircles (manifold : access collision.b2Manifold;   circleA : access constant b2CircleShape'Class;   xfA : in b2Transform;
                                                                         circleB : access constant b2CircleShape'Class;   xfB : in b2Transform)
   is
      pA      : constant b2Vec2  := b2Mul (xfA, circleA.m_p);
      pB      : constant b2Vec2  := b2Mul (xfB, circleB.m_p);

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
      manifold.localPoint  := circleA.m_p;
      manifold.localNormal := (0.0, 0.0);
      manifold.pointCount  := 1;

      manifold.points (1).localPoint := circleB.m_p;
      manifold.points (1).id.key     := 0;
   end b2CollideCircles;





   procedure b2CollidePolygonAndCircle (manifold : access collision.b2Manifold;   polygon : access constant b2PolygonShape'Class;   xfA : in b2Transform;
                                                                                  circle  : access constant b2CircleShape'Class;    xfB : in b2Transform)
   is
      --  Compute circle position in the frame of the polygon.
      c      : constant b2Vec2 := b2Mul  (xfB, circle.m_p);
      cLocal : constant b2Vec2 := b2MulT (xfA, c);

      --  Find the min separating edge.
      normalIndex : int32        :=      0;
      separation  : float32      :=     -b2_maxFloat;
      radius      : float32      :=      polygon.m_radius + circle.m_radius;
      vertexCount : constant int32        :=      polygon.m_vertexCount;
      vertices    : b2Vec2_array renames polygon.m_vertices;
      normals     : b2Vec2_array renames polygon.m_normals;
   begin
      manifold.pointCount := 0;

      for i in 1 .. vertexCount loop
         declare
            s : constant float32 := b2Dot (normals (i), cLocal - vertices (i));
         begin
            if s > radius then
               return;           -- Early out.
            end if;

            if s > separation then
               separation  := s;
               normalIndex := i;
            end if;
         end;
      end loop;

      --  Vertices that subtend the incident face.
      --
      declare
         vertIndex1 : constant int32 := normalIndex;

         function get_vertIndex2 return int32
         is
         begin
            if vertIndex1 < vertexCount then return vertIndex1 + 1;
            else                             return 1;
            end if;
         end get_vertIndex2;

         vertIndex2 : constant int32  := get_vertIndex2;

         v1         : constant b2Vec2 := vertices (vertIndex1);
         v2         : constant b2Vec2 := vertices (vertIndex2);

         u1, u2     : float32;
      begin
         --  If the center is inside the polygon ...
         if separation < b2_epsilon then
            manifold.pointCount := 1;
            manifold.kind       := collision.e_faceA;

            manifold.localNormal := normals (normalIndex);
            manifold.localPoint  := 0.5 * (v1 + v2);

            manifold.points (1).localPoint := circle.m_p;
            manifold.points (1).id.key     := 0;

            return;
         end if;

         --  Compute barycentric coordinates
         --
         u1 := b2Dot (cLocal - v1,  v2 - v1);
         u2 := b2Dot (cLocal - v2,  v1 - v2);

         if u1 <= 0.0 then
            if b2DistanceSquared (cLocal, v1)  >  radius * radius then
               return;
            end if;
            manifold.pointCount  := 1;
            manifold.kind        := collision.e_faceA;
            manifold.localNormal := cLocal - v1;
            Normalize (manifold.localNormal);
            manifold.localPoint := v1;
            manifold.points (1).localPoint := circle.m_p;
            manifold.points (1).id.key := 0;
         elsif u2 <= 0.0 then
            if b2DistanceSquared (cLocal, v2) > radius * radius then
               return;
            end if;

            manifold.pointCount  := 1;
            manifold.kind        := collision.e_faceA;
            manifold.localNormal := cLocal - v2;
            Normalize (manifold.localNormal);
            manifold.localPoint  := v2;
            manifold.points (1).localPoint := circle.m_p;
            manifold.points (1).id.key     := 0;
         else
            declare
               faceCenter : constant b2Vec2  := 0.5 * (v1 + v2);
               separation : constant float32 := b2Dot (cLocal - faceCenter,  normals (vertIndex1));
            begin
               if separation > radius then
                  return;
               end if;

               manifold.pointCount  := 1;
               manifold.kind        := collision.e_faceA;
               manifold.localNormal := normals (vertIndex1);
               manifold.localPoint  := faceCenter;
               manifold.points (1).localPoint := circle.m_p;
               manifold.points (1).id.key     := 0;
            end;
         end if;
      end;
   end b2CollidePolygonAndCircle;








   --  Find the separation between poly1 and poly2 for a give edge normal on poly1.
   --
   function b2EdgeSeparation (poly1 : access constant b2PolygonShape'Class;   xf1 : in b2Transform;
                              edge1 : in              int32;
                              poly2 : access constant b2PolygonShape'Class;   xf2 : in b2Transform) return float32
   is
      count1    : constant int32        :=      poly1.m_vertexCount;
      vertices1 : b2Vec2_array renames poly1.m_vertices;
      normals1  : b2Vec2_array renames poly1.m_normals;

      count2    : constant int32        :=      poly2.m_vertexCount;
      vertices2 : b2Vec2_array renames poly2.m_vertices;

      pragma Assert (1 <= edge1 and then edge1 <= count1);

      --  Convert normal from poly1's frame into poly2's frame.
      normal1World : constant b2Vec2 := b2Mul  (xf1.R, normals1 (edge1));
      normal1      : constant b2Vec2 := b2MulT (xf2.R, normal1World);

      --  Find support vertex on poly2 for -normal.
      index  : int32   := 0;
      minDot : float32 := b2_maxFloat;

      v1, v2     : b2Vec2;
      dot,
      separation : float32;
   begin
      for i in 1 .. count2 loop
         dot := b2Dot (vertices2 (i),  normal1);

         if dot < minDot then
            minDot := dot;
            index  := i;
         end if;
      end loop;

      v1 := b2Mul (xf1,  vertices1 (edge1));
      v2 := b2Mul (xf2,  vertices2 (index));

      separation := b2Dot (v2 - v1,  normal1World);
      return separation;
   end b2EdgeSeparation;





   --  Find the max separation between poly1 and poly2 using edge normals from poly1.
   --
   function b2FindMaxSeparation (edgeIndex : access          int32;
                                 poly1     : access constant b2PolygonShape'Class;   xf1 : in b2Transform;
                                 poly2     : access constant b2PolygonShape'Class;   xf2 : in b2Transform) return float32
   is
      count1   : constant int32        :=      poly1.m_vertexCount;
      normals1 : b2Vec2_array renames poly1.m_normals;

      --  Vector pointing from the centroid of poly1 to the centroid of poly2.
      d        : constant b2Vec2  := b2Mul (xf2, poly2.m_centroid)  -  b2Mul (xf1, poly1.m_centroid);
      dLocal1  : constant b2Vec2  := b2MulT (xf1.R, d);

      --  Find edge normal on poly1 that has the largest projection onto d.
      edge     : int32   := 0;
      maxDot   : float32 := -b2_maxFloat;

      dot      : float32;
   begin
      for i in 1 .. count1 loop
         dot := b2Dot (normals1 (i),  dLocal1);

         if dot > maxDot then
            maxDot := dot;
            edge   := i;
         end if;
      end loop;


      declare
         --  Get the separation for the edge normal.
         s : float32 := b2EdgeSeparation (poly1, xf1, edge, poly2, xf2);

         --  Check the separation for the previous edge normal.
         function get_prevEdge return int32
         is
         begin
            if edge = 1 then   return count1;
            else               return edge - 1;
            end if;
         end get_prevEdge;

         prevEdge : constant int32   := get_prevEdge;
         sPrev    : float32 := b2EdgeSeparation (poly1, xf1, prevEdge, poly2, xf2);

         --  Check the separation for the next edge normal.
         function get_nextEdge return int32
         is
         begin
            if edge = count1   then   return 1;
            else               return edge + 1;
            end if;
         end get_nextEdge;

         nextEdge : constant int32   := get_nextEdge;
         sNext    : float32 := b2EdgeSeparation (poly1, xf1, nextEdge, poly2, xf2);

         --  Find the best edge and the search direction.
         bestEdge       : int32;
         bestSeparation : float32;
         increment      : int32;
      begin
         if sPrev > s and then sPrev > sNext then
            increment      := -1;
            bestEdge       := prevEdge;
            bestSeparation := sPrev;
         elsif sNext > s then
            increment      := 1;
            bestEdge       := nextEdge;
            bestSeparation := sNext;
         else
            edgeIndex.all  := edge;
            return s;
         end if;

         --  Perform a local search for the best edge normal.
         --
         loop
            if increment = -1 then
               if bestEdge = 1 then   edge := count1;
               else                   edge := bestEdge - 1;
               end if;
            else
               if bestEdge = count1 then   edge := 1;
               else                        edge := bestEdge + 1;
               end if;
            end if;

            s := b2EdgeSeparation (poly1, xf1, edge, poly2, xf2);

            if s > bestSeparation then
               bestEdge       := edge;
               bestSeparation := s;
            else
               exit;
            end if;
         end loop;

         edgeIndex.all := bestEdge;
         return bestSeparation;
      end;
   end b2FindMaxSeparation;







   procedure b2FindIncidentEdge (c     : access          collision.b2ClipVertices;
                                 poly1 : access constant b2PolygonShape;   xf1 : in b2Transform;
                                 edge1 : in              int32;
                                 poly2 : access constant b2PolygonShape;   xf2 : in b2Transform)
   is
      count1    : constant int32        :=      poly1.m_vertexCount;
      normals1  : b2Vec2_array renames poly1.m_normals;

      count2    : constant int32        :=       poly2.m_vertexCount;
      vertices2 : b2Vec2_array renames  poly2.m_vertices;
      normals2  : b2Vec2_array renames  poly2.m_normals;

      pragma Assert (1 <= edge1 and then edge1 <= count1);

      --  Get the normal of the reference edge in poly2's frame.
      normal1 : constant b2Vec2  := b2MulT (xf2.R,  b2Mul (xf1.R, normals1 (edge1)));

      --  Find the incident edge on poly2.
      index   : int32   := 1;
      minDot  : float32 := b2_maxFloat;

      dot     : float32;
      i1, i2  : int32;
   begin
      for i in 1 .. count2 loop
         dot := b2Dot (normal1,  normals2 (i));

         if dot < minDot then
            minDot := dot;
            index  := i;
         end if;
      end loop;

      --  Build the clip vertices for the incident edge.
      --
      i1 := index;

      if i1 = count2 then
         i2 := 1;
      else
         i2 := i1 + 1;
      end if;

      c (1).v                          := b2Mul (xf2,  vertices2 (i1));
      c (1).id.features.referenceEdge  := uint8 (edge1);
      c (1).id.features.incidentEdge   := uint8 (i1);
      c (1).id.features.incidentVertex := 1;

      c (2).v                          := b2Mul (xf2, vertices2 (i2));
      c (2).id.features.referenceEdge  := uint8 (edge1);
      c (2).id.features.incidentEdge   := uint8 (i2);
      c (2).id.features.incidentVertex := 2;
   end b2FindIncidentEdge;





   --   Find edge normal of max separation on A - return if separating axis is found
   --   Find edge normal of max separation on B - return if separation axis is found
   --   Choose reference edge as min(minA, minB)
   --   Find incident edge
   --   Clip
   --
   procedure b2CollidePolygons (manifold : access collision.b2Manifold;   polygonA : access constant b2PolygonShape'Class;   xfA : in b2Transform;
                                                                          polygonB : access constant b2PolygonShape'Class;   xfB : in b2Transform)
   is
      totalRadius   :         float32 := polygonA.m_radius + polygonB.m_radius;

      edgeA         : aliased int32   := 0;
      separationA   : constant float32 := b2FindMaxSeparation (edgeA'Access, polygonA, xfA, polygonB, xfB);

      edgeB         : aliased int32;
      separationB   :         float32;

      poly1         : access constant b2PolygonShape;        -- reference polygon
      poly2         : access constant b2PolygonShape;        -- incident polygon

      xf1, xf2      : b2Transform;
      edge1         : int32;                -- reference edge
      flip          : uint8;

      k_relativeTol : constant := 0.98;
      k_absoluteTol : constant := 0.001;

      incidentEdge  : aliased collision.b2ClipVertices;
   begin
      manifold.pointCount := 0;

      if separationA > totalRadius then
         return;
      end if;

      edgeB       := 0;
      separationB := b2FindMaxSeparation (edgeB'Access, polygonB, xfB, polygonA, xfA);

      if separationB > totalRadius then
         return;
      end if;

      if separationB  >  k_relativeTol * separationA + k_absoluteTol then
         poly1 := polygonB;
         poly2 := polygonA;
         xf1   := xfB;
         xf2   := xfA;
         edge1 := edgeB;
         manifold.kind := collision.e_faceB;
         flip  := 1;
      else
         poly1 := polygonA;
         poly2 := polygonB;
         xf1   := xfA;
         xf2   := xfB;
         edge1 := edgeA;
         manifold.kind := collision.e_faceA;
         flip  := 0;
      end if;

      b2FindIncidentEdge (incidentEdge'Access, poly1, xf1, edge1, poly2, xf2);

      declare
         count1    : constant int32        :=      poly1.m_vertexCount;
         vertices1 : b2Vec2_array renames poly1.m_vertices;

         v11 : b2Vec2 := vertices1 (edge1);

         function get_v12 return b2Vec2
         is
         begin
            if edge1 = count1 then   return vertices1 (1);
            else                     return vertices1 (edge1 + 1);
            end if;
         end get_v12;

         v12 : b2Vec2 := get_v12;

         localTangent : constant b2Vec2 := normalize (v12 - v11);

         localNormal  : constant b2Vec2 := b2Cross (localTangent, 1.0);
         planePoint   : constant b2Vec2 := 0.5 * (v11 + v12);

         tangent      : constant b2Vec2 := b2Mul (xf1.R, localTangent);
         normal       : constant b2Vec2 := b2Cross (tangent, 1.0);

         frontOffset,
         sideOffset1,
         sideOffset2  : float32;

         clipPoints1,
         clipPoints2  : aliased collision.b2ClipVertices;

         np           : int32;
         pointCount   : int32;

         separation   : float32;
      begin
         v11 := b2Mul (xf1, v11);
         v12 := b2Mul (xf1, v12);

         --  Face offset.
         frontOffset := b2Dot (normal, v11);

         --  Side offsets, extended by polytope skin thickness.
         sideOffset1 := -b2Dot (tangent, v11) + totalRadius;
         sideOffset2 :=  b2Dot (tangent, v12) + totalRadius;

         --  Clip incident edge against extruded edge1 side edges.
         --

         --  Clip to box side 1
         np := collision.b2ClipSegmentToLine (clipPoints1'Access, incidentEdge, -tangent, sideOffset1);

         if np < 2 then
            return;
         end if;


         --  Clip to negative box side 1
         np := collision.b2ClipSegmentToLine (clipPoints2'Access, clipPoints1,  tangent, sideOffset2);

         if np < 2 then
            return;
         end if;

         --  Now clipPoints2 contains the clipped points.
         manifold.localNormal := localNormal;
         manifold.localPoint  := planePoint;

         pointCount := 0;

         for i in int32'(1) .. b2_maxManifoldPoints loop
            separation := b2Dot (normal,  clipPoints2 (i).v) - frontOffset;

            if separation <= totalRadius then
               pointCount := pointCount + 1;

               declare
                  cp : collision.b2ManifoldPoint renames manifold.points (pointCount);
               begin
                  cp.localPoint       := b2MulT (xf2, clipPoints2 (i).v);
                  cp.id               := clipPoints2 (i).id;
                  cp.id.features.flip := flip;
               end;
            end if;
         end loop;

         manifold.pointCount := pointCount;
      end;
   end b2CollidePolygons;



end impact.d2.Colliders;
