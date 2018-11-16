package body impact.d2.Shape.polygon
is





   function to_Polygon return b2PolygonShape
   is
   begin
      return (m_kind        => e_polygon,
              m_radius      => b2_polygonRadius,
              m_vertexCount => 0,
              m_vertices    => <>,
              m_normals     => <>,
              m_centroid    => (0.0, 0.0));
   end to_Polygon;




   overriding function Clone        (Self : in b2PolygonShape) return Shape.view
   is
   begin
      return new b2PolygonShape'(Self);
   end Clone;




   overriding function TestPoint (Self : in b2PolygonShape;   transform : in b2Transform;
                                                   p         : in b2Vec2   ) return Boolean
   is
      pLocal : constant b2Vec2 := b2MulT (transform.R,  p - transform.position);
      dot    : float32;
   begin
      for i in 1 .. Self.m_vertexCount loop
         dot := b2Dot (Self.m_normals (i),  pLocal - Self.m_vertices (i));

         if dot > 0.0 then
            return False;
         end if;
      end loop;

      return True;
   end TestPoint;







   overriding function RayCast (Self : in b2PolygonShape;   output    : access collision.b2RayCastOutput;
                                                 input     : in     collision.b2RayCastInput;
                                                 transform : in     b2Transform            ) return Boolean
   is
      use type int32;
      --  Put the ray into the polygon's frame of reference.
      p1 : constant b2Vec2 := b2MulT (transform.R,  input.p1 - transform.position);
      p2 : constant b2Vec2 := b2MulT (transform.R,  input.p2 - transform.position);
      d  : constant b2Vec2 := p2 - p1;
   begin

      if Self.m_vertexCount = 2 then
         declare
            v1     : constant b2Vec2 := Self.m_vertices (1);
            v2     : constant b2Vec2 := Self.m_vertices (2);
            normal : constant b2Vec2 := Self.m_normals  (1);

            numerator   : float32 := b2Dot (normal, v1 - p1);
            denominator : constant float32 := b2Dot (normal, d);

            t  : float32;
            q  : b2Vec2;
            r  : b2Vec2;
            rr : float32;
            s  : float32;
         begin
            --  Equations:
            --
            --  q = p1 + t * d
            --  dot (normal, q - v1) = 0
            --  dot (normal, p1 - v1)  +  t * dot (normal, d) = 0

            if denominator = 0.0 then
               return False;
            end if;

            t := numerator / denominator;

            if t < 0.0  or else  1.0 < t then
               return False;
            end if;

            q := p1 + t * d;

            --  Equations:
            --
            --  q = v1 + s * r
            --  s = dot(q - v1, r) / dot(r, r)

            r  := v2 - v1;
            rr := b2Dot (r, r);

            if rr = 0.0 then
               return False;
            end if;

            s := b2Dot (q - v1, r) / rr;

            if s < 0.0 or else 1.0 < s then
               return False;
            end if;

            output.fraction := t;

            if numerator > 0.0 then
               output.normal := -normal;
            else
               output.normal :=  normal;
            end if;

            return True;
         end;
      else
         declare
            lower : float32 := 0.0;
            upper : float32 := input.maxFraction;
            index : int32   := -1;

            numerator   : float32;
            denominator : float32;
         begin
            for i in 1 .. self.m_vertexCount loop
               --  Equations:
               --
               --  p = p1 + a * d
               --  dot(normal, p - v) = 0
               --  dot(normal, p1 - v) + a * dot(normal, d) = 0

               numerator   := b2Dot (Self.m_normals (i), Self.m_vertices (i) - p1);
               denominator := b2Dot (Self.m_normals (i), d);

               if denominator = 0.0 then
                  if numerator < 0.0 then
                     return False;
                  end if;
               else
                  --  Note: we want this predicate without division:
                  --  lower < numerator / denominator, where denominator < 0
                  --  Since denominator < 0, we have to flip the inequality:
                  --  lower < numerator / denominator <==> denominator * lower > numerator.
                  --
                  if denominator < 0.0 and then numerator < lower * denominator then
                     --  Increase lower.
                     --  The segment enters this half-space.
                     lower := numerator / denominator;
                     index := i;
                  elsif denominator > 0.0 and then numerator < upper * denominator then
                     --  Decrease upper.
                     --  The segment exits this half-space.
                     upper := numerator / denominator;
                  end if;
               end if;

               --  The use of epsilon here causes the assert on lower to trip
               --  in some cases. Apparently the use of epsilon was to make edge
               --  shapes work, but now those are handled separately.
               --  if (upper < lower - b2_epsilon)

               if upper < lower then
                  return False;
               end if;
            end loop;


            pragma Assert (0.0 <= lower and then lower <= input.maxFraction);

            if index >= 0 then
               output.fraction := lower;
               output.normal   := b2Mul (transform.R, Self.m_normals (index));

               return True;
            end if;
         end;
      end if;

      return False;
   end RayCast;







   overriding procedure ComputeAABB (Self : in b2PolygonShape;   aabb      : access collision.b2AABB;
                                                     transform : in     b2Transform)
   is
      lower : b2Vec2 := b2Mul (transform, Self.m_vertices (1));
      upper : b2Vec2 := lower;
      v     : b2Vec2;
      r     : b2Vec2;
   begin
      for i in 2 .. self.m_vertexCount loop
         v     := b2Mul (transform, Self.m_vertices (i));
         lower := b2Min (lower, v);
         upper := b2Max (upper, v);
      end loop;

      r := (Self.m_radius, Self.m_radius);
      aabb.lowerBound := lower - r;
      aabb.upperBound := upper + r;
   end ComputeAABB;





   overriding procedure ComputeMass (Self : in b2PolygonShape;   massData : access b2MassData;
                                                     density  : in     float32)
   is
      use type int32;
   begin
      --  Polygon mass, centroid, and inertia.
      --
      --           Let rho be the polygon density in mass per unit area.
      --           Then:
      --           mass = rho * int(dA)
      --           centroid.x = (1/mass) * rho * int(x * dA)
      --           centroid.y = (1/mass) * rho * int(y * dA)
      --           I = rho * int((x*x + y*y) * dA)
      --
      --           We can compute these integrals by summing all the integrals
      --           for each triangle of the polygon. To evaluate the integral
      --           for a single triangle, we make a change of variables to
      --           the (u,v) coordinates of the triangle:
      --           x = x0 + e1x * u + e2x * v
      --           y = y0 + e1y * u + e2y * v
      --           where 0 <= u && 0 <= v && u + v <= 1.
      --
      --           We integrate u from [0,1-v] and then v from [0,1].
      --           We also need to use the Jacobian of the transformation:
      --           D = cross(e1, e2)
      --
      --           Simplification: triangle centroid = (1/3) * (p1 + p2 + p3)
      --
      --           The rest of the derivation is handled by computer algebra.
      --

      pragma Assert (Self.m_vertexCount >= 2);

      --  A line segment has zero mass.
      --
      if Self.m_vertexCount = 2 then
         massData.center := 0.5 * (Self.m_vertices (1) + Self.m_vertices (2));
         massData.mass   := 0.0;
         massData.I      := 0.0;
         return;
      end if;

      declare
         center  : b2Vec2  := (0.0, 0.0);
         area    : float32 := 0.0;
         Inertia : float32 := 0.0;

         --  pRef is the reference point for forming triangles.
         --  It's location doesn't change the result (except for rounding error).
         --
         pRef   : constant b2Vec2  := (0.0, 0.0);
         --
         --  #if 0
         --          // This code would put the reference point inside the polygon.
         --          for (int32 i = 0; i < m_vertexCount; ++i)
         --          {
         --                  pRef += m_vertices[i];
         --          }
         --          pRef *= 1.0f / count;
         --  #endif


         k_inv3       : constant := 1.0 / 3.0;

         p1, p2, p3,
         e1, e2       : b2Vec2;

         D,
         triangleArea,
         px, py,
         ex1, ex2,
         ey1, ey2,
         intx2, inty2 : float32;
      begin
         for i in 1 .. Self.m_vertexCount loop
            --  Triangle vertices.
            --
            p1 := pRef;
            p2 := Self.m_vertices (i);

            if i < Self.m_vertexCount then
               p3 := self.m_vertices (i + 1);
            else
               p3 := Self.m_vertices (1);
            end if;

            e1 := p2 - p1;
            e2 := p3 - p1;

            D := b2Cross (e1, e2);

            triangleArea := 0.5 * D;
            area         := area + triangleArea;

            --  Area weighted centroid
            center := center + triangleArea * k_inv3 * (p1 + p2 + p3);

            px  := p1.x;   py  := p1.y;
            ex1 := e1.x;   ey1 := e1.y;
            ex2 := e2.x;   ey2 := e2.y;

            intx2 := k_inv3 * (0.25 * (ex1 * ex1 + ex2*ex1 + ex2*ex2) + (px * ex1 + px*ex2)) + 0.5*px*px;
            inty2 := k_inv3 * (0.25 * (ey1 * ey1 + ey2*ey1 + ey2*ey2) + (py * ey1 + py*ey2)) + 0.5*py*py;

            Inertia := Inertia + D * (intx2 + inty2);
         end loop;

         --  Total mass
         massData.mass := density * area;

         --  Center of mass
         pragma Assert (area > b2_epsilon);

         center          := center * (1.0 / area);
         massData.center := center;

         -- Inertia tensor relative to the local origin.
         massData.I := density * Inertia;
      end;

   end ComputeMass;






   function GetSupport       (Self : in b2PolygonShape;   d : in b2Vec2) return int32
   is
      bestIndex : int32   := 0;
      bestValue : float32 := b2Dot (Self.m_vertices (1), d);
      value     : float32;
   begin
      for i in 2 .. self.m_vertexCount loop
         value := b2Dot (Self.m_vertices (i), d);

         if value > bestValue then
            bestIndex := i;
            bestValue := value;
         end if;
      end loop;

      return bestIndex;
   end GetSupport;





   function GetSupportVertex (Self : in b2PolygonShape;   d : in b2Vec2) return b2Vec2
   is
   begin
      return Self.m_vertices (getSupport (Self, d));
   end GetSupportVertex;




   function GetVertexCount   (Self : in b2PolygonShape               ) return int32
   is
   begin
      return Self.m_vertexCount;
   end GetVertexCount;



   function GetVertex        (Self : in b2PolygonShape;   index : in int32) return b2Vec2
   is
      use type int32;
   begin
      pragma Assert (0 <= index and then index < Self.m_vertexCount);

      return Self.m_vertices (index);
   end GetVertex;





   --  Ensure the polygon is convex and the interior
   --  is to the left of each edge.
   --
   function is_Valid (Self : in b2PolygonShape) return Boolean
   is
      use type int32;
      i1, i2 : int32;
      edge,
      r      : b2Vec2;
      s      : float32;
   begin
      for i in 1 .. self.m_vertexCount loop
         i1 := i;

         if i < self.m_vertexCount then
            i2 := i + 1;
         else
            i2 := 1;
         end if;

         edge := Self.m_vertices (i2) - Self.m_vertices (i1);

         for j in 1 .. self.m_vertexCount loop
            if not (j = i1 or else j = i2) then                        -- Don't check vertices on the current edge.
               r := Self.m_vertices (j) - Self.m_vertices (i1);

               --  Your polygon is non-convex (it has an indentation) or has colinear edges.
               s := b2Cross (edge, r);
               pragma Assert (s > 0.0);
            end if;
         end loop;
      end loop;

      return True;
   end is_Valid;




   function computeCentroid (vs : in b2Vec2_array) return b2Vec2
   is
      use type int32;
      c    : b2Vec2  := (0.0, 0.0);
      area : float32 := 0.0;

      pRef : constant b2Vec2  := (0.0, 0.0);   -- pRef is the reference point for forming triangles.
                                               -- It's location doesn't change the result (except for rounding error).

      --  #if 0
      --          // This code would put the reference point inside the polygon.
      --          for (int32 i = 0; i < count; ++i)
      --          {
      --                  pRef += vs[i];
      --          }
      --          pRef *= 1.0f / count;
      --  #endif

      inv3 : constant := 1.0 / 3.0;

      p1, p2, p3   : b2Vec2;
      e1, e2       : b2Vec2;
      D            : float32;
      triangleArea : float32;
   begin
      pragma Assert (vs'Length >= 2);

      if vs'Length = 2 then
         c := 0.5 * (vs (1) + vs (2));
         return c;
      end if;

      for i in vs'Range loop
         --  Triangle vertices.
         p1 := pRef;
         p2 := vs (i);

         if i < vs'Last then
            p3 := vs (i + 1);
         else
            p3 := vs (1);
         end if;

         e1 := p2 - p1;
         e2 := p3 - p1;

         D := b2Cross (e1, e2);

         triangleArea := 0.5 * D;
         area         := area + triangleArea;

         --  Area weighted centroid
         c := c + triangleArea * inv3 * (p1 + p2 + p3);
      end loop;

      --  Centroid
      --
      pragma Assert (area > b2_epsilon);
      c := c * (1.0 / area);

      return c;
   end computeCentroid;









   procedure set (Self : in out b2PolygonShape;   vertices : in b2Vec2_array)
   is
      use type int32;
      i1, i2 : int32;
      edge   : b2Vec2;
   begin
      pragma Assert (2 <= vertices'Length and then vertices'Length <= b2_maxPolygonVertices);

      Self.m_vertexCount                     := vertices'Length;
      Self.m_vertices (1 .. vertices'Length) := vertices;


      --  Compute normals. Ensure the edges have non-zero length.
      --
      for i in 1 .. self.m_vertexCount loop
         i1 := i;

         if i < Self.m_vertexCount then
            i2 := i + 1;
         else
            i2 := 1;
         end if;

         edge := Self.m_vertices (i2) - Self.m_vertices (i1);
         pragma Assert (LengthSquared (edge)  >  b2_epsilon * b2_epsilon);

         Self.m_normals (i) := b2Cross (edge, 1.0);
         normalize (Self.m_normals (i));
      end loop;

      pragma Assert (is_Valid (Self));

      Self.m_centroid := ComputeCentroid (Self.m_vertices (1 .. Self.m_vertexCount));         -- Compute the polygon centroid.
   end set;







   procedure setAsBox (Self : in out b2PolygonShape;   hx, hy : in  float32)
   is
   begin
      Self.m_vertexCount := 4;

      Self.m_vertices (1) := (-hx, -hy);
      Self.m_vertices (2) := (hx, -hy);
      Self.m_vertices (3) := (hx,  hy);
      Self.m_vertices (4) := (-hx,  hy);

      Self.m_normals (1) := (0.0, -1.0);
      Self.m_normals (2) := (1.0,  0.0);
      Self.m_normals (3) := (0.0,  1.0);
      Self.m_normals (4) := (-1.0,  0.0);

      Self.m_centroid := (0.0, 0.0);
   end setAsBox;



   procedure setAsBox (Self : in out b2PolygonShape;   hx, hy : in float32;
                                                       center : in b2Vec2;
                                                       angle  : in float32)
   is
      xf : b2Transform;
   begin
      Self.m_vertexCount := 4;

      Self.m_vertices (1) := (-hx, -hy);
      Self.m_vertices (2) := (hx, -hy);
      Self.m_vertices (3) := (hx,  hy);
      Self.m_vertices (4) := (-hx,  hy);

      Self.m_normals (1) := (0.0, -1.0);
      Self.m_normals (2) := (1.0,  0.0);
      Self.m_normals (3) := (0.0,  1.0);
      Self.m_normals (4) := (-1.0,  0.0);

      Self.m_centroid := center;

      xf.position := center;
      set (xf.R, angle);

      --  Transform vertices and normals.
      --
      for i in 1 .. self.m_vertexCount loop
         Self.m_vertices (i) := b2Mul (xf,   Self.m_vertices (i));
         Self.m_normals  (i) := b2Mul (xf.R, Self.m_normals  (i));
      end loop;
   end setAsBox;





   procedure setAsEdge (Self : in out b2PolygonShape;   v1, v2 : in b2Vec2)
   is
   begin
      Self.m_vertexCount := 2;

      Self.m_vertices (1) := v1;
      Self.m_vertices (2) := v2;

      Self.m_centroid := 0.5 * (v1 + v2);

      Self.m_normals (1) := b2Cross (v2 - v1, 1.0);
      normalize (Self.m_normals (1));
      Self.m_normals (2) := -Self.m_normals (1);
   end setAsEdge;



end impact.d2.shape.polygon;
