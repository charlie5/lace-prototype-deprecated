with

     interfaces.C;

--- for debug



package body impact.d2.orbs.Distance
is
   use Interfaces;




   procedure set (Self : in out b2DistanceProxy;   shape : in impact.d2.orbs.Shape.view)
   is
      circle : constant impact.d2.orbs.Shape.view := shape;
   begin
      Self.m_vertices := (0.0, 0.0); -- circle.m_p'Access;
      Self.m_count    := 1;
      Self.m_radius   := circle.m_radius;
   end set;





   function  getSupport (Self : in b2DistanceProxy;   d : b2Vec2) return int32
   is
      bestIndex : int32   := 1;
      bestValue : float32 := b2Dot (getVertex (Self, 1),  d);

      value     : float32;
   begin
      for i in 2 .. Self.m_count loop
         value := b2Dot (getVertex (Self, i),  d);

         if value > bestValue then
            bestIndex := i;
            bestValue := value;
         end if;
      end loop;

      return bestIndex;
   end getSupport;



   function  getSupportVertex (Self : in b2DistanceProxy;   d : b2Vec2) return b2Vec2
   is
   begin
      return getVertex (Self, getSupport (Self, d));
   end getSupportVertex;



   function  getVertexCount (Self : in b2DistanceProxy) return int32
   is
   begin
      return Self.m_count;
   end getVertexCount;



   function  getVertex (Self : in b2DistanceProxy;   index : int32) return b2Vec2
   is
   begin
      pragma Assert (1 <= index and then index <= Self.m_count);

      return (0.0, 0.0);
   end getVertex;








   --  GJK using Voronoi regions (Christer Ericson) and Barycentric coordinates.
   --


   b2_gjkCalls,
   b2_gjkIters,
   b2_gjkMaxIters : int32;






   type b2SimplexVertex is
      record
         wA     : b2Vec2;        -- support point in proxyA
         wB     : b2Vec2;        -- support point in proxyB

         w      : b2Vec2;        -- wB - wA
         a      : float32;        -- barycentric coordinate for closest point

         indexA : int32;        -- wA index
         indexB : int32;        -- wB index
      end record;

   type b2SimplexVertices is array (int32 range 1 .. 3) of b2SimplexVertex;



   type b2Simplex is
      record
          m_v     : b2SimplexVertices;
          m_count : int32;
      end record;




   function getMetric (Self : in b2Simplex) return float32
   is
   begin
      case Self.m_count is
      when 0 =>
         pragma Assert (False);
         return 0.0;

      when 1 =>
         return 0.0;

      when 2 =>
         return b2Distance (Self.m_v (1).w,
                            Self.m_v (2).w);

      when 3 =>
         return b2Cross (Self.m_v (2).w - Self.m_v (1).w,
                         Self.m_v (3).w - Self.m_v (1).w);

      when others =>
         pragma Assert (False);
         return 0.0;
      end case;
   end getMetric;





   procedure ReadCache (Self : in out b2Simplex;   cache                  : in b2SimplexCache;
                                                   proxyA,     proxyB     : in b2DistanceProxy;
                                                   transformA, transformB : in b2Transform)
   is
      wALocal,
      wBLocal : b2Vec2;
   begin
      pragma Assert (cache.count <= 3);

      --  Copy data from cache.
      Self.m_count := int32 (cache.count);

      for i in 1 .. self.m_count loop
         declare
            v : b2SimplexVertex renames self.m_v (i);
         begin
            v.indexA := int32 (cache.indexA (i));
            v.indexB := int32 (cache.indexB (i));

            wALocal := GetVertex (proxyA, v.indexA);
            wBLocal := GetVertex (proxyB, v.indexB);

            v.wA := b2Mul (transformA, wALocal);
            v.wB := b2Mul (transformB, wBLocal);

            v.w := v.wB - v.wA;
            v.a := 0.0;
         end;
      end loop;

      --  Compute the new simplex metric, if it is substantially different than
      --  old metric then flush the simplex.
      if Self.m_count > 1 then
         declare
            metric1 : float32 := cache.metric;
            metric2 : float32 := getMetric (Self);
         begin
            if              metric2 < 0.5 * metric1
              or else 2.0 * metric1 < metric2
              or else       metric2 < b2_epsilon
            then
               Self.m_count := 0;                  -- Reset the simplex.
            end if;
         end;
      end if;


      if Self.m_count = 0 then                 -- If the cache is empty or invalid ...
         declare
            v : b2SimplexVertex renames Self.m_v (1);
         begin
            v.indexA := 1;
            v.indexB := 1;

            wALocal := GetVertex (proxyA, 1);
            wBLocal := GetVertex (proxyB, 1);

            v.wA := b2Mul (transformA, wALocal);
            v.wB := b2Mul (transformB, wBLocal);

            v.w     := v.wB - v.wA;
            Self.m_count := 1;
         end;
      end if;
   end ReadCache;



   procedure writeCache (Self : in b2Simplex;   cache : access b2SimplexCache)
   is
   begin
      cache.metric := GetMetric (Self);
      cache.count  := uint16 (Self.m_count);

      for i in 1 .. Self.m_count loop
         cache.indexA (i) := uint8 (Self.m_v (i).indexA);
         cache.indexB (i) := uint8 (Self.m_v (i).indexB);
      end loop;
   end writeCache;





   function GetSearchDirection (Self : in b2Simplex) return b2Vec2
   is
   begin
      case Self.m_count is
         when 1 =>
            return -Self.m_v (1).w;

         when 2 =>
            declare
               e12 : constant b2Vec2  := Self.m_v (2).w  -  Self.m_v (1).w;
               sgn : constant float32 := b2Cross (e12,  -Self.m_v (1).w);
            begin
               if sgn > 0.0 then
                  return b2Cross (1.0, e12);      -- Origin is left of e12.
               else
                  return b2Cross (e12, 1.0);     -- Origin is right of e12.
               end if;
            end;

         when others =>
            pragma Assert (False);
            return b2Vec2_zero;
      end case;
   end GetSearchDirection;





   function getClosestPoint (Self : in b2Simplex) return b2Vec2
   is
   begin
      case Self.m_count is
         when 0 =>
            pragma Assert (False);
            return b2Vec2_zero;

         when 1 =>
            return Self.m_v (1).w;

         when 2 =>
            return Self.m_v (1).a * Self.m_v (1).w + Self.m_v (2).a * Self.m_v (2).w;

         when 3 =>
            return b2Vec2_zero;

         when others =>
            pragma Assert (False);
            return b2Vec2_zero;
      end case;
   end getClosestPoint;



   procedure    GetWitnessPoints (Self : in b2Simplex;   pA, pB : access b2Vec2)
   is
   begin
      case Self.m_count is
         when 0 =>
            pragma Assert (False);
            raise Program_Error;

         when 1 =>
            pA.all := Self.m_v (1).wA;
            pB.all := Self.m_v (1).wB;

         when 2 =>
            pA.all := Self.m_v (1).a * Self.m_v (1).wA  +  Self.m_v (2).a * Self.m_v (2).wA;
            pB.all := Self.m_v (1).a * Self.m_v (1).wB  +  Self.m_v (2).a * Self.m_v (2).wB;

         when 3 =>
            pA.all := Self.m_v (1).a * Self.m_v (1).wA + Self.m_v (2).a * Self.m_v (2).wA + Self.m_v (3).a * Self.m_v (3).wA;
            pB.all := pA.all;

         when others =>
            pragma Assert (False);
            raise Program_Error;
      end case;
   end GetWitnessPoints;






   --   Solve a line segment using barycentric coordinates.
   --
   --   p = a1 * w1 + a2 * w2
   --   a1 + a2 = 1
   --
   --   The vector from the origin to the closest point on the line is
   --   perpendicular to the line.
   --   e12 = w2 - w1
   --   dot(p, e) = 0
   --   a1 * dot(w1, e) + a2 * dot(w2, e) = 0
   --
   --   2-by-2 linear system
   --   [1      1     ][a1] = [1]
   --   [w1.e12 w2.e12][a2] = [0]
   --
   --   Define
   --   d12_1 =  dot(w2, e12)
   --   d12_2 = -dot(w1, e12)
   --   d12 = d12_1 + d12_2
   --
   --   Solution
   --   a1 = d12_1 / d12
   --   a2 = d12_2 / d12
   --
   procedure Solve2 (Self : in out b2Simplex)
   is
      w1  : constant b2Vec2 := Self.m_v (1).w;
      w2  : constant b2Vec2 := Self.m_v (2).w;
      e12 : constant b2Vec2 := w2 - w1;

      d12_1,
      d12_2,
      inv_d12 : float32;
   begin
      --  w1 region
      --
      d12_2 := -b2Dot (w1, e12);

      if d12_2 <= 0.0 then
         --  a2 <= 0, so we clamp it to 0
         Self.m_v (1).a := 1.0;
         Self.m_count   := 1;

         return;
      end if;

      --  w2 region
      --
      d12_1 := b2Dot (w2, e12);

      if d12_1 <= 0.0 then
         --  a1 <= 0, so we clamp it to 0
         Self.m_v (2).a := 1.0;
         Self.m_count   := 1;
         Self.m_v (1)   := Self.m_v (2);

         return;
      end if;

      --  Must be in e12 region.
      --
      inv_d12        := 1.0 / (d12_1 + d12_2);
      Self.m_v (1).a := d12_1 * inv_d12;
      Self.m_v (2).a := d12_2 * inv_d12;
      Self.m_count   := 2;
   end Solve2;





   --  Possible regions:
   --  - points[2]
   --  - edge points[0]-points[2]
   --  - edge points[1]-points[2]
   --  - inside the triangle
   --
   procedure Solve3 (Self : in out b2Simplex)
   is
      w1 : constant b2Vec2 := Self.m_v (1).w;
      w2 : constant b2Vec2 := Self.m_v (2).w;
      w3 : constant b2Vec2 := Self.m_v (3).w;

      --  Edge12
      --  [1      1     ][a1] = [1]
      --  [w1.e12 w2.e12][a2] = [0]
      --  a3 = 0
      e12   : constant b2Vec2  := w2 - w1;
      w1e12 : constant float32 := b2Dot (w1, e12);
      w2e12 : constant float32 := b2Dot (w2, e12);
      d12_1 : float32 :=  w2e12;
      d12_2 : float32 := -w1e12;

      --  Edge13
      --  [1      1     ][a1] = [1]
      --  [w1.e13 w3.e13][a3] = [0]
      --  a2 = 0
      e13   : constant b2Vec2 := w3 - w1;
      w1e13 : constant float32 := b2Dot (w1, e13);
      w3e13 : constant float32 := b2Dot (w3, e13);
      d13_1 : float32 :=  w3e13;
      d13_2 : float32 := -w1e13;

      --  Edge23
      --  [1      1     ][a2] = [1]
      --  [w2.e23 w3.e23][a3] = [0]
      --  a1 = 0
      e23   : constant b2Vec2  := w3 - w2;
      w2e23 : constant float32 := b2Dot (w2, e23);
      w3e23 : constant float32 := b2Dot (w3, e23);
      d23_1 : float32 :=  w3e23;
      d23_2 : float32 := -w2e23;

      --  Triangle123
      n123   : constant float32 := b2Cross (e12, e13);

      d123_1 : float32 := n123 * b2Cross (w2, w3);
      d123_2 : float32 := n123 * b2Cross (w3, w1);
      d123_3 : float32 := n123 * b2Cross (w1, w2);

      inv_d12,
      inv_d13,
      inv_d23,
      inv_d123 : float32;
   begin
      --  w1 region
      if d12_2 <= 0.0 and then d13_2 <= 0.0
      then
         Self.m_v (1).a := 1.0;
         Self.m_count   := 1;
         return;
      end if;

      --  e12
      if d12_1 > 0.0 and then d12_2 > 0.0 and then d123_3 <= 0.0
      then
         inv_d12 := 1.0 / (d12_1 + d12_2);
         Self.m_v (1).a := d12_1 * inv_d12;
         Self.m_v (2).a := d12_2 * inv_d12;
         Self.m_count := 2;
         return;
      end if;

      --  e13
      if d13_1 > 0.0 and then d13_2 > 0.0 and then d123_2 <= 0.0
      then
         inv_d13        := 1.0 / (d13_1 + d13_2);
         Self.m_v (1).a := d13_1 * inv_d13;
         Self.m_v (3).a := d13_2 * inv_d13;
         Self.m_count   := 2;
         Self.m_v (2)   := Self.m_v (3);
         return;
      end if;

      --  w2 region
      if d12_1 <= 0.0 and then d23_2 <= 0.0
      then
         Self.m_v (2).a := 1.0;
         Self.m_count   := 1;
         Self.m_v (1)   := Self.m_v (2);
         return;
      end if;

      --  w3 region
      if d13_1 <= 0.0 and then d23_1 <= 0.0
      then
         Self.m_v (3).a := 1.0;
         Self.m_count   := 1;
         Self.m_v (1)   := Self.m_v (3);
         return;
      end if;

      --  e23
      if d23_1 > 0.0 and then d23_2 > 0.0 and then d123_1 <= 0.0
      then
         inv_d23        := 1.0 / (d23_1 + d23_2);
         Self.m_v (2).a := d23_1 * inv_d23;
         Self.m_v (3).a := d23_2 * inv_d23;
         Self.m_count   := 2;
         Self.m_v (1)   := Self.m_v (3);
         return;
      end if;

      --  Must be in triangle123
      --
      inv_d123 := 1.0 / (d123_1 + d123_2 + d123_3);

      Self.m_v (1).a := d123_1 * inv_d123;
      Self.m_v (2).a := d123_2 * inv_d123;
      Self.m_v (3).a := d123_3 * inv_d123;

      Self.m_count := 3;
   end Solve3;




   procedure b2Distance (output : access b2DistanceOutput;
                         cache  : access b2SimplexCache;
                         input  : in     b2DistanceInput)
   is
      proxyA : b2DistanceProxy renames input.proxyA;
      proxyB : b2DistanceProxy renames input.proxyB;

      transformA : constant b2Transform := input.transformA;
      transformB : constant b2Transform := input.transformB;

      simplex : b2Simplex;
   begin
      b2_gjkCalls := b2_gjkCalls + 1;

      readCache (simplex,  cache.all, proxyA, proxyB, transformA, transformB);     -- Initialize the simplex.

      declare
         k_maxIters : constant          :=      20;
         vertices   : b2SimplexVertices renames simplex.m_v;

         --  These store the vertices of the last simplex so that we
         --  can check for duplicates and prevent cycling.
         saveA, saveB : array (int32 range 1 .. 3) of int32;
         saveCount    : int32                  := 0;

         closestPoint : constant b2Vec2  := GetClosestPoint (simplex);
         distanceSqr1 : float32 := lengthSquared   (closestPoint);
         distanceSqr2 : float32 := distanceSqr1;

         p, d : b2Vec2;

         iter_Count : int32 := 0;
      begin
         --  Main iteration loop.
         --
         for iter in 1 .. k_maxIters loop
            --  Copy simplex so we can identify duplicates.
            --
            saveCount := simplex.m_count;

            for i in 1 .. saveCount loop
               saveA (i) := vertices (i).indexA;
               saveB (i) := vertices (i).indexB;
            end loop;

            case simplex.m_count is
               when 1 => null;
               when 2 => Solve2 (simplex);
               when 3 => Solve3 (simplex);
               when others => pragma Assert (False); raise Program_Error;
            end case;

            if simplex.m_count = 3 then  -- If we have 3 points, then the origin is in the corresponding triangle.
               exit;
            end if;

            --  Compute closest point.
            --
            p            := GetClosestPoint (simplex);
            distanceSqr2 := LengthSquared (p);

            --  Ensure progress
            --
            if distanceSqr2 >= distanceSqr1 then
               null; -- exit;
            end if;

            distanceSqr1 := distanceSqr2;
            d            := GetSearchDirection (simplex);                    -- Get search direction.


            if LengthSquared (d) < b2_epsilon * b2_epsilon then                -- Ensure the search direction is numerically fit.
               --  The origin is probably contained by a line segment
               --  or triangle. Thus the shapes are overlapped.

               --  We can't return zero here even though there may be overlap.
               --  In case the simplex is a point, segment, or triangle it is difficult
               --  to determine if the origin is contained in the CSO or very close to it.
               exit;
            end if;

            --  Compute a tentative new simplex vertex using support points.
            --
            declare
               vertex    : b2SimplexVertex renames vertices (simplex.m_count + 1);
               wBLocal   : b2Vec2;
               duplicate : Boolean;
            begin
               vertex.indexA := GetSupport (proxyA,  b2MulT (transformA.R, -d));
               vertex.wA     := b2Mul (transformA, getVertex (proxyA, vertex.indexA));
               vertex.indexB := getSupport (proxyB,  b2MulT (transformB.R, d));
               vertex.wB     := b2Mul (transformB, getVertex (proxyB, vertex.indexB));
               vertex.w      := vertex.wB - vertex.wA;

               --  Iteration count is equated to the number of support point calls.
               iter_Count  := iter_Count + 1;
               b2_gjkIters := b2_gjkIters + 1;

               --  Check for duplicate support points. This is the main termination criteria.
               duplicate := False;

               for i in 1 .. saveCount loop
                  if         vertex.indexA = saveA (i)
                    and then vertex.indexB = saveB (i)
                  then
                     duplicate := True;
                     exit;
                  end if;
               end loop;

               --  If we found a duplicate support point we must exit to avoid cycling.
               --
               if duplicate then
                  exit;
               end if;

               --  New vertex is ok and needed.
               simplex.m_count := simplex.m_count + 1;
            end;

         end loop;


         b2_gjkMaxIters := int32'Max (b2_gjkMaxIters, iter_Count);

         --  Prepare output.
         --
         getWitnessPoints (simplex, output.pointA'Access, output.pointB'Access);

         output.distance   := b2Distance (output.pointA, output.pointB);
         output.iterations := iter_Count;


         --  Cache the simplex.
         writeCache (simplex, cache);


         if input.useRadii then     -- Apply radii if requested.
            declare
               rA : float32 := proxyA.m_radius;
               rB : float32 := proxyB.m_radius;

               normal,
               p     : b2Vec2;
            begin
               if         output.distance > rA + rB
                 and then output.distance > b2_epsilon
               then
                  --  Shapes are still no overlapped.
                  --  Move the witness points to the outer surface.
                  output.distance := output.distance - rA + rB;
                  normal          := output.pointB - output.pointA;
                  normalize (normal);
                  output.pointA := output.pointA + rA * normal;
                  output.pointB := output.pointB - rB * normal;
               else
                  --  Shapes are overlapped when radii are considered.
                  --  Move the witness points to the middle.
                  p               := 0.5 * (output.pointA + output.pointB);
                  output.pointA   := p;
                  output.pointB   := p;
                  output.distance := 0.0;
               end if;
            end;
         end if;

      end;

   end b2Distance;


end impact.d2.orbs.Distance;
