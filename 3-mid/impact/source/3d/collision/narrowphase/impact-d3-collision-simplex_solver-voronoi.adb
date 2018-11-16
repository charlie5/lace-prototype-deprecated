with -- math.algebra.linear.d3,
     impact.d3.Vector;



package body impact.d3.collision.simplex_Solver.voronoi
is


   --- btUsageBitfield
   --

   procedure reset (Self : out btUsageBitfield)
   is
   begin
      self.usedVertexA := False;
      self.usedVertexB := False;
      self.usedVertexC := False;
      self.usedVertexD := False;
   end reset;





   --- btSubSimplexClosestResult
   --

   procedure reset                      (Self : in out btSubSimplexClosestResult)
   is
   begin
      Self.m_degenerate := False;

      setBarycentricCoordinates (Self);
      reset                     (Self.m_usedVertices);
   end reset;




   procedure setBarycentricCoordinates  (Self :    out btSubSimplexClosestResult;   a, b, c, d : in math.Real := 0.0)
   is
   begin
      Self.m_barycentricCoords (1) := a;
      Self.m_barycentricCoords (2) := b;
      Self.m_barycentricCoords (3) := c;
      Self.m_barycentricCoords (4) := d;
   end setBarycentricCoordinates;




   function  isValid                    (Self : in     btSubSimplexClosestResult) return Boolean
   is
   begin
      return     Self.m_barycentricCoords (1) >= 0.0
        and then Self.m_barycentricCoords (2) >= 0.0
        and then Self.m_barycentricCoords (3) >= 0.0
        and then Self.m_barycentricCoords (4) >= 0.0;
   end isValid;








   --- impact.d3.collision.simplex_Solver.voronoi
   --


   VERTA : constant := 1;
   VERTB : constant := 2;
   VERTC : constant := 3;
   VERTD : constant := 4;


   CATCH_DEGENERATE_TETRAHEDRON : constant Boolean := True;






   overriding procedure reset (Self : in out Item)
   is
   begin
      --  clear the simplex, remove all the vertices
      --
      Self.m_cachedValidClosest := False;
      Self.m_numVertices        := 0;
      Self.m_needsUpdate        := True;
      Self.m_lastW              := (math.Infinity, math.Infinity, math.Infinity);

      reset (Self.m_cachedBC);
   end reset;







   overriding procedure addVertex      (Self : in out Item;   w, p, q : in math.Vector_3)
   is
   begin
      Self.m_lastW       := w;
      Self.m_needsUpdate := True;

      Self.m_numVertices := Self.m_numVertices + 1;

      Self.m_simplexVectorW (Self.m_numVertices) := w;
      Self.m_simplexPointsP (Self.m_numVertices) := p;
      Self.m_simplexPointsQ (Self.m_numVertices) := q;
   end addVertex;








   --  return/calculate the closest vertex
   --

   overriding function  closest        (Self : access Item;   v : access math.Vector_3) return Boolean
   is
      Success : constant Boolean := Self.updateClosestVectorAndPoints;
   begin
      v.all := Self.m_cachedV;
      return Success;
   end closest;







   overriding function  maxVertex (Self : in     Item) return Real
   is
      use impact.d3.Vector;

      numverts : constant Integer   := Self.numVertices;
      maxV     : math.Real := 0.0;
      curLen2  : math.Real;
   begin
      for i in 1 .. numverts loop
         curLen2 := length2 (Self.m_simplexVectorW (i));   -- tbd: check length2 is correct.

         if maxV < curLen2 then
            maxV := curLen2;
         end if;
      end loop;

      return maxV;
   end maxVertex;






   overriding function  fullSimplex    (Self : in     Item) return Boolean
   is
   begin
      return Self.m_numVertices = 4;
   end fullSimplex;





   overriding function  getSimplex (Self : in     Item;   pBuf, qBuf, yBuf : access impact.d3.collision.simplex_Solver.Vector_3_array) return Integer
   is
   begin
      for i in 1 .. Self.numVertices loop
         yBuf (i) := Self.m_simplexVectorW (i);
         pBuf (i) := Self.m_simplexPointsP (i);
         qBuf (i) := Self.m_simplexPointsQ (i);
      end loop;

      return Self.numVertices;
   end getSimplex;










   overriding function  inSimplex      (Self : in     Item;   w : in  math.Vector_3) return Boolean
   is
      use impact.d3.Vector, math.Vectors;

      found    : Boolean := False;
      numverts : constant Integer := Self.numVertices;
   begin
      --  w is in the current (reduced) simplex
      --
      for i in 1 .. numverts loop
         if BT_USE_EQUAL_VERTEX_THRESHOLD then
            if distance2 (Self.m_simplexVectorW (i),  w)  <=  Self.m_equalVertexThreshold then
               Found := True;
            end if;
         else
            if Self.m_simplexVectorW (i) = w then
               Found := True;
            end if;
         end if;
      end loop;


      --  check in case lastW is already removed
      --
      if w = Self.m_lastW then
         return True;
      end if;

      return found;
   end inSimplex;






   overriding procedure backup_closest (Self : in out Item;   v : out math.Vector_3)
   is
   begin
      v := Self.m_cachedV;
   end backup_closest;





   overriding function  emptySimplex   (Self : in     Item) return Boolean
   is
   begin
      return Self.numVertices = 0;
   end emptySimplex;







   function  updateClosestVectorAndPoints (Self : access Item) return Boolean
   is
   begin

      if Self.m_needsUpdate then
         reset (Self.m_cachedBC);

         Self.m_needsUpdate := False;


         case Self.numVertices
         is
         when 0 =>
            Self.m_cachedValidClosest := False;


         when 1 =>
            Self.m_cachedP1 := Self.m_simplexPointsP (1);
            Self.m_cachedP2 := Self.m_simplexPointsQ (1);
            Self.m_cachedV  := Self.m_cachedP1 - Self.m_cachedP2;      -- == m_simplexVectorW[0]

            reset (Self.m_cachedBC);

            setBarycentricCoordinates (Self.m_cachedBC,  1.0, 0.0, 0.0, 0.0);
            Self.m_cachedValidClosest := isValid (Self.m_cachedBC);


         when 2 =>
            --  closest point origin from line segment
            declare
               use impact.d3.Vector;
               from    : math.Vector_3 renames Self.m_simplexVectorW (1);
               to      : math.Vector_3 renames Self.m_simplexVectorW (2);
               nearest : math.Vector_3;
               pragma Unreferenced (nearest);

               p     : constant math.Vector_3 := (0.0, 0.0, 0.0);
               diff  : math.Vector_3 := p  - from;
               v     : constant math.Vector_3 := to - from;

               t     : math.Real     := dot (v, diff);  -- v.dot (diff);
               dotVV : math.Real;
            begin
               if t > 0.0 then
                  dotVV := dot (v, v); -- v.dot (v);

                  if t < dotVV then
                     t    := t / dotVV;
                     diff := diff - t*v;

                     Self.m_cachedBC.m_usedVertices.usedVertexA := True;
                     Self.m_cachedBC.m_usedVertices.usedVertexB := True;
                  else
                     t    := 1.0;
                     diff := diff - v;
                     --  reduce to 1 point
                     Self.m_cachedBC.m_usedVertices.usedVertexB := True;
                  end if;

               else
                  t := 0.0;
                  --  reduce to 1 point
                  Self.m_cachedBC.m_usedVertices.usedVertexA := True;
               end if;


               setBarycentricCoordinates (Self.m_cachedBC,  1.0-t, t);
               nearest := from + t*v;

               Self.m_cachedP1 := Self.m_simplexPointsP (1)  +  t * (Self.m_simplexPointsP (2) - Self.m_simplexPointsP (1));
               Self.m_cachedP2 := Self.m_simplexPointsQ (1)  +  t * (Self.m_simplexPointsQ (2) - Self.m_simplexPointsQ (1));

               Self.m_cachedV  := Self.m_cachedP1 - Self.m_cachedP2;

               Self.reduceVertices (Self.m_cachedBC.m_usedVertices);

               Self.m_cachedValidClosest := isValid (Self.m_cachedBC);
            end;


         when 3 =>   -- closest point origin from triangle
            declare
               p : constant math.Vector_3 := (0.0, 0.0, 0.0);

               a : math.Vector_3 renames Self.m_simplexVectorW (1);
               b : math.Vector_3 renames Self.m_simplexVectorW (2);
               c : math.Vector_3 renames Self.m_simplexVectorW (3);

               unused : Boolean;
               pragma Unreferenced (unused);
            begin
               unused := closestPtPointTriangle (Self.all,  p,  a, b, c,  Self.m_cachedBC'Access);

               Self.m_cachedP1 :=   Self.m_simplexPointsP (1) * Self.m_cachedBC.m_barycentricCoords (1)
                                  + Self.m_simplexPointsP (2) * Self.m_cachedBC.m_barycentricCoords (2)
                                  + Self.m_simplexPointsP (3) * Self.m_cachedBC.m_barycentricCoords (3);

               Self.m_cachedP2 :=   Self.m_simplexPointsQ (1) * Self.m_cachedBC.m_barycentricCoords (1)
                                  + Self.m_simplexPointsQ (2) * Self.m_cachedBC.m_barycentricCoords (2)
                                  + Self.m_simplexPointsQ (3) * Self.m_cachedBC.m_barycentricCoords (3);

               Self.m_cachedV  := Self.m_cachedP1 - Self.m_cachedP2;

               Self.reduceVertices (Self.m_cachedBC.m_usedVertices);

               Self.m_cachedValidClosest := isValid (Self.m_cachedBC);
            end;


         when 4 =>
            declare
               p : constant math.Vector_3 := (0.0, 0.0, 0.0);

               a : math.Vector_3 renames Self.m_simplexVectorW (1);
               b : math.Vector_3 renames Self.m_simplexVectorW (2);
               c : math.Vector_3 renames Self.m_simplexVectorW (3);
               d : math.Vector_3 renames Self.m_simplexVectorW (4);

               hasSeperation : constant Boolean := Self.closestPtPointTetrahedron (p,  a, b, c, d,  Self.m_cachedBC'Access);
               Break_early   : Boolean := False;
            begin
               if hasSeperation then
                  Self.m_cachedP1 :=   Self.m_simplexPointsP (1) * Self.m_cachedBC.m_barycentricCoords (1)
                                     + Self.m_simplexPointsP (2) * Self.m_cachedBC.m_barycentricCoords (2)
                                     + Self.m_simplexPointsP (3) * Self.m_cachedBC.m_barycentricCoords (3)
                                     + Self.m_simplexPointsP (4) * Self.m_cachedBC.m_barycentricCoords (4);

                  Self.m_cachedP2 :=   Self.m_simplexPointsQ (1) * Self.m_cachedBC.m_barycentricCoords (1)
                                     + Self.m_simplexPointsQ (2) * Self.m_cachedBC.m_barycentricCoords (2)
                                     + Self.m_simplexPointsQ (3) * Self.m_cachedBC.m_barycentricCoords (3)
                                     + Self.m_simplexPointsQ (4) * Self.m_cachedBC.m_barycentricCoords (4);

                  Self.m_cachedV  := Self.m_cachedP1 - Self.m_cachedP2;

                  Self.reduceVertices (Self.m_cachedBC.m_usedVertices);
               else
                  if Self.m_cachedBC.m_degenerate then
                     Self.m_cachedValidClosest := False;
                  else
                     Self.m_cachedValidClosest := True;
                     --  degenerate case == false, penetration = true + zero
                     Self.m_cachedV := (0.0, 0.0, 0.0);
                  end if;

                  Break_early := True;        -- break;
               end if;

               if not Break_early then
                  Self.m_cachedValidClosest := isValid (Self.m_cachedBC);
               end if;
               --  closest point origin from tetrahedron
            end;


         when others =>
            Self.m_cachedValidClosest := False;
         end case;

      end if;


      return Self.m_cachedValidClosest;
   end updateClosestVectorAndPoints;










   overriding procedure compute_points (Self : in out Item;   p1, p2 : out math.Vector_3)
   is
      unused : Boolean;
      pragma Unreferenced (unused);
   begin
      unused := updateClosestVectorAndPoints (Self'Access);

      p1 := Self.m_cachedP1;
      p2 := Self.m_cachedP2;
   end compute_points;






   overriding function  numVertices    (Self : in     Item) return Integer
   is
   begin
      return Self.m_numVertices;
   end numVertices;






   procedure removeVertex   (Self : in out Item;   Index : in Integer)
   is
   begin
      pragma Assert (Self.m_numVertices > 0);

      Self.m_simplexVectorW (index) := Self.m_simplexVectorW (Self.m_numVertices);
      Self.m_simplexPointsP (index) := Self.m_simplexPointsP (Self.m_numVertices);
      Self.m_simplexPointsQ (index) := Self.m_simplexPointsQ (Self.m_numVertices);

      Self.m_numVertices := Self.m_numVertices - 1;
   end removeVertex;






   procedure reduceVertices (Self : in out Item;   usedVerts : in btUsageBitfield)
   is
   begin
      if Self.numVertices >= 4  and then not usedVerts.usedVertexD then
         removeVertex (Self, 4);
      end if;

      if Self.numVertices >= 3  and then not usedVerts.usedVertexC then
         removeVertex (Self, 3);
      end if;

      if Self.numVertices >= 2  and then not usedVerts.usedVertexB then
         removeVertex (Self, 2);
      end if;

      if Self.numVertices >= 1  and then not usedVerts.usedVertexA then
         removeVertex (Self, 1);
      end if;
   end reduceVertices;







   function  closestPtPointTetrahedron (Self : in Item;   p           : in     math.Vector_3;
                                                          a, b, c, d  : in     math.Vector_3;
                                                          finalResult : access btSubSimplexClosestResult) return Boolean
   is
      use impact.d3.Vector;

      tempResult       : aliased btSubSimplexClosestResult;

      bestSqDist,
      sqDist           : math.Real;

      pointOutsideABC,
      pointOutsideACD,
      pointOutsideADB,
      pointOutsideBDC  : Boolean;

      q                : math.Vector_3;

      unused : Boolean;
      pragma Unreferenced (unused);
   begin

      --  Start out assuming point inside all halfspaces, so closest to itself
      --
      finalResult.m_closestPointOnSimplex := p;
      reset (finalResult.m_usedVertices);
      finalResult.m_usedVertices.usedVertexA := True;
      finalResult.m_usedVertices.usedVertexB := True;
      finalResult.m_usedVertices.usedVertexC := True;
      finalResult.m_usedVertices.usedVertexD := True;

      begin
         pointOutsideABC := Self.pointOutsideOfPlane (p,  a, b, c, d);
         pointOutsideACD := Self.pointOutsideOfPlane (p,  a, c, d, b);
         pointOutsideADB := Self.pointOutsideOfPlane (p,  a, d, b, c);
         pointOutsideBDC := Self.pointOutsideOfPlane (p,  b, d, c, a);
      exception
         when degenerate_Triangle =>
            finalResult.m_degenerate := True;
            return False;
      end;


      if         not pointOutsideABC
        and then not pointOutsideACD
        and then not pointOutsideADB
        and then not pointOutsideBDC
      then
         return False;
      end if;


      bestSqDist := math.Infinity;

      --  If point outside face abc then compute closest point on abc
      if pointOutsideABC then
         unused := closestPtPointTriangle (Self,  p,  a, b, c,  tempResult'Access);
         q := tempResult.m_closestPointOnSimplex;

         sqDist := dot (q - p,  q - p);

         --  Update best closest point if (squared) distance is less than current best
         if sqDist < bestSqDist then
            bestSqDist                             := sqDist;
            finalResult.m_closestPointOnSimplex    := q;
            --  convert result bitmask!
            reset (finalResult.m_usedVertices);
            finalResult.m_usedVertices.usedVertexA := tempResult.m_usedVertices.usedVertexA;
            finalResult.m_usedVertices.usedVertexB := tempResult.m_usedVertices.usedVertexB;
            finalResult.m_usedVertices.usedVertexC := tempResult.m_usedVertices.usedVertexC;
            setBarycentricCoordinates (finalResult.all,  tempResult.m_barycentricCoords (VERTA),
                                                         tempResult.m_barycentricCoords (VERTB),
                                                         tempResult.m_barycentricCoords (VERTC),
                                                         0.0);

         end if;
      end if;


      --  Repeat test for face acd
      if pointOutsideACD then
         unused := closestPtPointTriangle (Self,  p,  a, c, d,  tempResult'Access);
         q := tempResult.m_closestPointOnSimplex;
         --  convert result bitmask!

         sqDist := dot (q - p,  q - p);

         if sqDist < bestSqDist then
            bestSqDist                             := sqDist;
            finalResult.m_closestPointOnSimplex    := q;
            reset (finalResult.m_usedVertices);
            finalResult.m_usedVertices.usedVertexA := tempResult.m_usedVertices.usedVertexA;

            finalResult.m_usedVertices.usedVertexC := tempResult.m_usedVertices.usedVertexB;
            finalResult.m_usedVertices.usedVertexD := tempResult.m_usedVertices.usedVertexC;
            setBarycentricCoordinates (finalResult.all,  tempResult.m_barycentricCoords (VERTA),
                                                         0.0,
                                                         tempResult.m_barycentricCoords (VERTB),
                                                         tempResult.m_barycentricCoords (VERTC));

         end if;
      end if;
      --  Repeat test for face adb


      if pointOutsideADB then
         unused := closestPtPointTriangle (Self,  p,  a, d, b,  tempResult'Access);
         q := tempResult.m_closestPointOnSimplex;
         --  convert result bitmask!

         sqDist := dot (q - p,  q - p);

         if sqDist < bestSqDist then
            bestSqDist                             := sqDist;
            finalResult.m_closestPointOnSimplex    := q;
            reset (finalResult.m_usedVertices);
            finalResult.m_usedVertices.usedVertexA := tempResult.m_usedVertices.usedVertexA;
            finalResult.m_usedVertices.usedVertexB := tempResult.m_usedVertices.usedVertexC;

            finalResult.m_usedVertices.usedVertexD := tempResult.m_usedVertices.usedVertexB;
            setBarycentricCoordinates (finalResult.all,  tempResult.m_barycentricCoords (VERTA),
                                                         tempResult.m_barycentricCoords (VERTC),
                                                         0.0,
                                                         tempResult.m_barycentricCoords (VERTB));

         end if;
      end if;
      --  Repeat test for face bdc


      if pointOutsideBDC then
         unused := closestPtPointTriangle (Self,  p,  b, d, c,  tempResult'Access);
         q := tempResult.m_closestPointOnSimplex;
         --  convert result bitmask!
         sqDist := dot (q - p,  q - p);

         if sqDist < bestSqDist then
            bestSqDist := sqDist;
            finalResult.m_closestPointOnSimplex := q;
            reset (finalResult.m_usedVertices);
            --
            finalResult.m_usedVertices.usedVertexB := tempResult.m_usedVertices.usedVertexA;
            finalResult.m_usedVertices.usedVertexC := tempResult.m_usedVertices.usedVertexC;
            finalResult.m_usedVertices.usedVertexD := tempResult.m_usedVertices.usedVertexB;

            setBarycentricCoordinates (finalResult.all,  0.0,
                                                         tempResult.m_barycentricCoords (VERTA),
                                                         tempResult.m_barycentricCoords (VERTC),
                                                         tempResult.m_barycentricCoords (VERTB));

         end if;
      end if;

      --  help! we ended up full !

      if finalResult.m_usedVertices.usedVertexA
        and then finalResult.m_usedVertices.usedVertexB
        and then finalResult.m_usedVertices.usedVertexC
        and then finalResult.m_usedVertices.usedVertexD
      then
         return True;
      end if;

      return True;

   end closestPtPointTetrahedron;










   function  pointOutsideOfPlane (Self : in Item;   p           : in     math.Vector_3;
                                                    a, b, c, d  : in     math.Vector_3) return Boolean
   is
      pragma Unreferenced (Self);
      use impact.d3.Vector;
      normal : constant math.Vector_3 := cross (b - a, c - a);

      signp  : constant math.Real     := dot (p - a,  normal);  -- [AP AB AC]
      signd  : constant math.Real     := dot (d - a,  normal);  -- [AD AB AC]
   begin

      if CATCH_DEGENERATE_TETRAHEDRON then
--           if BT_USE_DOUBLE_PRECISION then
            if signd * signd < (1.0e-8 * 1.0e-8) then
               raise degenerate_Triangle;
            end if;
--           else
--              if signd * signd < (1.0e-4 * 1.0e-4) then
--                 -- printf("affine dependent/degenerate\n");//
--                 raise degenerate_triangle_Error;
--              end if;
--           end if;
      end if;

      --  Points on opposite sides if expression signs are opposite
      --
      return signp * signd < 0.0;
   end pointOutsideOfPlane;







   function  closestPtPointTriangle (Self : in Item;   p           : in     math.Vector_3;
                                     a, b, c     : in     math.Vector_3;
                                     Result      : access btSubSimplexClosestResult) return Boolean
   is
      pragma Unreferenced (Self);
      use impact.d3.Vector;
   begin
      reset (result.m_usedVertices);

      --  Check if P in vertex region outside A
      declare
         ab : constant math.Vector_3 := b - a;
         ac : constant math.Vector_3 := c - a;
         ap : constant math.Vector_3 := p - a;

         d1 : math.Real     := dot (ab, ap);
         d2 : math.Real     := dot (ac, ap);
      begin
         if d1 <= 0.0  and then  d2 <= 0.0 then
            result.m_closestPointOnSimplex    := a;
            result.m_usedVertices.usedVertexA := True;
            setBarycentricCoordinates (result.all,  1.0, 0.0, 0.0);

            return True;     -- a; // barycentric coordinates (1,0,0)
         end if;

         --  Check if P in vertex region outside B
         declare
            bp : constant math.Vector_3 := p - b;
            d3 : math.Real     := dot (ab, bp);
            d4 : math.Real     := dot (ac, bp);
         begin
            if d3 >= 0.0  and then  d4 <= d3 then
               result.m_closestPointOnSimplex    := b;
               result.m_usedVertices.usedVertexB := True;
               setBarycentricCoordinates (result.all,  0.0, 1.0, 0.0);

               return True; -- b; // barycentric coordinates (0,1,0)
            end if;

            --  Check if P in edge region of AB, if so return projection of P onto AB
            declare
               vc : math.Real := d1 * d4 - d3*d2;
               v  : math.Real;
            begin
               if vc <= 0.0  and then  d1 >= 0.0  and then  d3 <= 0.0 then
                  v                                 := d1 / (d1 - d3);
                  result.m_closestPointOnSimplex    := a + v * ab;
                  result.m_usedVertices.usedVertexA := True;
                  result.m_usedVertices.usedVertexB := True;
                  setBarycentricCoordinates (result.all,  1.0-v,  v,  0.0);

                  return True;
                  --  return a + v * ab; // barycentric coordinates (1-v,v,0)
               end if;

               --  Check if P in vertex region outside C
               declare
                  cp : constant math.Vector_3 := p - c;
                  d5 : math.Real     := dot (ab, cp);
                  d6 : math.Real     := dot (ac, cp);
               begin
                  if d6 >= 0.0  and then  d5 <= d6 then
                     result.m_closestPointOnSimplex    := c;
                     result.m_usedVertices.usedVertexC := True;
                     setBarycentricCoordinates (result.all,  0.0,  0.0,  1.0);
                     return True;   -- //c; // barycentric coordinates (0,0,1)
                  end if;

                  --  Check if P in edge region of AC, if so return projection of P onto AC
                  declare
                     vb : math.Real := d5 * d2 - d1*d6;
                     w  : math.Real;
                  begin
                     if vb <= 0.0 and then  d2 >= 0.0  and then  d6 <= 0.0 then
                        w                                 := d2 / (d2 - d6);
                        result.m_closestPointOnSimplex    := a + w * ac;
                        result.m_usedVertices.usedVertexA := True;
                        result.m_usedVertices.usedVertexC := True;
                        setBarycentricCoordinates (result.all,  1.0-w,  0.0,  w);

                        return True;
                        --  return a + w * ac; // barycentric coordinates (1-w,0,w)
                     end if;

                     --  Check if P in edge region of BC, if so return projection of P onto BC
                     declare
                        va : math.Real := d3 * d6 - d5*d4;
                        w  : math.Real;
                     begin
                        if va <= 0.0  and then  (d4 - d3) >= 0.0  and then  (d5 - d6) >= 0.0 then
                           w                                 := (d4 - d3) / ((d4 - d3) + (d5 - d6));
                           result.m_closestPointOnSimplex    := b + w * (c - b);
                           result.m_usedVertices.usedVertexB := True;
                           result.m_usedVertices.usedVertexC := True;
                           setBarycentricCoordinates (result.all,  0.0,  1.0-w,  w);

                           return True;
                           --       // return b + w * (c - b); // barycentric coordinates (0,1-w,w)
                        end if;

                        --  P inside face region. Compute Q through its barycentric coordinates (u,v,w)
                        declare
                           denom : constant math.Real := 1.0 / (va + vb + vc);
                           v     : constant math.Real := vb * denom;
                           w     : constant math.Real := vc * denom;
                        begin
                           result.m_closestPointOnSimplex    := a + ab * v + ac * w;
                           result.m_usedVertices.usedVertexA := True;
                           result.m_usedVertices.usedVertexB := True;
                           result.m_usedVertices.usedVertexC := True;
                           setBarycentricCoordinates (result.all,  1.0-v-w,  v,  w);

                           return True;
                           --        return a + ab * v + ac * w; // = u*a + v*b + w*c, u = va * denom = impact.d3.Scalar(1.0) - v - w
                        end;
                     end;
                  end;
               end;
            end;
         end;
      end;

   end closestPtPointTriangle;


end impact.d3.collision.simplex_Solver.voronoi;








