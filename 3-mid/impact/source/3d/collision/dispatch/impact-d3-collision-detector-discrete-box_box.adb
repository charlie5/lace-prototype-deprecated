with interfaces.C.Pointers;
with impact.d3.Scalar;



package body impact.d3.collision.Detector.discrete.box_box
--
--   Given two boxes (p1,R1,side1) and (p2,R2,side2), collide them together and
--   generate contact points. this returns 0 if there is no contact otherwise
--   it returns the number of contacts generated.
--
--   `normal' returns the contact normal.
--
--   `depth' returns the maximum penetration depth along that normal.
--
--   `return_code' returns a number indicating the type of contact that was detected:
--          1,2,3 = box 2 intersects with a face of box 1
--          4,5,6 = box 1 intersects with a face of box 2
--          7..15 = edge-edge contact
--
--   `maxc' is the maximum number of contacts allowed to be generated, i.e.
--   the size of the `contact' array.

--   `contact' and `skip' are the contact array information provided to the
--   collision functions. this function only fills in the position and depth  fields.
--
is
   package iC renames interfaces.C;

   dInfinity : constant math.Real := math.Real'Last;



--  typedef impact.d3.Scalar dMatrix3[4*3];




   type Vector is array (Integer range 1 .. 3) of aliased math.Real;



   function to_Math (Self : in Vector) return math.Vector_3
   is
   begin
      return (Self (1), Self (2), Self (3));
   end to_Math;





   function "-" (L, R : in Vector) return Vector
   is
   begin
      return (L (1) - R (1),
              L (2) - R (2),
              L (3) - R (3));
   end;




   function "*" (L, R : in Vector) return math.Real
   is
   begin
      return L (1) * R (1)
         +   L (2) * R (2)
         +   L (3) * R (3);
   end;


   function "-" (L : in Vector) return Vector
   is
   begin
      return (-L (1),
              -L (2),
              -L (3));
   end;





   type Matrix is array (Integer range 1 .. 3, Integer range 1 .. 3) of aliased math.Real;


   function Row (Self : in Matrix;   row_Id : in Index) return Vector
   is
   begin
      return (Self (row_Id, 1),  Self (row_Id, 2),  Self (row_Id, 3));
   end Row;



   function Col (Self : in Matrix;   col_Id : in Index) return Vector
   is
   begin
      return (Self (1, col_Id),  Self (2, col_Id),  Self (3, col_Id));
   end Col;









   function to_box_box_Detector (box1, box2 : in impact.d3.Shape.convex.internal.polyhedral.box.view) return Item
   is
      Self : Item;
   begin
      Self.m_box1 := box1;
      Self.m_box2 := box2;

      return Self;
   end to_box_box_Detector;





   function dDOTpq (a, b : in math.Matrix_4x4;
                    p, q : in Integer    ) return math.Real
   is

      a_Vec : math.Vector_16 := to_Vector_16 (a);
      b_Vec : math.Vector_16 := to_Vector_16 (b);
   begin
      return a_Vec (1)   * b_Vec (1)
        +    a_Vec (p)   * b_Vec (q)
        +    a_Vec (2*p) * b_Vec (2*q);
   end dDOTpq;

   --  #define dDOTpq (a,b,p,q) (a[0] * b[0]  +  a[p] * b[q]  +  a[2*p] * b[2*q])





   function dDOT (a, b : in Vector) return math.Real
   is
      use math.Vectors;
   begin
      return a * b;   -- dot product
   end dDOT;



   function dDOT44 (a : in Matrix;   a_col_Id : in Integer;
                    b : in Matrix;   b_col_Id : in Integer) return math.Real
   is
   begin
      return Col (a, a_col_Id)  *  Col (b, b_col_Id);    -- dDOTpq (a, b,  4, 4);
   end dDOT44;



   function dDOT41 (a      : in Matrix;
                    a_col_Id : in Integer;
                    b      : in Vector) return math.Real
   is
   begin
      return Col (a, a_col_Id)  *  b;      -- dDOTpq (a, b,  4, 1);
   end dDOT41;



   function dDOT14 (a        : in Vector;
                    b        : in Matrix;
                    b_col_Id : in Integer) return math.Real
   is
   begin
      return a * Col (b, b_col_Id);        -- dDOTpq (a, b,  1, 4);
   end dDOT14;


--  static impact.d3.Scalar dDOT   (const impact.d3.Scalar *a, const impact.d3.Scalar *b) { return dDOTpq(a,b,1,1); }
--  static impact.d3.Scalar dDOT44 (const impact.d3.Scalar *a, const impact.d3.Scalar *b) { return dDOTpq(a,b,4,4); }
--  static impact.d3.Scalar dDOT41 (const impact.d3.Scalar *a, const impact.d3.Scalar *b) { return dDOTpq(a,b,4,1); }
--  static impact.d3.Scalar dDOT14 (const impact.d3.Scalar *a, const impact.d3.Scalar *b) { return dDOTpq(a,b,1,4); }






--     procedure dMULTIPLYOP1_331 (A :    out math.Vector_3;
--                                 B : in     math.Matrix_3x3;
--                                 C : in     math.Vector_3)
--     is
--     begin
--        A (1) := dDOT41 (B, C);
--        A (2) := dDOT41 (B, C);
--        A (3) := dDOT41 (B, C);
--     end;



--  #define dMULTIPLYOP1_331(A,op,B,C) \
--  {\
--    (A)[0] op dDOT41((B),(C)); \
--    (A)[1] op dDOT41((B+1),(C)); \
--    (A)[2] op dDOT41((B+2),(C)); \
--  }

--
--  #define dMULTIPLYOP0_331(A,op,B,C) \
--  { \
--    (A)[0] op dDOT((B),(C)); \
--    (A)[1] op dDOT((B+4),(C)); \
--    (A)[2] op dDOT((B+8),(C)); \
--  }

--
--  #define dMULTIPLY1_331(A,B,C) dMULTIPLYOP1_331(A,=,B,C)
--  #define dMULTIPLY0_331(A,B,C) dMULTIPLYOP0_331(A,=,B,C)


   procedure dMULTIPLY1_331 (A :    out Vector;
                             B : in     Matrix;
                             C : in     Vector)
   is
   begin
      A (1) := dDOT41 (B, 1, C);
      A (2) := dDOT41 (B, 2, C);
      A (3) := dDOT41 (B, 3, C);
   end dMULTIPLY1_331;



   procedure dMULTIPLY0_331 (A :    out Vector;
                             B : in     Matrix;
                             C : in     Vector)
   is
   begin
      A (1) := dDOT (Row (B, 1),  C);
      A (2) := dDOT (Row (B, 2),  C);
      A (3) := dDOT (Row (B, 3),  C);
   end dMULTIPLY0_331;






   procedure dLineClosestApproach (pa, ua,
                                   pb, ub       : in     Vector;
                                   alpha, beta  :    out math.Real)
   is
      p    : constant Vector := pb - pa;

      uaub : constant math.Real :=  dDOT (ua, ub);
      q1   : constant math.Real :=  dDOT (ua,  p);
      q2   : constant math.Real := -dDOT (ub,  p);
      d    : math.Real := 1.0 - uaub*uaub;

   begin
      if d <= 0.0001 then   -- This needs to be made more robust.
         alpha := 0.0;
         beta  := 0.0;
      else
         d     := 1.0 / d;
         alpha := (q1 + uaub*q2)  *  d;
         beta  := (q2 + uaub*q1)  *  d;
      end if;
   end dLineClosestApproach;





   type c_Vector is array (Integer range <>) of aliased math.Real;

   subtype c_Vector_2  is c_Vector (0 ..  1);
   subtype c_Vector_8  is c_Vector (0 ..  7);
   subtype c_Vector_16 is c_Vector (0 ..  15);

--     subtype c_Vector_8  is array (Integer range 0 ..  7) of aliased math.Real;
--     subtype c_Vector_16 is array (Integer range 0 .. 15) of aliased math.Real;





   --   Find all the intersection points between the 2D rectangle with vertices
   --   at (+/-h[0],+/-h[1]) and the 2D quadrilateral with vertices (p[0],p[1]),
   --   (p[2],p[3]),(p[4],p[5]),(p[6],p[7]).
   --
   --   The intersection points are returned as x,y pairs in the 'ret' array.
   --   the number of intersection points is returned by the function (this will
   --   be in the range 0 to 8).
   --
   function intersectRectQuad2 (h   : in     c_Vector_2;
                                p   : access c_Vector_8;
                                ret : access c_Vector_16) return Integer
   is
      use type interfaces.Unsigned_32;

      nq     : interfaces.Unsigned_32 := 4;
      nr     : interfaces.Unsigned_32 := 0;

      type Real_array is array (Natural range <>) of aliased math.Real;

      package  real_Pointers is new interfaces.C.Pointers (Natural, math.Real, real_array, math.Real'First);
      subtype  Real_view     is real_Pointers.Pointer;
      use type Real_view;

      use real_Pointers, interfaces.C;


      buffer : c_Vector_16;



--        q      : access math.Real := p;        -- q (and r) contain nq (and nr) coordinate points for the
--        r      : access math.Real := ret;      -- current (and chopped) polygons.
      q      : Real_view := p   (0)'Access;      -- q (and r) contain nq (and nr) coordinate points for the
      r      : Real_view := ret (0)'Access;      -- current (and chopped) polygons.

--        pq, pr,
--        nextq  : access math.Real;

--        pq, pr,
--        nextq  : Integer;

      pq, pr,
      nextq  : Real_view;

      sign   : math.Real;
      i      : Integer;

   begin
      for dir in 0 .. 1    -- direction notation: xy[0] = x axis, xy[1] = y axis
      loop

         sign := -1.0;
         loop
            --  chop q along the line xy[dir] = sign*h[dir]
            --
            pq := q;
            pr := r;
            nr := 0;

            --              pq := 1;
            --              pr := 1;
            --              nr := 0;

            i := Integer (nq);
            while i > 0
            loop
               --  go through all points in q and all lines between adjacent points
               --
               declare
                  pq_array : Real_array renames Value (pq, 8);
               begin

                  if sign * pq_array (dir)  <   h (dir) then     -- this point is inside the chopping line
                     copy_Array (pq + 0,  pr + 0,  1);
                     copy_Array (pq + 1,  pr + 1,  1);

                     pr    := pr + 2;
                     nr    := nr + 1;

                     if (nr and 8) /= 0 then
                        q := r;
                        goto done;
                     end if;
                  end if;

                  if i > 1 then   nextq := pq + 2;
                  else   nextq := q;
                  end if;

                  declare
                     nextq_array : Real_array renames Value (nextq, 8);
                  begin
                     if    (sign * pq_array    (dir)  <  h (dir))
                       xor (sign * nextq_array (dir)  <  h (dir))
                     then   -- this line crosses the chopping line
                        Real_view (pr + ptrdiff_t (1-dir)).all :=   pq_array (1-dir) + (nextq_array (1-dir) - pq_array (1-dir))
                                                                  / (nextq_array (dir) - pq_array (dir)) * (sign * h (dir) - pq_array (dir));
                        Real_view (pr + ptrdiff_t (dir)).all := sign * h (dir);

                        pr := pr + 2;
                        nr := nr + 1;

                        if (nr and 8) /= 0 then
                           q := r;
                           goto done;
                        end if;

                     end if;

                     pq := pq + 2;
                     i  := i - 1;
                  end;
               end;
            end loop;

            q := r;

            if q = ret (0)'Access then   r := buffer (0)'Access;
            else   r := ret    (0)'Access;
            end if;

            nq := nr;

            if sign = -1.0 then   sign := 1.0;
            else   exit;
            end if;
         end loop;

      end loop;



      <<done>>

      if q /= ret (0)'Access then
         copy_Array (q,  ret (0)'Access,   ptrdiff_t (nr * 2));
      end if;

      return Integer (nr);
   end intersectRectQuad2;










   --  Given n points in the plane (array p, of size 2*n), generate m points that
   --  best represent the whole set. The definition of 'best' here is not
   --  predetermined - the idea is to select points that give good box-box
   --  collision detection behavior. The chosen point indexes are returned in the
   --  array iret (of size m).
   --
   --  'i0' is always the first entry in the array.
   --
   --  n must be in the range [1..8]. m must be in the range [1..n]. i0 must be in the range [0..n-1].

   procedure cullPoints2 (n    : in     Integer;
                          p    : access c_Vector_16;
                          m    : in     Integer;
                          i0   : in     Integer;
                          iret :    out math.Integers)
   is
      a,
      cx, cy,
      q     : math.Real;

   begin
      --  Compute the centroid of the polygon in cx, cy.
      --
      if n = 1 then
         cx := p (0);
         cy := p (1);

      elsif n = 2 then
         cx := 0.5 * (p (0) + p (2));
         cy := 0.5 * (p (1) + p (3));

      else
         a  := 0.0;
         cx := 0.0;
         cy := 0.0;

         for i in 0 .. (n - 1) - 1
         loop
            q  :=    p (i * 2) * p (i * 2 + 3)
                  -  p (i * 2 + 2) * p (i * 2 + 1);
            a  := a + q;

            cx := cx + q * (p (i * 2) + p (i * 2 + 2));
            cy := cy + q * (p (i * 2 + 1) + p (i * 2 + 3));
         end loop;

         q := p (n * 2 - 2) * p (1)  -  p (0) * p (n * 2 - 1);

         if abs (a + q) > impact.d3.Scalar.SIMD_EPSILON then
            a := 1.0  /  (3.0 * (a + q));
         else
            a := math.Real'Last;    -- BT_LARGE_FLOAT;
         end if;

         cx := a  *  (cx  +  q * (p (n * 2 - 2) + p (0)));
         cy := a  *  (cy  +  q * (p (n * 2 - 1) + p (1)));
      end if;

      --  Compute the angle of each point with respect to the centroid.
      --
      declare
         use math.Functions;

         big_A : c_Vector_8;
         avail : math.Integers (1 .. 8);

         diff,
         maxdiff : math.Real;

         iret_Id : Integer := 1;
      begin

         for i in 0 .. n - 1
         loop
            big_A (i) := arcTan (p (i * 2 + 1) - cy,
                                 p (i * 2) - cx);
         end loop;

         --  Search for points that have angles closest to A[i0] + i*(2*pi/m).
         --
         for i in 1 .. n
         loop
            avail (i) := 1;
         end loop;

         avail (i0 + 1)    := 0;
         iret  (iret_Id) := i0;
         iret_Id         := iret_Id + 1;

         for j in 2 .. m
         loop
            a := math.Real (j - 1) * (2.0*Pi/math.Real (m))  +  big_A (i0);

            if a > Pi then
               a := a - 2.0*Pi;
            end if;

            maxdiff := 1.0e9;

            iret (iret_Id) := i0;                        -- iret is not allowed to keep this value, but it sometimes does, when diff=#QNAN0

            for i in 1 .. n
            loop
               if avail (i) /= 0 then
                  diff := abs (big_A (i - 1) - a);

                  if diff > Pi then
                     diff := 2.0*Pi - diff;
                  end if;

                  if diff < maxdiff then
                     maxdiff        := diff;
                     iret (iret_Id) := i - 1;
                  end if;
               end if;
            end loop;

            pragma Assert (iret (iret_Id) /= i0);        -- Ensure 'iret' was set.

            avail (iret (iret_Id) + 1) := 0;
            iret_Id                    := iret_Id + 1;
         end loop;
      end;

   end cullPoints2;









   function dBoxBox2 (p1          : in     Vector;
                      the_R1      : in     math.Matrix_3x3;
                      side1       : in     math.Vector_3;

                      p2          : in     Vector;
                      the_R2      : in     math.Matrix_3x3;
                      side2       : in     math.Vector_3;

                      normal      : access Vector;
                      depth       : access math.Real;
                      return_code : access Integer;
                      the_maxc    : in     Integer;
                      --                        dContactGeom*   /*contact*/,
                      --                        int             /*skip*/,
                      output      : access impact.d3.collision.Detector.discrete.Result'Class) return Integer
   is
      R1 : aliased Matrix := (1 => (the_R1 (1, 1),  the_R1 (1, 2),  the_R1 (1, 3)),
                              2 => (the_R1 (2, 1),  the_R1 (2, 2),  the_R1 (2, 3)),
                              3 => (the_R1 (3, 1),  the_R1 (3, 2),  the_R1 (3, 3)));

      R2 : aliased Matrix := (1 => (the_R2 (1, 1),  the_R2 (1, 2),  the_R2 (1, 3)),
                              2 => (the_R2 (2, 1),  the_R2 (2, 2),  the_R2 (2, 3)),
                              3 => (the_R2 (3, 1),  the_R2 (3, 2),  the_R2 (3, 3)));

      maxc : Integer := the_maxc;

      fudge_factor  : constant math.Real := 1.05;
      p, pp,
      normalC       : Vector        := (0.0, 0.0, 0.0);

      normalR       : Vector        := (0.0, 0.0, 0.0);
      A, B          : aliased Vector;

      R11, R12, R13,
      R21, R22, R23,
      R31, R32, R33 : math.Real;

      Q11, Q12, Q13,
      Q21, Q22, Q23,
      Q31, Q32, Q33 : math.Real;

      s, s2, l      : math.Real;

--        i,j,
      code          : Integer;

      invert_normal : Boolean;

   begin
      --  Get vector from centers of box 1 to box 2, relative to box 1.
      --
      p := p2 - p1;
      dMULTIPLY1_331 (pp, R1, p);                -- get pp = p relative to body 1

      --  Get side lengths / 2.
      --
      A (1) := side1 (1) * 0.5;
      A (2) := side1 (2) * 0.5;
      A (3) := side1 (3) * 0.5;
      B (1) := side2 (1) * 0.5;
      B (2) := side2 (2) * 0.5;
      B (3) := side2 (3) * 0.5;

      --  Rij is R1'*R2, i.e. the relative rotation between R1 and R2.
      --
      R11 := dDOT44 (R1, 1,  R2, 1);   R12 := dDOT44 (R1, 1,  R2, 2);   R13 := dDOT44 (R1, 1,  R2, 3);
      R21 := dDOT44 (R1, 2,  R2, 1);   R22 := dDOT44 (R1, 2,  R2, 2);   R23 := dDOT44 (R1, 2,  R2, 3);
      R31 := dDOT44 (R1, 3,  R2, 1);   R32 := dDOT44 (R1, 3,  R2, 2);   R33 := dDOT44 (R1, 3,  R2, 3);

      Q11 := abs (R11);   Q12 := abs (R12);   Q13 := abs (R13);
      Q21 := abs (R21);   Q22 := abs (R22);   Q23 := abs (R23);
      Q31 := abs (R31);   Q32 := abs (R32);   Q33 := abs (R33);







      --  For all 15 possible separating axes:
      --   * see if the axis separates the boxes. if so, return 0.
      --   * find the depth of the penetration along the separating axis (s2)
      --   * if this is the largest depth so far, record it.
      --
      --  The normal vector will be set to the separating axis with the smallest depth.
      --
      --  Note: normalR is set to point to a column of R1 or R2 if that is
      --       the smallest depth normal so far. otherwise normalR is 0 and normalC is
      --       set to a vector relative to body 1. invert_normal is 1 if the sign of
      --       the normal should be flipped.
      --
      declare

         return_Required : Boolean := False;


         procedure TST (expr1, expr2 : in     math.Real;
                        norm         : in     Vector;
                        cc           : in     Integer)
         is
         begin
            s2 := abs (expr1) - expr2;
            if s2 > 0.0 then
               return_Required := True;
            end if;

            if s2 > s then
               s             := s2;
               normalR       := norm;
               invert_normal := (expr1 < 0.0);
               code          := (cc);
            end if;
         end TST;


         --  #define TST(expr1,expr2,norm,cc) \
         --    s2 = btFabs(expr1) - (expr2); \
         --    if (s2 > 0) return 0; \
         --    if (s2 > s) { \
         --      s = s2; \
         --      normalR = norm; \
         --      invert_normal = ((expr1) < 0); \
         --      code = (cc); \
         --    }

      begin
         s             := -dInfinity;
         invert_normal := False;
         code          := 0;

         --  separating axis = u1,u2,u3
         TST (pp (1),  (A (1) + B (1)*Q11 + B (2)*Q12 + B (3)*Q13),  Col (R1, 1),  1);   if return_Required then   return 0;   end if;
         TST (pp (2),  (A (2) + B (1)*Q21 + B (2)*Q22 + B (3)*Q23),  Col (R1, 2),  2);   if return_Required then   return 0;   end if;
         TST (pp (3),  (A (3) + B (1)*Q31 + B (2)*Q32 + B (3)*Q33),  Col (R1, 3),  3);   if return_Required then   return 0;   end if;

         --  separating axis = v1,v2,v3
         TST (dDOT41 (R2, 1, p), (A (1) * Q11 + A (2)*Q21 + A (3)*Q31 + B (1)),  Col (R2, 1),  4);   if return_Required then   return 0;   end if;
         TST (dDOT41 (R2, 2, p), (A (1) * Q12 + A (2)*Q22 + A (3)*Q32 + B (2)),  Col (R2, 2),  5);   if return_Required then   return 0;   end if;
         TST (dDOT41 (R2, 3, p), (A (1) * Q13 + A (2)*Q23 + A (3)*Q33 + B (3)),  Col (R2, 3),  6);   if return_Required then   return 0;   end if;

         --  note: cross product axes need to be scaled when s is computed.
         --  normal (n1,n2,n3) is relative to box 1.

      end;


      declare
         return_Required : Boolean := False;


         procedure TST (expr1, expr2 : in math.Real;
                        n1, n2, n3   : in math.Real;
                        cc           : in Integer)
         is
            use impact.d3.Scalar, math.Functions;
         begin
            s2 := abs (expr1) - expr2;

            if s2 > impact.d3.Scalar.SIMD_EPSILON then
               return_Required := True;
            end if;

            l := sqRt (n1 * n1 + n2*n2 + n3*n3);

            if l > SIMD_EPSILON then
               s2 := s2 / l;

               if s2 * fudge_factor  >  s then
                  s             := s2;
                  normalR       := (0.0, 0.0, 0.0);
                  normalC (1)   := (n1)/l;
                  normalC (2)   := (n2)/l;
                  normalC (3)   := (n3)/l;
                  invert_normal := (expr1 < 0.0);
                  code          := (cc);
               end if;
            end if;

         end TST;

         --  #define TST(expr1,expr2,n1,n2,n3,cc) \
         --    s2 = btFabs(expr1) - (expr2); \
         --    if (s2 > SIMD_EPSILON) return 0; \
         --    l = btSqrt((n1)*(n1) + (n2)*(n2) + (n3)*(n3)); \
         --    if (l > SIMD_EPSILON) { \
         --      s2 /= l; \
         --      if (s2*fudge_factor > s) { \
         --        s = s2; \
         --        normalR = 0; \
         --        normalC[0] = (n1)/l; normalC[1] = (n2)/l; normalC[2] = (n3)/l; \
         --        invert_normal = ((expr1) < 0); \
         --        code = (cc); \
         --      } \
         --    }


         fudge2 : constant math.Real := 1.0e-5;
      begin

         Q11 := Q11 + fudge2;
         Q12 := Q12 + fudge2;
         Q13 := Q13 + fudge2;

         Q21 := Q21 + fudge2;
         Q22 := Q22 + fudge2;
         Q23 := Q23 + fudge2;

         Q31 := Q31 + fudge2;
         Q32 := Q32 + fudge2;
         Q33 := Q33 + fudge2;

         --  separating axis = u1 x (v1,v2,v3)
         TST (pp (3) * R21 - pp (2)*R31,  (A (2) * Q31 + A (3)*Q21 + B (2)*Q13 + B (3)*Q12),  0.0,  -R31,  R21,   7);   if return_Required then   return 0;   end if;
         TST (pp (3) * R22 - pp (2)*R32,  (A (2) * Q32 + A (3)*Q22 + B (1)*Q13 + B (3)*Q11),  0.0,  -R32,  R22,   8);   if return_Required then   return 0;   end if;
         TST (pp (3) * R23 - pp (2)*R33,  (A (2) * Q33 + A (3)*Q23 + B (1)*Q12 + B (2)*Q11),  0.0,  -R33,  R23,   9);   if return_Required then   return 0;   end if;

         --  separating axis = u2 x (v1,v2,v3)
         TST (pp (1) * R31 - pp (3)*R11,  (A (1) * Q31 + A (3)*Q11 + B (2)*Q23 + B (3)*Q22),  R31,  0.0,  -R11,  10);   if return_Required then   return 0;   end if;
         TST (pp (1) * R32 - pp (3)*R12,  (A (1) * Q32 + A (3)*Q12 + B (1)*Q23 + B (3)*Q21),  R32,  0.0,  -R12,  11);   if return_Required then   return 0;   end if;
         TST (pp (1) * R33 - pp (3)*R13,  (A (1) * Q33 + A (3)*Q13 + B (1)*Q22 + B (2)*Q21),  R33,  0.0,  -R13,  12);   if return_Required then   return 0;   end if;

         --  separating axis = u3 x (v1,v2,v3)
         TST (pp (2) * R11 - pp (1)*R21,  (A (1) * Q21 + A (2)*Q11 + B (2)*Q33 + B (3)*Q32),  -R21,  R11,  0.0,  13);   if return_Required then   return 0;   end if;
         TST (pp (2) * R12 - pp (1)*R22,  (A (1) * Q22 + A (2)*Q12 + B (1)*Q33 + B (3)*Q31),  -R22,  R12,  0.0,  14);   if return_Required then   return 0;   end if;
         TST (pp (2) * R13 - pp (1)*R23,  (A (1) * Q23 + A (2)*Q13 + B (1)*Q32 + B (2)*Q31),  -R23,  R13,  0.0,  15);   if return_Required then   return 0;   end if;

      end;


      if code = 0 then
         return 0;
      end if;



      --    If we get to this point, the boxes interpenetrate. Compute the normal in global coordinates.
      --
      if normalR /= (0.0, 0.0, 0.0) then
         normal.all := normalR;         --      normal[0] = normalR[0];
                                        --      normal[1] = normalR[4];
                                        --      normal[2] = normalR[8];
      else
         dMULTIPLY0_331 (normal.all, R1, normalC);
      end if;


      if invert_normal then
         normal.all := -normal.all;   -- normal[0] := -normal[0];
                                      -- normal[1] := -normal[1];
                                      -- normal[2] := -normal[2];
      end if;


      depth.all := -s;



      --  Compute contact point(s).
      --

      if code > 6 then   -- an edge from box 1 touches an edge from box 2.
         declare
            pa, pb : Vector;
            sign   : math.Real;
         begin
            --  find a point pa on the intersecting edge of box 1
            --
            for i in 1 .. 3
            loop
               pa (i) := p1 (i);
            end loop;

            for j in 1 .. 3
            loop
               if dDOT14 (normal.all,  R1, j)  >  0.0 then
                  sign :=  1.0;
               else
                  sign := -1.0;
               end if;

               for i in 1 .. 3
               loop
                  pa (i) := pa (i)  +  sign * A (j) * R1 (i, j);   -- pa[i] += sign * A[j] * R1[i*4+j];
               end loop;
            end loop;


            --  find a point pb on the intersecting edge of box 2
            --
            for i in 1 .. 3
            loop
               pb (i) := p2 (i);
            end loop;

            for j in 1 .. 3
            loop
               if dDOT14 (normal.all, R2, j)  >  0.0 then
                  sign := -1.0;
               else
                  sign :=  1.0;
               end if;

               for i in 1 .. 3
               loop
                  pb (i) := pb (i)  +  sign * B (j) * R2 (i, j);
               end loop;
            end loop;



            declare
               alpha, beta : aliased math.Real;
               ua,    ub   : Vector;

                -- contact[0].pos[i] = impact.d3.Scalar(0.5)*(pa[i]+pb[i]);
                -- contact[0].depth = *depth;
               pointInWorld : math.Vector_3;

               USE_CENTER_POINT : constant Boolean := True;

            begin
               for i in 1 .. 3
               loop
                  ua (i) := R1 ((code - 7)/3 + 1,
                                i);   -- ua[i] = R1[((code)-7)/3 + i*4];
               end loop;

               for i in 1 .. 3
               loop
                  ub (i) := R2 ((code - 7) mod 3 + 1,
                                i);
               end loop;


               dLineClosestApproach (pa, ua, pb, ub, alpha, beta);

               for i in 1 .. 3
               loop
                  pa (i) := pa (i)  +  ua (i) * alpha;   -- pa[i] += ua[i]*alpha;
               end loop;

               for i in 1 .. 3
               loop
                  pb (i) := pb (i)  +  ub (i) * beta;
               end loop;


               if USE_CENTER_POINT then
                  for i in 1 .. 3
                  loop
                     pointInWorld (i) := (pa (i) + pb (i)) * 0.5;
                  end loop;

                  output.addContactPoint (-math.Vector_3 (normal.all), pointInWorld,        -depth.all);
               else
                  output.addContactPoint (-math.Vector_3 (normal.all), math.Vector_3 (pb),  -depth.all);
               end if;

               return_code.all := code;
            end;
         end;

         return 1;
      end if;


      --    Okay, we have a face-something intersection (because the separating
      --    axis is perpendicular to a face).
      --
      --    Define face 'a' to be the reference
      --    face (i.e. the normal vector is perpendicular to this) and face 'b' to be
      --    the incident face (the closest face of the other box).
      --
      declare
         Ra, Rb : access Matrix;

         pa, pb :        Vector;
         Sa, Sb : access Vector;

         normal2,
         nr,                               -- normal vector of reference face dotted with axes of incident box.
         anr    : Vector;                  -- absolute values of nr.

         lanr,
         a1,
         a2     : Integer;

      begin
         if code <= 3 then
            Ra := R1'Access;
            Rb := R2'Access;

            pa := p1;
            pb := p2;

            Sa := A'Access;
            Sb := B'Access;
         else
            Ra := R2'Access;
            Rb := R1'Access;

            pa := p2;
            pb := p1;

            Sa := B'Access;
            Sb := A'Access;
         end if;


         if code <= 3 then
            normal2 (1) := normal (1);
            normal2 (2) := normal (2);
            normal2 (3) := normal (3);

         else
            normal2 (1) := -normal (1);
            normal2 (2) := -normal (2);
            normal2 (3) := -normal (3);
         end if;

         dMULTIPLY1_331 (nr, Rb.all, normal2);

         anr (1) := abs (nr (1));
         anr (2) := abs (nr (2));
         anr (3) := abs (nr (3));


         --  find the largest compontent of anr: this corresponds to the normal
         --  for the indident face. the other axis numbers of the indicent face
         --  are stored in a1,a2.
         --
         if anr (2) > anr (1) then
            if anr (2) > anr (3) then
               a1   := 1;
               lanr := 2;
               a2   := 3;
            else
               a1   := 1;
               a2   := 2;
               lanr := 3;
            end if;

         else
            if anr (1) > anr (3) then
               lanr := 1;
               a1   := 2;
               a2   := 3;
            else
               a1   := 1;
               a2   := 2;
               lanr := 3;
            end if;
         end if;



         declare
            center : Vector;
            codeN,
            code1,
            code2  : Integer;
         begin
            --  Compute center point of incident face, in reference-face coordinates.
            --
            if nr (lanr) < 0.0 then
               for i in 1 .. 3
               loop
                  center (i) := pb (i) - pa (i) + Sb (lanr) * Rb (i, lanr);        -- center[i] = pb[i] - pa[i] + Sb[lanr] * Rb[i*4+lanr];
               end loop;

            else
               for i in 1 .. 3
               loop
                  center (i) := pb (i) - pa (i) - Sb (lanr) * Rb (i, lanr);        -- center[i] = pb[i] - pa[i] - Sb[lanr] * Rb[i*4+lanr];
               end loop;
            end if;


            --  Find the normal and non-normal axis numbers of the reference box.
            --
            if code <= 3 then
               codeN := code - 1;
            else
               codeN := code - 4;
            end if;


            if    codeN = 0 then   code1 := 2;
               code2 := 3;

            elsif codeN = 1 then   code1 := 1;
               code2 := 3;

            else                   code1 := 1;
               code2 := 2;
            end if;



            declare
               quad     : aliased c_Vector_8;                     -- 2D coordinate of incident face (x,y pairs)

               c1,  c2,
               m11, m12,
               m21, m22 : math.Real;
            begin
               --  Find the four corners of the incident face, in reference-face coordinates.
               --
               c1 := dDOT14 (center,  Ra.all, code1);
               c2 := dDOT14 (center,  Ra.all, code2);

               --  optimize this? - we have already computed this data above, but it is not
               --  stored in an easy-to-index format. for now it's quicker just to recompute
               --  the four dot products.
               --
               m11 := dDOT44 (Ra.all, code1,   Rb.all, a1);
               m12 := dDOT44 (Ra.all, code1,   Rb.all, a2);
               m21 := dDOT44 (Ra.all, code2,   Rb.all, a1);
               m22 := dDOT44 (Ra.all, code2,   Rb.all, a2);

               declare
                  k1 : constant math.Real := m11 * Sb (a1);
                  k2 : constant math.Real := m21 * Sb (a1);
                  k3 : constant math.Real := m12 * Sb (a2);
                  k4 : constant math.Real := m22 * Sb (a2);
               begin
                  quad (0) := c1 - k1 - k3;
                  quad (1) := c2 - k2 - k4;
                  quad (2) := c1 - k1 + k3;
                  quad (3) := c2 - k2 + k4;
                  quad (4) := c1 + k1 + k3;
                  quad (5) := c2 + k2 + k4;
                  quad (6) := c1 + k1 - k3;
                  quad (7) := c2 + k2 - k4;
               end;


               declare
                  --  find the size of the reference face
                  rect  : constant c_Vector_2 := (Sa (code1),
                                         Sa (code2));

                  --  intersect the incident and reference faces
                  ret   : aliased c_Vector_16;
                  n     : constant Integer       := intersectRectQuad2 (rect, quad'Access, ret'Access);     pragma Assert (n >= 1);   -- This should never fail.

                  point : array (Integer range 1 .. 8) of Vector;                -- penetrating contact points
                  dep   : math.Vector_8;                                        -- depths for those points
                  det1  : constant math.Real    := 1.0 / (m11 * m22 - m12*m21);

                  cnum  : Integer;
               begin
                  --  convert the intersection points into reference-face coordinates,
                  --  and compute the contact position and depth for each point. only keep
                  --  those points that have a positive (penetrating) depth. delete points in
                  --  the 'ret' array as necessary so that 'point' and 'ret' correspond.
                  --
                  m11 := m11 * det1;
                  m12 := m12 * det1;
                  m21 := m21 * det1;
                  m22 := m22 * det1;

                  cnum := 0;                        -- number of penetrating contact points found

                  for j in 0 .. n - 1
                  loop
                     declare
                        k1 : math.Real :=  m22 * (ret (j * 2) - c1)   -   m12 * (ret (j * 2 + 1) - c2);
                        k2 : math.Real := -m21 * (ret (j * 2) - c1)   +   m11 * (ret (j * 2 + 1) - c2);
                     begin
                        for i in 1 .. 3
                        loop
                           point (cnum + 1)(i) := center (i)  +  k1 * Rb (i, a1)  +  k2 * Rb (i, a2);
                        end loop;

                        dep (cnum + 1) := Sa (codeN + 1)  -  dDOT (normal2,  point (cnum + 1));

                        if  dep (cnum + 1) >= 0.0 then
                           ret (cnum * 2) := ret (j * 2  );
                           ret (cnum * 2 + 1) := ret (j * 2 + 1);

                           cnum           := cnum + 1;
                        end if;
                     end;
                  end loop;

                  pragma Assert (cnum >= 1);   -- This should never fail.



                  --  We can't generate more contacts than we actually have.
                  --
                  if maxc > cnum then
                     maxc := cnum;
                  end if;

                  if maxc < 1 then
                     maxc := 1;
                  end if;



                  if cnum <= maxc then      -- We have less contacts than we need, so we use them all.
                     declare
                        pointInWorld : Math.Vector_3;
                     begin
                        if code < 4 then
                           for j in 1 .. cnum
                           loop
                              for i in 1 .. 3
                              loop
                                 pointInWorld (i) := point (j)(i) + pa (i);
                              end loop;

                              output.addContactPoint (-to_Math (normal.all),  pointInWorld,  -dep (j));
                           end loop;
                        else
                           for j in 1 .. cnum
                           loop
                              for i in 1 .. 3
                              loop
                                 pointInWorld (i) := point (j)(i)  +  pa (i) - normal (i) * dep (j);
                                 -- pointInWorld[i] = point[j*3+i] + pa[i];
                              end loop;

                              output.addContactPoint (-to_Math (normal.all), pointInWorld, -dep (j));
                           end loop;
                        end if;
                     end;

                  else                     -- We have more contacts than are wanted, some of them must be culled.
                     declare               -- Find the deepest point, it is always the first contact.
                        i1       : Integer   := 0;
                        maxdepth : math.Real := dep (1);

                        iret     : math.Integers (1 .. 8);

                        posInWorld : math.Vector_3;
                     begin

                        for i in 2 .. cnum
                        loop
                           if dep (i) > maxdepth then
                              maxdepth := dep (i);
                              i1       := i - 1;
                           end if;
                        end loop;

                        cullPoints2 (cnum, ret'Access, maxc, i1, iret);

                        for j in 1 .. maxc
                        loop
                           --      dContactGeom *con = CONTACT(contact,skip*j);
                           --    for (i=0; i<3; i++) con->pos[i] = point[iret[j]*3+i] + pa[i];
                           --  con->depth = dep[iret[j]];

                           for i in 1 .. 3
                           loop
                              posInWorld (i) := point (iret (j) + 1) (i)  +  pa (i);
                           end loop;

                           if code < 4 then
                              output.addContactPoint (-to_Math (normal.all),
                                                       posInWorld,
                                                      -dep (iret (j) + 1));
                           else
                              output.addContactPoint (-to_Math (normal.all),
                                                       posInWorld - to_Math (normal.all) * dep (iret (j) + 1),
                                                      -dep (iret (j) + 1));
                           end if;
                        end loop;

                        cnum := maxc;
                     end;
                  end if;


                  return_code.all := code;
                  return cnum;
               end;
            end;
         end;
      end;

   end dBoxBox2;











   overriding procedure getClosestPoints (Self : in out Item;   input       : in     ClosestPointInput;
                                                     output      : in out Result'Class;
                                                     swapResults : in     Boolean          := False)
   is
      pragma Unreferenced (swapResults);
      use linear_Algebra_3d;

      transformA : Transform_3d renames input.m_transformA;
      transformB : Transform_3d renames input.m_transformB;

      skip : Integer := 0;
      --          contact : access dContactGeom;    *contact = 0;

      R1,
      R2 : math.Matrix_3x3;

   begin
      for j in 1 .. 3
      loop
         R1 (1, j) := transformA.Rotation (j, 1);
         R2 (1, j) := transformB.Rotation (j, 1);

         R1 (2, j) := transformA.Rotation (j, 2);
         R2 (2, j) := transformB.Rotation (j, 2);

         R1 (3, j) := transformA.Rotation (j, 3);
         R2 (3, j) := transformB.Rotation (j, 3);
      end loop;


      declare
         normal      : aliased Vector;
         depth       : aliased math.Real;
         return_code : aliased Integer;
         maxc        : constant Integer  := 4;

         cnum : Integer;
         pragma Unreferenced (cnum);
      begin
         cnum := dBoxBox2 (Vector (transformA.Translation),
                           R1,
                           2.0 * Self.m_box1.getHalfExtentsWithMargin,

                           Vector (transformB.Translation),
                           R2,
                           2.0 * Self.m_box2.getHalfExtentsWithMargin,

                           normal'Access, depth'Access, return_code'Access,
                           maxc,
                           output'Access);
      end;

   end getClosestPoints;



end impact.d3.collision.Detector.discrete.box_box;
