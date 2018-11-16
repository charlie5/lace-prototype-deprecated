with impact.d3.Vector;

package body impact.d3.graham_scan_2d_convex_Hull
is



   function to_GrahamVector2 (org      : in math.Vector_3;
                              orgIndex : in Integer) return GrahamVector2
   is
      Self : GrahamVector2;

   begin
      Self.Vector     := org;
      Self.m_orgIndex := orgIndex;


      return Self;
   end to_GrahamVector2;






--     function to_btAngleCompareFunc (anchor : in  math.Vector_3) return btAngleCompareFunc
--     is
--        Self : btAngleCompareFunc;
--     begin
--        Self.m_anchor := anchor;
--        return Self;
--     end;











   procedure GrahamScanConvexHull2D (originalPoints : in out GrahamVector2_Vector;
                                     hull           : in out GrahamVector2_Vector)
   is
      use type ada.Containers.Count_Type;

   begin
      if originalPoints.Length <= 1 then
         for i in 1 .. originalPoints.Length
         loop
            hull.append (originalPoints.Element (1));
         end loop;

         return;
      end if;


      --  step1 : find anchor point with smallest x/y and move it to first location
      --  also precompute angles
      --
      for i in 1 .. Integer (originalPoints.Length)
      loop
         declare
            left  : math.Vector_3 := originalPoints.Element (i).Vector;
            right : math.Vector_3 := originalPoints.Element (1).Vector;

         begin
            if        left (1) < right (1)
              or else (       not (right (1) < left  (1))
                       and then       left (2) < right (2))
            then
               originalPoints.swap (1, i);
            end if;
         end;
      end loop;



      for i in 1 .. Integer (originalPoints.Length)
      loop
         declare
            use impact.d3.Vector, Math;

            xvec : constant math.Vector_3 := (1.0, 0.0, 0.0);
            ar   : constant math.Vector_3 := originalPoints.Element (i).Vector - originalPoints.Element (1).Vector;

            Pad : GrahamVector2 := originalPoints.Element (i);
         begin
            Pad.m_angle := dot (cross (xvec, ar),
                                (0.0, 0.0, 1.0))  / length (ar);
            originalPoints.replace_Element (i, Pad);
         end;
      end loop;


      --  step 2: sort all points, based on 'angle' with this anchor
      --
      declare
         i    : Integer            := 1;

         --           comp : btAngleCompareFunc := to_btAngleCompareFunc (anchor => originalPoints (1).Vector);

         anchor : constant math.Vector_3 := originalPoints.Element (1).Vector;

         function "<" (a, b : in    GrahamVector2) return Boolean
         is
            use impact.d3.Vector, Math;

            al, bl : math.Real;
         begin

            if a.m_angle /= b.m_angle then
               return a.m_angle < b.m_angle;

            else
               al := length2 (a.Vector - anchor);
               bl := length2 (b.Vector - anchor);

               if al /= bl then
                  return  al < bl;
               else
                  return a.m_orgIndex < b.m_orgIndex;
               end if;

            end if;

         end "<";


         Point_1 : GrahamVector2;
         Point_2 : GrahamVector2;

         package Sorter is new GrahamVector2_Vectors.Generic_Sorting ("<");

      begin
         Point_1 := originalPoints.First_Element;
         originalPoints.delete_First;

         Point_2 := originalPoints.First_Element;
         originalPoints.delete_First;

         Sorter.sort (originalPoints);   --           originalPoints.quickSortInternal (comp, 1, originalPoints.Length - 1);

         originalPoints.Prepend (Point_2);
         originalPoints.Prepend (Point_1);




         while i < 3
         loop
            hull.append (originalPoints.Element (i));
            i := i + 1;
         end loop;


         --  step 3: keep all 'convex' points and discard concave points (using back tracking)
         --
         while i <= Integer (originalPoints.Length)
         loop
            declare
               use Math, impact.d3.Vector;

               isConvex : Boolean      := False;
               a, b     : math.Vector_3;

            begin
               while      not isConvex
                 and then hull.Length > 1
               loop
                  a := hull.Element (Integer (hull.Length) - 2).Vector;
                  b := hull.Element (Integer (hull.Length) - 1).Vector;

                  isConvex := dot (cross (a - b,
                    a - originalPoints.Element (i).Vector),
                    (0.0, 0.0, 1.0)              )  >  0.0;

                  if not isConvex then
                     hull.delete_Last;
                  else
                     hull.append (originalPoints.Element (i));
                  end if;
               end loop;
            end;

            i := i + 1;
         end loop;

      end;

   end GrahamScanConvexHull2D;



end impact.d3.graham_scan_2d_convex_Hull;
