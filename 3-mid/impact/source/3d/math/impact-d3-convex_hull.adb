with impact.d3.Vector;
with impact.d3.Scalar;
with ada.unchecked_Deallocation;



package body impact.d3.convex_Hull
is



   ------------
   --- Globals
   --


   generic
      type T is private;
   procedure Any_swap (A, B : in out T);

   procedure Any_swap (A, B : in out T)
   is
      Pad : constant T := A;
   begin
      A := B;
      B := Pad;
   end Any_swap;








   -----------
   --  Utility
   --


   function PlaneLineIntersection (plane  : in btPlane;
                                   p0, p1 : in math.Vector_3) return math.Vector_3;

   function PlaneProject          (plane  : in btPlane;
                                   point  : in math.Vector_3) return math.Vector_3;






   function ThreePlaneIntersection (p0, p1, p2 : in btPlane) return math.Vector_3
   is
      use impact.d3.Vector, Math;

      N1   : constant math.Vector_3 := p0.normal;
      N2   : constant math.Vector_3 := p1.normal;
      N3   : constant math.Vector_3 := p2.normal;

      n2n3 : math.Vector_3 := cross (N2, N3);
      n3n1 : math.Vector_3 := cross (N3, N1);
      n1n2 : math.Vector_3 := cross (N1, N2);


      N1_dot_n2n3 : constant math.Real := dot (N1, n2n3);        pragma Assert (abs (N1_dot_n2n3)  >  0.000001);
      quotient    : constant math.Real := -1.0 / N1_dot_n2n3;

      potentialVertex : math.Vector_3;

   begin
      n2n3 := n2n3 * p0.dist;
      n3n1 := n3n1 * p1.dist;
      n1n2 := n1n2 * p2.dist;

      potentialVertex := (n2n3 + n3n1 + n1n2) * quotient;

      return potentialVertex;
   end ThreePlaneIntersection;









   function DistanceBetweenLines (ustart, udir,
                                  vstart, vdir   : in     math.Vector_3;
                                  upoint, vpoint : access math.Vector_3 := null) return math.Real;

   function TriNormal            (v0, v1, v2 : in math.Vector_3) return math.Vector_3;

--     function NormalOf             (vert : in math.Vector_3;
--                                    n    : in Integer      ) return math.Vector_3;







   --- PlaneLineIntersection
   --
   --  Returns the point where the line 'p0 - p1' intersects the plane 'n & d'.
   --

   dif : math.Vector_3;

   function PlaneLineIntersection (plane  : in btPlane;
                                   p0, p1 : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, Math;
   begin
      dif := p1 - p0;

      declare
         dn : constant math.Real := dot (plane.normal, dif);
         t  : constant math.Real :=   -(plane.dist + dot (plane.normal, p0))
                           / dn;
      begin
         return p0  +  (dif * t);
      end;
   end PlaneLineIntersection;







   function PlaneProject          (plane  : in btPlane;
                                   point  : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, Math;
   begin
      return point -    plane.normal
                      * (dot (point, plane.normal)  +  plane.dist);
   end PlaneProject;





   --  Return the normal of the triangle inscribed by v0, v1, and v2.
   --
   function TriNormal (v0, v1, v2 : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, Math;

      cp : constant math.Vector_3 := cross (v1 - v0,
                                   v2 - v1);
      m  : constant math.Real     := length (cp);

   begin
      if m = 0.0 then
         return (1.0, 0.0, 0.0);
      end if;

      return cp *  (1.0 / m);
   end TriNormal;






   cp : math.Vector_3;

   function DistanceBetweenLines (ustart, udir,
                                  vstart, vdir   : in     math.Vector_3;
                                  upoint, vpoint : access math.Vector_3 := null) return math.Real
   is
      use impact.d3.Vector, Math;
   begin
      cp := normalized (cross (udir, vdir));

      declare
         distu : constant math.Real := -dot (cp, ustart);
         distv : constant math.Real := -dot (cp, vstart);
         dist  : constant math.Real :=  abs (distu - distv);

         plane : btPlane;

      begin
         if upoint /= null then
            plane.normal :=  normalized (cross (vdir, cp));
            plane.dist   := -dot        (plane.normal, vstart);
            upoint.all   := PlaneLineIntersection (plane,  ustart,  ustart + udir);
         end if;

         if vpoint /= null then
            plane.normal :=  normalized (cross (udir, cp));
            plane.dist   := -dot        (plane.normal, ustart);
            vpoint.all   := PlaneLineIntersection (plane,  vstart,  vstart + vdir);
         end if;


         return dist;
      end;
   end DistanceBetweenLines;








   --- HullDesc
   --



   function to_HullDesc return HullDesc
   is
      Self : HullDesc;
   begin
      Self.mFlags          := QF_DEFAULT;
      Self.mVcount         := 0;
      Self.mVertexStride   := math.Vector_3'Size / 8;
      Self.mNormalEpsilon  := 0.001;
      Self.mMaxVertices    := 4096; -- maximum number of points to be considered for a convex hull.
      Self.mMaxFaces           := 4096;

      return Self;
   end to_HullDesc;




   function to_HullDesc (flag     : in     HullFlag;
                         vcount   : in     Natural;
                         vertices : access vector_3_Array;
                         stride   : in     Positive := math.Vector_3'Size / 8) return HullDesc
   is
      Self : HullDesc;
   begin
      Self.mFlags          := flag;
      Self.mVcount         := vcount;
      Self.mVertices       := vertices;
      Self.mVertexStride   := stride;
      Self.mNormalEpsilon  := 0.001;
      Self.mMaxVertices    := 4096; -- maximum number of points to be considered for a convex hull.

      return Self;
   end to_HullDesc;






   function HasHullFlag (Self : in HullDesc;   Flag : in HullFlag) return Boolean
   is
   begin
      if (Self.mFlags and flag) /= 0 then
         return True;
      end if;

      return False;
   end HasHullFlag;






   procedure SetHullFlag (Self : in out  HullDesc;   Flag : in HullFlag)
   is
   begin
      Self.mFlags := Self.mFlags or flag;
   end SetHullFlag;





   procedure ClearHullFlag (Self : in out  HullDesc;   Flag : in HullFlag)
   is
   begin
      Self.mFlags := Self.mFlags and not flag;
   end ClearHullFlag;






   ------------
   --- btPlane
   --


   function to_btPlane return btPlane
   is
   begin
      return (normal => math.Origin_3d,
              dist   => 0.0);
   end to_btPlane;





   function to_btPlane (n : in math.Vector_3;
                        d : in math.Real  ) return btPlane
   is
   begin
      return (normal => n,
              dist   => d);
   end to_btPlane;




   function PlaneFlip (Self : in btPlane) return btPlane
   is
      use Math;
   begin
      return to_btPlane (-Self.normal,
                         -Self.dist);
   end PlaneFlip;




   function coplanar  (a, b : in btPlane) return Boolean
   is
   begin
      return    a = b
        or else a = PlaneFlip (b);
   end coplanar;






   -------------
   --- HalfEdge
   --


   function to_HalfEdge (ea : Integer;
                         v  : Integer;
                         p  : Integer) return HalfEdge
   is
   begin
      return (ea => ea,
              v  => v,
              p  => p);
   end to_HalfEdge;












   ------------
   --- ConvexH
   --

   use type Flags;

   COPLANAR_Flag    : constant Flags := 0;
   UNDER_Flag       : constant Flags := 1;
   OVER_Flag        : constant Flags := 2;
   SPLIT_Flag       : constant Flags := OVER_Flag or UNDER_Flag;

   PAPERWIDTH       : constant              := 0.001;
   planetestepsilon : constant math.Real    := PAPERWIDTH;








   function to_ConvexH (vertices_size,
                        edges_size,
                        facets_size : in Positive) return ConvexH
   is
      use ada.Containers;

      Self : ConvexH;
   begin
      Self.vertices.set_Length (Count_type (vertices_size));
      Self.edges   .set_Length (Count_type ( edges_size));
      Self.facets  .set_Length (Count_type (facets_size));


      return Self;
   end to_ConvexH;






   ---------
   --- int4
   --



   function to_int4 (x, y, z, w : in Integer) return int4
   is
   begin
      return (x => x,
              y => y,
              z => z,
              w => w);
   end to_int4;







   function Element (Self : in int4;   i : in Integer) return Integer
   is
   begin
      case i
      is
         when 1      => return Self.x;
         when 2      => return Self.y;
         when 3      => return Self.z;
         when 4      => return Self.w;
         when others => raise Program_Error;
      end case;
   end Element;




   function Element (Self : access int4;   i : in Integer) return access Integer
   is
   begin
      case i
      is
         when 1      => return Self.x'Access;
         when 2      => return Self.y'Access;
         when 3      => return Self.z'Access;
         when 4      => return Self.w'Access;
         when others => raise Program_Error;
      end case;
   end Element;









   ----------------
   --- HullLibrary
   --



   function m_vertexIndexMapping (Self : access HullLibrary) return access Containers.Integer_Vector
   is
   begin
      return Self.m_vertexIndexMapping'Access;
   end m_vertexIndexMapping;





   procedure ReleaseHull (result : in out PHullResult)
   is
      use type ada.containers.Count_type;
   begin
      if result.m_Indices.Length /= 0 then
         result.m_Indices.clear;
      end if;

      result.mVcount     := 0;
      result.mIndexCount := 0;
      result.mVertices   := null;
   end ReleaseHull;








   function CreateConvexHull (Self : access HullLibrary;   desc   : in     HullDesc  'Class;     -- describes the input request
                                                           result : access HullResult'Class)     -- contains the result
                              return HullError
   is
      use ada.Containers;

      ret          : HullError  := QE_FAIL;
      hr           : aliased PHullResult;

      vcount       : constant Natural := Natural'Max (desc.mVcount, 8);
      vertexSource : aliased vector_3_Array := (1 .. vcount => <>);
      scale        : aliased math.Vector_3;
      ovcount      : aliased Natural;

      ok           : Boolean;

   begin
--        vertexSource.set_Length (Count_type (vcount));

      --  Normalize point cloud, remove duplicates !
      --
      ok := Self.CleanupVertices (desc.mVcount, desc.mVertices,             desc.mVertexStride,
                                  ovcount'Access,      vertexSource'Access,
                                  desc.mNormalEpsilon,
                                  scale'Access);

      if ok then
         --  Scale vertices back to their original size.
         --
         for i in 1 .. ovcount
         loop
            declare
               use impact.d3.Vector;
               v : math.Vector_3 renames vertexSource (i);
            begin
               v := Scaled (v,  by => Scale);
--                 v (1) := V (1) * scale[0];
--                 v[1]*=scale[1];
--                 v[2]*=scale[2];
            end;
         end loop;

         ok := Self.ComputeHull (ovcount,  vertexSource'Access,  hr'Access,  desc.mMaxVertices);

         if ok then
            declare
               vertexScratch : aliased Vector_3_Array := (1 .. hr.mVcount => <>);
            begin
               --                          vertexScratch.set_length (hr.mVcount);

               --  Re-index triangle mesh so it refers to only used vertices, rebuild a new vertex table.
               --
               Self.BringOutYourDead (hr.mVertices,  hr.mVcount,           vertexScratch'Access,
                                      ovcount,       hr.m_Indices'Access,  hr.mIndexCount);

               ret := QE_OK;

               if desc.HasHullFlag (QF_TRIANGLES) then    -- if he wants the results as triangle!
                  result.mPolygons          := False;
                  result.mNumOutputVertices := ovcount;

                  --                                  result.m_OutputVertices.set_Length (Count_type (ovcount));
                  result.m_OutputVertices := new vector_3_Array'(1 .. ovcount => <>);

                  result.mNumFaces          := hr.mFaceCount;
                  result.mNumIndices        := hr.mIndexCount;

                  result.m_Indices.set_Length (Count_type (hr.mIndexCount));

                  result.m_OutputVertices.all (1 .. ovcount) := vertexScratch (1 .. ovcount);
                  --                                  memcpy(&result.m_OutputVertices[0], &vertexScratch[0], sizeof(impact.d3.Vector)*ovcount );

                  if desc.HasHullFlag (QF_REVERSE_ORDER) then
                     declare
                        source : TUIntArray renames hr    .m_Indices;
                        dest   : TUIntArray renames result.m_Indices;

                        Offset : Integer := 0;
                     begin
                        for i in 1 .. hr.mFaceCount
                        loop
                           dest (Offset + 1) := source.Element (Offset + 3);
                           dest (Offset + 2) := source.Element (Offset + 2);
                           dest (Offset + 3) := source.Element (Offset + 1);

                           Offset := Offset + 3;
                        end loop;
                     end;

                  else
                     for i in 1 .. hr.mIndexCount
                     loop
                        result.m_Indices.replace_Element (i,  hr.m_Indices (i));
                     end loop;
                     --                                          memcpy(&result.m_Indices[0], &hr.m_Indices[0], sizeof(unsigned int)*hr.mIndexCount);
                  end if;

               else
                  result.mPolygons          := True;
                  result.mNumOutputVertices := ovcount;

                  result.m_OutputVertices := new vector_3_Array'(1 .. ovcount => <>);

                  result.mNumFaces          := hr.mFaceCount;
                  result.mNumIndices        := hr.mIndexCount + hr.mFaceCount;

                  result.m_Indices.set_Length (Count_type (result.mNumIndices));

                  result.m_OutputVertices (1 .. ovcount) := vertexScratch (1 .. ovcount);
                  --                                  memcpy(&result.m_OutputVertices[0], &vertexScratch[0], sizeof(impact.d3.Vector)*ovcount );

                  declare
                     source : TUIntArray renames hr    .m_Indices;
                     dest   : TUIntArray renames result.m_Indices;

                     d_Offset : Integer := 0;
                     s_Offset : Integer := 0;

                  begin
                     for i in 1 .. hr.mFaceCount
                     loop
                        dest (d_Offset + 1) := 3;

                        if desc.HasHullFlag (QF_REVERSE_ORDER) then
                           dest (d_Offset + 2) := source.Element (s_Offset + 3);
                           dest (d_Offset + 3) := source.Element (s_Offset + 2);
                           dest (d_Offset + 4) := source.Element (s_Offset + 1);
                        else
                           dest (d_Offset + 2) := source.Element (s_Offset + 1);
                           dest (d_Offset + 3) := source.Element (s_Offset + 2);
                           dest (d_Offset + 4) := source.Element (s_Offset + 3);
                        end if;

                        d_Offset := d_Offset + 4;
                        s_Offset := s_Offset + 3;
                     end loop;
                  end;

               end if;

               ReleaseHull (hr);
            end;
         end if;
      end if;


      return ret;
   end CreateConvexHull;






   function ReleaseResult    (Self : in HullLibrary;   result : access HullResult'Class)     -- release memory allocated for this result, we are done with it.
                              return HullError
   is
      pragma Unreferenced (Self);
      use type ada.containers.Count_type;

      procedure free is new ada.unchecked_Deallocation (vector_3_Array, access_vector_3_Array);

   begin
      if result.m_OutputVertices /= null then
         result.mNumOutputVertices := 0;
         free (result.m_OutputVertices);
      end if;

      if result.m_Indices.Length /= 0 then
         result.mNumIndices := 0;
         result.m_Indices.clear;
      end if;


      return QE_OK;
   end ReleaseResult;








   ---------
   --- int3
   --


   package body int3_Forge
   is

      function to_int3 (x, y, z : in Integer) return int3
      is
         Self : constant int3 := (x, y, z);
      begin
         return Self;
      end to_int3;

   end int3_Forge;





   function Element (Self : in     int3;   i : in Integer) return        Integer
   is
   begin
      case i
      is
         when 1      =>   return Self.x;
         when 2      =>   return Self.y;
         when 3      =>   return Self.z;
         when others =>   raise Program_Error;
      end case;
   end Element;




   function Element (Self : access int3;   i : in Integer) return access Integer
   is
   begin
      case i
      is
         when 1      =>   return Self.x'Access;
         when 2      =>   return Self.y'Access;
         when 3      =>   return Self.z'Access;
         when others =>   raise Program_Error;
      end case;
   end Element;









   -------------------
   --- btHullTriangle
   --


   function to_btHullTriangle (a, b, c : in Integer) return btHullTriangle
   is
      Self : btHullTriangle := (int3_Forge.to_int3 (a, b, c) with others => <>);
   begin
      Self.n    := (-1, -1, -1);
      Self.vmax := -1;
      Self.rise :=  0.0;

      return Self;
   end to_btHullTriangle;






   er : aliased Integer := -1;

   function neib (Self : access btHullTriangle;   a, b : in Integer) return access Integer
   is
      i1, i2 : Integer;

   begin
      for i in 1 .. 3
      loop
         i1 := (i + 1) mod 3;
         i2 := (i + 2) mod 3;


         if         Self.Element (i) = a
           and then Self.Element (i1) = b
         then
            return Self.n'Access.Element (i2);
         end if;


         if         Self.Element (i) = b
           and then Self.Element (i1) = a
         then
            return Self.n'Access.Element (i2);
         end if;

      end loop;


      pragma Assert (False);
      return er'Access;
   end neib;








   ----------------
   --- HullLibrary
   --


   --- HullLibrary - Utility
   --


   function PlaneTest (p : in btPlane;
                       v : in math.Vector_3) return Flags
   is
      use impact.d3.Vector,  math.Vectors;

      a : constant math.Real := dot (v, p.normal)  +  p.dist;
   begin
      if    a >  planetestepsilon then   return     OVER_Flag;
      elsif a < -planetestepsilon then   return    UNDER_Flag;
      else                               return COPLANAR_Flag;
      end if;
   end PlaneTest;






   function SplitTest (convex : access ConvexH;
                       plane  : in     btPlane) return Flags
   is
      use impact.d3.Vector,  math.Vectors;

      Flag : Flags := 0;
   begin
      for i in 1 .. Integer (convex.vertices.Length)
      loop
         Flag := Flag  or  PlaneTest (plane,
                                      convex.vertices (i));
      end loop;


      return Flag;
   end SplitTest;






   type VertFlag is
      record
         planetest,
         junk,
         undermap,
         overmap  : Integer;
      end record;





   type EdgeFlag is
      record
         planetest,
         fixes,
         undermap,
         overmap  : Integer;
      end record;





   type PlaneFlag is
      record
         undermap,
         overmap  : Integer;
      end record;






   type Coplanar_t is
      record
         ea,
         v0,
         v1 : Integer;
      end record;






--     generic
--        type T is private;
--        with function Element (Self : in T;   Index : in Integer) return math.Vector_3;
--
--     function maxdirfiltered (p     : in     T;
--                              count : in     Natural;
--                              dir   : in     math.Vector_3;
--                              allow : access bullet.Containers.integer_Vector) return Integer;


--     function maxdirfiltered (p     : in     T;
--                              count : in     Natural;
--                              dir   : in     math.Vector_3;
--                              allow : access bullet.Containers.integer_Vector) return Integer
--     is
--        use impact.d3.Vector;
--
--        pragma assert (count /= 0);
--        m : Integer := -1;
--     begin
--        for i in 1 .. count
--          loop
--           if allow.all (i) /= 0 then
--
--              if        m = -1
--                or else    dot (Element (p, i),  dir)
--                        >  dot (Element (p, m),  dir)
--              then
--                 m := i;
--              end if;
--
--           end if;
--        end loop;
--
--        pragma assert (m /= -1);
--
--        return m;
--     end maxdirfiltered;



   function maxdirfiltered (p     : in     Vector_3_Array;
                            count : in     Natural;
                            dir   : in     math.Vector_3;
                            allow : access Containers.integer_Vector) return Integer
   is
      use impact.d3.Vector;

      pragma Assert (count /= 0);
      m : Integer := -1;
   begin
      for i in 1 .. count
        loop
         if allow.all (i) /= 0 then

            if        m = -1
              or else    dot (p (i),  dir)
                      >  dot (p (m),  dir)
            then
               m := i;
            end if;

         end if;
      end loop;

      pragma Assert (m /= -1);

      return m;
   end maxdirfiltered;






   function orth (v : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector;

      a : constant math.Vector_3 := cross (v,  (0.0, 0.0, 1.0));
      b : constant math.Vector_3 := cross (v,  (0.0, 1.0, 0.0));

   begin
      if length (a)  >  length (b) then   return normalized (a);
      else                                return normalized (b);
      end if;
   end orth;




   function Element (Self : in Vector_3_array;   Index : in Integer) return math.Vector_3
   is
   begin
      return Self (Index);
   end Element;




--     generic
--        type T is private;
--        with function Element (Self : in T;   Index : in Integer) return math.Vector_3;
--
--     function maxdirsterid (p     : in     T;
--                            count : in     Natural;
--                            dir   : in     math.Vector_3;
--                            allow : access bullet.Containers.integer_Vector) return Integer;



   function maxdirsterid (p     : in     Vector_3_array;
                          count : in     Natural;
                          dir   : in     math.Vector_3;
                          allow : access Containers.integer_Vector) return Integer
   is
      use impact.d3.Vector;

--        function my_maxdirfiltered is new maxdirfiltered (T, Element);

      m : Integer  := -1;
      x, xx : math.Real;

      u, v : math.Vector_3;

      ma : Integer := -1;
   begin
      while m = -1
      loop
         m := maxdirfiltered (p, count, dir, allow);

         if allow.all (m) = 3 then
            return m;
         end if;


         u := orth  (dir);
         v := cross (u, dir);

         ma := -1;

         x := 0.0;
         while x <= 360.0
         loop
            declare
               use math.Functions, Math;

               s  : constant math.Real := sin (impact.d3.Scalar.SIMD_RADS_PER_DEG * x);
               c  : constant math.Real := cos (impact.d3.Scalar.SIMD_RADS_PER_DEG * x);

               mb : constant Integer   := maxdirfiltered (p,  count,  dir + (u * s + v*c) * 0.025,  allow);
               mc : Integer;

            begin
               if         ma = m
                 and then mb = m
               then
                  allow.all (m) := 3;
                  return m;
               end if;

               if         ma /= -1     -- Yuck - this is really ugly
                 and then ma /= mb
               then
                  mc := ma;
                  xx := x - 40.0;

                  while xx <= x
                  loop
                     declare
                        s  : constant math.Real := sin (impact.d3.Scalar.SIMD_RADS_PER_DEG * xx);
                        c  : constant math.Real := cos (impact.d3.Scalar.SIMD_RADS_PER_DEG * xx);

                        md : constant Integer   := maxdirfiltered (p,  count,  dir + (u * s + v*c) * 0.025,  allow);

                     begin
                        if         mc = m
                          and then md = m
                        then
                           allow.all (m) := 3;
                           return m;
                        end if;

                        mc := md;
                        xx := xx + 5.0;
                     end;
                  end loop;
               end if;

               ma := mb;
               x  := x + 45.0;
            end;
         end loop;

         allow.all (m) := 0;
         m             := -1;
      end loop;


      pragma Assert (False);
      return m;
   end maxdirsterid;







   function above (vertices : in vector_3_array;
                   t        : in int3        ;
                   p        : in math.Vector_3;
                   epsilon  : in math.Real   ) return Boolean
   is
      use impact.d3.Vector, Math;

      n : constant math.Vector_3 := TriNormal (vertices (t.Element (1)),
                                      vertices (t.Element (2)),
                                      vertices (t.Element (3)));
   begin
      return    dot (n,  p - vertices (t.Element (1)))
             >  epsilon;                                     -- EPSILON ???
   end above;





   function hasedge (t : in int3;   a, b : in Integer) return Boolean
   is
      i1 : Integer;

   begin
      for i in 1 .. 3
      loop
         i1 := (i + 1) mod 3;

         if         t.Element (i) = a
           and then t.Element (i1) = b
         then
            return True;
         end if;
      end loop;


      return False;
   end hasedge;






   function hasvert (t : in int3;   v : in Integer) return Boolean
   is
   begin
      return    t.Element (1) = v
        or else t.Element (2) = v
        or else t.Element (3) = v;
   end hasvert;





   function shareedge (a, b : in int3) return Boolean
   is
      i1 : Integer;

   begin
      for i in 1 .. 3
      loop
         i1 := (i + 1) mod 3;

         if hasedge (a,  b.Element (i1),
                         b.Element (i))
         then
            return True;
         end if;
      end loop;


      return False;
   end shareedge;












   --- HullLibrary - Operations
   --


   function ComputeHull (Self : access HullLibrary;   vcount       : in     Natural        ;
                                                      vertices     : access Vector_3_array  ;
                                                      result       : access PHullResult'Class;
                         vlimit       : in     Natural        ) return Boolean
   is
      tris_count : aliased Natural;
      ret        : constant Integer := Self.calchull (vertices,  vcount,   result.m_Indices'Access,  tris_count'Access,  vlimit);
   begin
      if ret = 0 then
         return False;
      end if;

      result.mIndexCount := tris_count * 3;
      result.mFaceCount  := tris_count;
      result.mVertices   := vertices;
      result.mVcount     := vcount;

      return True;
   end ComputeHull;










   function  allocateTriangle   (Self : access HullLibrary;   a, b, c      : in Integer          ) return btHullTriangle_view
   is
      tr : constant btHullTriangle_view := new btHullTriangle'(to_btHullTriangle (a, b, c));
   begin
      tr.id := Integer (Self.m_tris.Length);
      Self.m_tris.append (tr);

      return tr;
   end allocateTriangle;











   procedure deAllocateTriangle (Self : in out HullLibrary;   the_Triangle : in out btHullTriangle_view)
   is
      pragma Assert (Self.m_tris (the_Triangle.id) = the_Triangle);
      procedure free is new ada.unchecked_Deallocation (btHullTriangle'Class, btHullTriangle_view);

   begin
      Self.m_tris (the_Triangle.id) := null;

      the_Triangle.destruct;
      free (the_Triangle);
   end deAllocateTriangle;









   procedure b2bfix (Self : in out HullLibrary;   s, t         : in btHullTriangle_view)
   is
   begin
      for i in 1 .. 3
      loop
         declare
            i1 : constant Integer := (i + 1) mod 3;
            i2 : constant Integer := (i + 2) mod 3;

            a  : constant Integer := s.Element (i1);
            b  : constant Integer := s.Element (i2);

            pragma Assert (Self.m_tris.Element (s.neib (a, b).all).neib (b, a).all = s.id);
            pragma Assert (Self.m_tris.Element (t.neib (a, b).all).neib (b, a).all = t.id);

         begin
            Self.m_tris.Element (s.neib (a, b).all).neib (b, a).all := t.neib (b, a).all;
            Self.m_tris.Element (t.neib (b, a).all).neib (a, b).all := s.neib (a, b).all;
         end;
      end loop;
   end b2bfix;








   procedure removeb2b          (Self : in out HullLibrary;   s, t         : in out btHullTriangle_view)
   is
   begin
      Self.b2bfix (s, t);

      Self.deAllocateTriangle (s);
      Self.deAllocateTriangle (t);
   end removeb2b;








   procedure checkit            (Self : in out HullLibrary;   t            : in btHullTriangle_view)
   is
      pragma Assert (Self.m_tris (t.id) = t);
   begin
      for i in 1 .. 3
      loop
         declare
            i1 : constant Integer := (i + 1) mod 3;
            i2 : constant Integer := (i + 2) mod 3;
            a  : constant Integer := t.Element (i1);
            b  : constant Integer := t.Element (i2);
         begin
            pragma Assert (a /= b);
            pragma Assert (Self.m_tris.Element (t.n.Element (i)).neib (b, a).all  =  t.id);

            null;
         end;
      end loop;
   end checkit;









   function  extrudable         (Self : in     HullLibrary;   epsilon      : in math.Real        ) return btHullTriangle_view
   is
      t : btHullTriangle_view;
   begin
      for i in 1 .. Integer (Self.m_tris.Length)
      loop
         if        t = null
           or else (       Self.m_tris (i) /= null
                    and then t.rise  <  Self.m_tris (i).rise)
         then
            t := Self.m_tris (i);
         end if;
      end loop;


      if t.rise > epsilon then   return t;
      else                       return null;
      end if;
   end extrudable;











   function  calchull (Self : access HullLibrary;   verts        : access Vector_3_array;
                                                       verts_count  : in     Natural;
                                                       tris_out     : access TUIntArray;
                                                       tris_count   : access Natural;
                       vlimit       : in     Integer      ) return Integer
   is
      use type ada.containers.Count_type;

      rc : constant Integer := Self.calchullgen (verts, verts_count, vlimit);
      ts : Containers.integer_vector;

   begin
      if rc = 0 then
         return 0;
      end if;

      for i in 1 .. Integer (Self.m_tris.Length)
      loop
         if Self.m_tris (i) /= null then

            for j in 1 .. 3
            loop
               ts.append (Self.m_tris (i).all.Element (j));
            end loop;

            Self.deAllocateTriangle (Self.m_tris (i));
         end if;
      end loop;


      tris_count.all := Integer (ts.Length) / 3;
      tris_out.set_Length (ts.Length);

      for i in 1 .. Integer (ts.Length)
      loop
         tris_out.replace_Element (i,  ts (i));
      end loop;

      Self.m_tris.set_Length (0);


      return 1;
   end calchull;










   function  calchullgen (Self : access HullLibrary;   verts        : access Vector_3_array;
                                                       verts_count  : in     Natural;
                          the_vlimit   : in     Natural    ) return Integer
   is
      vlimit : Natural := the_vlimit;
   begin
      if verts_count < 4 then
         return 0;
      end if;


      if vlimit = 0 then
         vlimit := 1000000000;
      end if;

      declare
         use impact.d3.Vector, Math, ada.Containers;

         bmin      : math.Vector_3 := verts (1);
         bmax      : math.Vector_3 := verts (1);

         isextreme : Containers.integer_Vector;
         allow     : aliased Containers.integer_Vector;

         epsilon   : math.Real;
         p         : int4;

      begin
         isextreme.reserve_Capacity (Count_type (verts_count));
         allow    .reserve_Capacity (Count_type (verts_count));

         for j in 1 .. verts_count
         loop
            allow.append     (1);
            isextreme.append (0);

            setMin (bmin,  verts (j));
            setMax (bmax,  verts (j));
         end loop;


         epsilon := length (bmax - bmin) * 0.001;     pragma Assert (epsilon /= 0.0);


         p := int4 (Self.FindSimplex (verts, verts_count, allow'Access));

         if p.x = -1 then
            return 0; -- simplex failed
         end if;


         declare
            use int3_Forge;

            center : constant math.Vector_3 :=   (verts (p.Element (1))
                                         + verts (p.Element (2))
                                         + verts (p.Element (3))
                                         + verts (p.Element (4)))
                                      / 4.0;                         -- a valid interior point

            t0 : constant btHullTriangle_view := Self.allocateTriangle (p.Element (3),  p.Element (4),  p.Element (2));
            t1 : constant btHullTriangle_view := Self.allocateTriangle (p.Element (4),  p.Element (3),  p.Element (1));
            t2 : constant btHullTriangle_view := Self.allocateTriangle (p.Element (1),  p.Element (2),  p.Element (4));
            t3 : constant btHullTriangle_view := Self.allocateTriangle (p.Element (2),  p.Element (1),  p.Element (3));

            te : btHullTriangle_view;

         begin
            t0.n := to_int3 (3, 4, 2);
            t1.n := to_int3 (4, 3, 1);
            t2.n := to_int3 (1, 2, 4);
            t3.n := to_int3 (2, 1, 3);

            isextreme (p.Element (1)) := 1;
            isextreme (p.Element (2)) := 1;
            isextreme (p.Element (3)) := 1;
            isextreme (p.Element (4)) := 1;

            Self.checkit (t0);
            Self.checkit (t1);
            Self.checkit (t2);
            Self.checkit (t3);

            for j in 1 .. Integer (Self.m_tris.Length)
            loop
               declare
                  t : constant btHullTriangle_view := Self.m_tris (j);
                  pragma Assert (t /= null);
                  pragma Assert (t.vmax < 0);

                  n : constant math.Vector_3 := TriNormal (verts (t.Element (1)),
                                                  verts (t.Element (2)),
                                                  verts (t.Element (3)));
               begin
                  t.vmax := maxdirsterid (verts.all,  verts_count,  n,  allow'Access);
                  t.rise := dot (n,  verts (t.vmax) - verts (t.Element (1)));
               end;
            end loop;


            vlimit := vlimit - 4;

            loop
               exit when vlimit = 0;

               te := Self.extrudable (epsilon);
               exit when te = null;

               declare
                  ti : int3 := int3 (te.all);
                  v  : constant Integer := te.vmax;
                  pragma Assert (v /= -1);
                  pragma Assert (isextreme (v) = 0);              -- wtf we've already done this vertex

                  j : Integer;
               begin
                  isextreme (v) := 1;

                  --  // if(v==p0 || v==p1 || v==p2 || v==p3) continue;     -- done these already

                  j := Integer (Self.m_tris.Length);

                  while j /= 0
                  loop
                     j := j - 1;

                     if Self.m_tris (j) /= null then
                        declare
                           t : constant int3 := int3 (Self.m_tris (j).all);
                        begin
                           if above (verts.all,  t,  verts (v),  0.01 * epsilon) then
                              Self.extrude (Self.m_tris (j), v);
                           end if;
                        end;
                     end if;
                  end loop;


                  --  now check for those degenerate cases where we have a flipped triangle or a really skinny triangle
                  j := Integer (Self.m_tris.Length);

                  while j /= 0
                  loop
                     j := j - 1;

                     if Self.m_tris (j) /= null then
                        exit when not hasvert (int3 (Self.m_tris.Element (j).all),  v);

                        declare
                           nt : constant int3 := int3 (Self.m_tris (j).all);
                        begin
                           if        above  (verts.all,  nt,  center,  0.01 * epsilon)
                             or else length (cross (verts (nt.Element (1)) - verts (nt.Element (0)),
                                                    verts (nt.Element (2)) - verts (nt.Element (1))))  <  epsilon * epsilon * 0.1
                           then
                              declare
                                 nb : btHullTriangle_view := Self.m_tris (Self.m_tris (j).n.Element (1));

                                 pragma Assert (nb /= null);
                                 pragma Assert (not hasvert (int3 (nb.all),  v));
                                 pragma Assert (nb.id < j);
                              begin
                                 Self.extrude (nb, v);
                                 j := Integer (Self.m_tris.Length);
                              end;
                           end if;
                        end;
                     end if;

                  end loop;


                  j := Integer (Self.m_tris.Length);

                  while j /= 0
                  loop
                     declare
                        t : constant btHullTriangle_view := Self.m_tris (j);
                        n : math.Vector_3;
                     begin
                        j := j - 1;

                        if t /= null then
                           exit when t.vmax >= 0;

                           n      := TriNormal (verts (t.Element (1)),
                                                verts (t.Element (2)),
                                                verts (t.Element (3)));
                           t.vmax := maxdirsterid (verts.all, verts_count, n, allow'Access);

                           if isextreme (t.vmax) /= 0 then
                              t.vmax := -1;                 -- already done that vertex - algorithm needs to be able to terminate.
                           else
                              t.rise := dot (n,  verts (t.vmax) - verts (t.Element (1)));
                           end if;
                        end if;
                     end;
                  end loop;

                  vlimit := vlimit - 1;

               end;
            end loop;

         end;
      end;


      return 1;
   end calchullgen;






   function  FindSimplex (Self : in     HullLibrary;   verts        : access Vector_3_array;
                                                              verts_count  : in     Natural;
                          allow        : access Containers.integer_Vector) return int4'Class
   is
      pragma Unreferenced (Self);
      use impact.d3.Vector, Math;

      basis : array (1 .. 3) of math.Vector_3 := ((0.01, 0.02, 1.0), others => <>);

      p0 : constant Integer := maxdirsterid (verts.all, verts_count,  basis (1), allow);
      p1 : constant Integer := maxdirsterid (verts.all, verts_count, -basis (1), allow);
      p2,
      p3 : Integer;

      procedure swap is new Any_swap (Integer);
   begin
      basis (1) := verts (p0) - verts (p1);

      if        p0 = p1
        or else basis (1) = math.Origin_3d
      then
         return to_int4 (-1, -1, -1, -1);
      end if;


      basis (2) := cross ((1.00, 0.02, 0.0), basis (1));
      basis (3) := cross ((-0.02, 1.00, 0.0), basis (1));

      if length (basis (2))  >  length (basis (3)) then
         normalize (basis (2));
      else
         basis (2) := basis (3);
         normalize (basis (2));
      end if;


      p2 := maxdirsterid (verts.all, verts_count, basis (2), allow);

      if p2 = p0  or else  p2 = p1 then
         p2 := maxdirsterid (verts.all, verts_count,  -basis (2), allow);
      end if;

      if p2 = p0  or else  p2 = p1 then
         return to_int4 (-1, -1, -1, -1);
      end if;


      basis (2) := verts (p2) - verts (p0);
      basis (3) := normalized (cross (basis (2),  basis (1)));

      p3 := maxdirsterid (verts.all,  verts_count,  basis (3),  allow);

      if p3 = p0  or else  p3 = p1  or else  p3 = p2 then
         p3 := maxdirsterid (verts.all,  verts_count,  -basis (3),  allow);
      end if;

      if p3 = p0  or else  p3 = p1  or else  p3 = p2 then
         return to_int4 (-1, -1, -1, -1);
      end if;

      pragma Assert (not (p0 = p1 or else  p0 = p2  or else  p0 = p3  or else p1 = p2  or else p1 = p3  or else  p2 = p3));

      if dot (verts (p3) - verts (p0),
              cross (verts (p1) - verts (p0),
                     verts (p2) - verts (p0)))  <  0.0
      then
         swap (p2, p3);
      end if;


      return to_int4 (p0, p1, p2, p3);
   end FindSimplex;









--     function  ConvexHCrop        (Self : in     HullLibrary;   convex       : access ConvexH'Class;
--                                                                slice        : in     btPlane'Class) return access ConvexH'Class
--     is
--     begin
--        return ;
--     end;








   procedure extrude            (Self : in out HullLibrary;   t0           : in out btHullTriangle_view;
                                                              v            : in     Integer)
   is
      use int3_Forge;

      t  : constant int3       := int3 (t0.all);
      n  : constant Natural    := Natural (Self.m_tris.Length);
      ta : btHullTriangle_view := Self.allocateTriangle (v,  t.Element (2),  t.Element (3));
      tb,
      tc : btHullTriangle_view;

   begin
      ta.n                                                                            := to_int3 (t0.n.Element (1),  n + 1,  n + 2);
      Self.m_tris.Element (t0.n.Element (1)).neib (t.Element (2), t.Element (3)).all  := n + 0;

      tb                                                                              := Self.allocateTriangle (v,  t.Element (3),  t.Element (1));
      tb.n                                                                            := to_int3 (t0.n.Element (2),  n + 2,  n + 0);
      Self.m_tris.Element (t0.n.Element (2)).neib (t.Element (3),  t.Element (1)).all := n + 1;

      tc                                                                              := Self.allocateTriangle (v,  t.Element (1),  t.Element (2));
      tc.n                                                                            := to_int3 (t0.n.Element (3),  n + 0,  n + 1);
      Self.m_tris.Element (t0.n.Element (3)).neib (t.Element (1),  t.Element (2)).all := n + 2;

      Self.checkit (ta);
      Self.checkit (tb);
      Self.checkit (tc);

--        if hasvert (int3 (Self.m_tris (ta.n.Element (1)).all),  v) then   Self.removeb2b (ta, Self.m_tris (ta.n.Element (1)));   end if;
--        if hasvert (int3 (Self.m_tris (tb.n.Element (1)).all),  v) then   Self.removeb2b (tb, Self.m_tris (tb.n.Element (1)));   end if;
--        if hasvert (int3 (Self.m_tris (tc.n.Element (1)).all),  v) then   Self.removeb2b (tc, Self.m_tris (tc.n.Element (1)));   end if;

      declare
         my_ta : btHullTriangle_view := Self.m_tris (ta.n.Element (1));
         my_tb : btHullTriangle_view := Self.m_tris (tb.n.Element (1));
         my_tc : btHullTriangle_view := Self.m_tris (tc.n.Element (1));
      begin
         if hasvert (int3 (Self.m_tris (ta.n.Element (1)).all),  v) then   Self.removeb2b (ta, my_ta);   end if;
         if hasvert (int3 (Self.m_tris (tb.n.Element (1)).all),  v) then   Self.removeb2b (tb, my_tb);   end if;
         if hasvert (int3 (Self.m_tris (tc.n.Element (1)).all),  v) then   Self.removeb2b (tc, my_tc);   end if;
      end;

      Self.deAllocateTriangle (t0);
   end extrude;










   --  XXX, might be broken
   --
   procedure addPoint (vcount  : in out Integer;
                       p       : access vector_3_Array;
                       x, y, z : in     math.Real)
   is
      dest : math.Vector_3 renames p (vcount);

   begin
      dest   := (x, y, z);
      vcount := vcount + 1;
   end addPoint;






   function GetDist (px, py, pz : in math.Real;
                     p2         : in math.vector_3) return math.Real
   is
      dx : constant math.Real := px - p2 (1);
      dy : constant math.Real := py - p2 (2);
      dz : constant math.Real := pz - p2 (3);

   begin
      return dx * dx + dy*dy + dz*dz;
   end GetDist;













   procedure BringOutYourDead (Self : in out HullLibrary;   verts        : access Vector_3_Array;
                                                            vcount       : in     Natural;
                                                            overts       : access Vector_3_Array;
                                                            ocount       :    out Natural;
                                                            indices      : access Containers.integer_Vector;
                               indexcount   : in     Natural)
   is
      use ada.Containers;

      tmpIndices  : Containers.integer_Vector;
      usedIndices : TUIntArray;

      v           : Integer;

   begin
      tmpIndices.set_Length (Self.m_vertexIndexMapping.Length);

      for i in 1 .. Integer (Self.m_vertexIndexMapping.Length)
      loop
         tmpIndices (i) := Self.m_vertexIndexMapping.Element (i);
      end loop;


      usedIndices.append (0, count => Count_type (vcount));         --          memset(&usedIndices[0],0,sizeof(unsigned int)*vcount);
      ocount := 0;


      for i in 1 .. indexcount
      loop
         v := indices.all (i);                          -- original array index
         pragma Assert (       v >= 0
                        and then v <  vcount);


         if usedIndices (v) /= 0 then   -- already remapped
            indices.all (i) := usedIndices (v) - 1;     -- index to new array

         else
            indices.all (i) := ocount;                  --  new index mapping

            overts (ocount)(1) := verts (v)(1);         -- copy old vert to new vert array
            overts (ocount)(2) := verts (v)(2);
            overts (ocount)(3) := verts (v)(3);

            for k in 1 .. Integer (Self.m_vertexIndexMapping.Length)
            loop
               if tmpIndices (k) = v then
                  Self.m_vertexIndexMapping (k) := ocount;
               end if;
            end loop;

            ocount := ocount + 1;                       -- increment output vert count
            pragma Assert (       ocount >= 0
                           and then ocount <= vcount);

            usedIndices (v) := ocount;                  -- assign new index remapping
         end if;

      end loop;

   end BringOutYourDead;










   function  CleanupVertices (Self : access HullLibrary;   svcount       : in     Natural;
                                                           svertices     : access vector_3_Array;
                                                           stride        : in     Integer;

                                                           vcount        : access Natural;                    -- output number of vertices
                                                           vertices      : access vector_3_Array;      -- location to store the results

                                                           normalepsilon : in     math.Real;
                              scale         : access math.Vector_3) return Boolean
   is
      pragma Unreferenced (stride);
      EPSILON : constant := 0.000001;    -- close enough to consider two impact.d3.Scalaring point numbers to be 'the same'.
      FLT_MAX : constant := math.Real'Last;

      recip   : Math.Vector_3;

      bmin, bmax : math.Vector_3;
      center     : math.Vector_3;

      dx, dy, dz : math.Real;

   begin
      if svcount = 0 then
         return False;
      end if;

      Self.m_vertexIndexMapping.set_Length (0);

      vcount.all := 0;
      recip      := (0.0, 0.0, 0.0);

      if scale /= null then
         scale (1) := 1.0;
         scale (2) := 1.0;
         scale (3) := 1.0;
      end if;

      bmin := (FLT_MAX,  FLT_MAX,  FLT_MAX);
      bmax := (-FLT_MAX, -FLT_MAX, -FLT_MAX);

      --          const char *vtx = (const char *) svertices;

      for i in 1 .. svcount
      loop
         declare
            --                          const impact.d3.Scalar *p = (const impact.d3.Scalar *) vtx;
            p : math.Vector_3 renames svertices (i);
         begin
            --                          vtx := vtx + stride;

            for j in 1 .. 3
            loop
               if p (j) < bmin (j) then   bmin (j) := p (j);   end if;
               if p (j) > bmax (j) then   bmax (j) := p (j);   end if;
            end loop;
         end;
      end loop;


      dx         := bmax (1) - bmin (1);
      dy         := bmax (2) - bmin (2);
      dz         := bmax (3) - bmin (3);

      center (1) := dx * 0.5 + bmin (1);
      center (2) := dy * 0.5 + bmin (2);
      center (3) := dz * 0.5 + bmin (3);


      if dx < EPSILON or else dy < EPSILON or else dz < EPSILON or else svcount < 3
      then
         declare
            len    : math.Real := FLT_MAX;

            x1, x2,
            y1, y2,
            z1, z2 : math.Real;

         begin
            if dx > EPSILON and then dx < len then   len := dx;   end if;
            if dy > EPSILON and then dy < len then   len := dy;   end if;
            if dz > EPSILON and then dz < len then   len := dz;   end if;

            if len = FLT_MAX then
               dx := 0.01;          -- one centimeter
               dy := 0.01;
               dz := 0.01;
            else
               if dx < EPSILON then   dx := len * 0.05;   end if;            -- 1/5th the shortest non-zero edge.
               if dy < EPSILON then   dy := len * 0.05;   end if;
               if dz < EPSILON then   dz := len * 0.05;   end if;
            end if;

            x1 := center (1) - dx;
            x2 := center (1) + dx;

            y1 := center (2) - dy;
            y2 := center (2) + dy;

            z1 := center (3) - dz;
            z2 := center (3) + dz;

            addPoint (vcount.all, vertices,  x1, y1, z1);
            addPoint (vcount.all, vertices,  x2, y1, z1);
            addPoint (vcount.all, vertices,  x2, y2, z1);
            addPoint (vcount.all, vertices,  x1, y2, z1);
            addPoint (vcount.all, vertices,  x1, y1, z2);
            addPoint (vcount.all, vertices,  x2, y1, z2);
            addPoint (vcount.all, vertices,  x2, y2, z2);
            addPoint (vcount.all, vertices,  x1, y2, z2);

            return True;   -- Return cube
         end;

      else
         if scale /= null then
            scale (1)  := dx;
            scale (2)  := dy;
            scale (3)  := dz;

            recip (1)  := 1.0 / dx;
            recip (2)  := 1.0 / dy;
            recip (3)  := 1.0 / dz;

            center (1) := center (1) * recip (1);
            center (2) := center (2) * recip (2);
            center (3) := center (3) * recip (3);
         end if;

      end if;



      --          vtx := (const char *) svertices;

      for i in 1 .. svcount
      loop
         declare
            --                  const impact.d3.Vector *p = (const impact.d3.Vector *)vtx;
            p : math.Vector_3 renames svertices (i);

            --                  vtx+=stride;

            px : math.Real := p (1);
            py : math.Real := p (2);
            pz : math.Real := p (3);

            j  : Integer;
         begin
            if scale /= null then
               px := px * recip (1); -- normalize
               py := py * recip (2);
               pz := pz * recip (3);
            end if;

            j := 1;

            loop
               declare
                  --  XXX might be broken

                  v     : math.Vector_3 renames vertices (j);

                  x     : constant math.Real := v (1);
                  y     : constant math.Real := v (2);
                  z     : constant math.Real := v (3);

                  dx    : constant math.Real := abs (x - px);
                  dy    : constant math.Real := abs (y - py);
                  dz    : math.Real := abs (z - pz);

                  dist1,
                  dist2 : math.Real;

               begin
                  if         dx < normalepsilon
                    and then dy < normalepsilon
                    and then dz < normalepsilon
                  then
                     --  ok, it is close enough to the old one
                     --  now let us see if it is further from the center of the point cloud than the one we already recorded.
                     --  in which case we keep this one instead.

                     dist1 := GetDist ( px,    py,    pz,  center);
                     dist2 := GetDist (v (1), v (2), v (3),  center);

                     if dist1 > dist2 then
                        v (1) := px;
                        v (2) := py;
                        v (3) := pz;
                     end if;

                     exit;
                  end if;

                  exit when j = vcount.all;
                  j := j + 1;
               end;
            end loop;


            if j = vcount.all then
               declare
                  dest : math.Vector_3 renames vertices (vcount.all);
               begin
                  dest (1) := px;
                  dest (2) := py;
                  dest (3) := pz;

                  vcount.all := vcount.all + 1;
               end;
            end if;


            Self.m_vertexIndexMapping.append (j);
         end;
      end loop;



      --  Ok, now make sure we didn't prune so many vertices it is now invalid.
      --

      bmin := (FLT_MAX,  FLT_MAX,  FLT_MAX);
      bmax := (-FLT_MAX, -FLT_MAX, -FLT_MAX);

      for i in 1 .. vcount.all
      loop
         declare
            p : math.Vector_3 renames vertices (i);
         begin
            for j in 1 .. 3
            loop
               if p (j)  <  bmin (j) then   bmin (j) := p (j);   end if;
               if p (j)  >  bmax (j) then   bmax (j) := p (j);   end if;
            end loop;
         end;
      end loop;


      dx := bmax (1) - bmin (1);
      dy := bmax (2) - bmin (2);
      dz := bmax (3) - bmin (3);

      if                dx < EPSILON
        or else         dy < EPSILON
        or else         dz < EPSILON
        or else vcount.all < 3
      then
         declare
            cx  : math.Real := dx * 0.5  +  bmin (1);
            cy  : math.Real := dy * 0.5  +  bmin (2);
            cz  : math.Real := dz * 0.5  +  bmin (3);

            len : math.Real := FLT_MAX;

            x1, y1, z1 : math.Real;
            x2, y2, z2 : math.Real;

         begin
            if dx >= EPSILON and then dx < len then   len := dx;   end if;
            if dy >= EPSILON and then dy < len then   len := dy;   end if;
            if dz >= EPSILON and then dz < len then   len := dz;   end if;

            if len = FLT_MAX then
               dx := 0.01;   -- one centimeter
               dy := 0.01;
               dz := 0.01;
            else
               if dx < EPSILON then   dx := len * 0.05;   end if;     -- 1/5th the shortest non-zero edge.
               if dy < EPSILON then   dy := len * 0.05;   end if;
               if dz < EPSILON then   dz := len * 0.05;   end if;
            end if;

            x1 := cx - dx;
            x2 := cx + dx;

            y1 := cy - dy;
            y2 := cy + dy;

            z1 := cz - dz;
            z2 := cz + dz;

            vcount.all := 0;    -- add box

            addPoint (vcount.all, vertices,  x1, y1, z1);
            addPoint (vcount.all, vertices,  x2, y1, z1);
            addPoint (vcount.all, vertices,  x2, y2, z1);
            addPoint (vcount.all, vertices,  x1, y2, z1);
            addPoint (vcount.all, vertices,  x1, y1, z2);
            addPoint (vcount.all, vertices,  x2, y1, z2);
            addPoint (vcount.all, vertices,  x2, y2, z2);
            addPoint (vcount.all, vertices,  x1, y2, z2);

            return True;
         end;
      end if;


      return True;
   end CleanupVertices;




end impact.d3.convex_Hull;
