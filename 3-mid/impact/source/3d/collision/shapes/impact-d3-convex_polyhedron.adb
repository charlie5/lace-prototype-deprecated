with Impact.d3.Vector, Ada.Containers.Hashed_Maps;

package body Impact.d3.convex_Polyhedron
   --
   --  Separating axis rest based on work from Pierre Terdiman, see
   --
   --  Contact clipping based on work from Simon Hobbs
   --
is

   function IsAlmostZero (v : in Math.Vector_3) return Boolean is
   begin
      if abs (v (1)) > 1.0e-6
        or else abs (v (2)) > 1.0e-6
        or else abs (v (3)) > 1.0e-6
      then
         return False;
      end if;

      return True;
   end IsAlmostZero;

   procedure swap (Left, Right : in out Short_Integer) is
      Pad : constant Short_Integer := Left;
   begin
      Left  := Right;
      Right := Pad;
   end swap;

   type btInternalVertexPair is record
      m_v0 : Short_Integer;
      m_v1 : Short_Integer;
   end record;

   function to_btInternalVertexPair
     (v0, v1 : in Short_Integer)
      return   btInternalVertexPair
   is
      Self : btInternalVertexPair;
   begin
      Self.m_v0 := v0;
      Self.m_v1 := v1;

      if Self.m_v1 > Self.m_v0 then
         swap (Self.m_v0, Self.m_v1);
      end if;

      return Self;
   end to_btInternalVertexPair;

   function getHash
     (Self : in btInternalVertexPair)
      return Ada.Containers.Hash_Type
   is
      use Ada.Containers;

      v0 : constant Hash_Type := Hash_Type (Self.m_v0);
      v1 : constant Hash_Type := Hash_Type (Self.m_v1);

   begin
      return v0 + (v1 * 2 ** 16);
   end getHash;

   function equals
     (Self  : in btInternalVertexPair;
      other : in btInternalVertexPair)
      return  Boolean
   is
   begin
      return Self.m_v0 = other.m_v0 and then Self.m_v1 = other.m_v1;
   end equals;

   type btInternalEdge is record
      m_face0, m_face1 : Short_Integer := -1;
   end record;

   package btInternalVertexPair_Maps_of_btInternalEdge is new
     Ada.Containers.Hashed_Maps (
      btInternalVertexPair,
      btInternalEdge,
      getHash,
      equals);

   subtype btInternalVertexPair_Map_of_btInternalEdge is
     btInternalVertexPair_Maps_of_btInternalEdge.Map;

   --          btHashMap<btInternalVertexPair,btInternalEdge> edges;

   procedure initialize (Self : in out Item) is
      use btInternalVertexPair_Maps_of_btInternalEdge, Impact.d3.Vector;

      edges     : btInternalVertexPair_Map_of_btInternalEdge;
      TotalArea : Math.Real := 0.0;

      numVertices : Integer;
      NbTris      : Integer;

      k  : Integer;
      vp : btInternalVertexPair;
      --        edptr       : access btInternalEdge;
      edptr : btInternalVertexPair_Maps_of_btInternalEdge.Cursor;
      edge  : Math.Vector_3;

      found : Boolean;

      ed : btInternalEdge;
   begin
      Self.m_localCenter := (0.0, 0.0, 0.0);

      for i in 1 .. Integer (Self.m_faces.Length) loop
         numVertices := Integer (Self.m_faces.Element (i).m_indices.Length);
         NbTris      := numVertices;

         for j in 1 .. NbTris loop
            k     := (j + 1) mod numVertices;
            vp    :=
              (Short_Integer (Self.m_faces.Element (i).m_indices.Element
                                 (j)),
               Short_Integer (Self.m_faces.Element (i).m_indices.Element
                                 (k)));
            edptr := edges.Find (vp);
            edge  := Self.m_vertices.Element (Integer (vp.m_v1)) -
                     Self.m_vertices.Element (Integer (vp.m_v0));
            Normalize (edge);

            found := False;

            for p in 1 .. Self.m_uniqueEdges.Length loop
               if IsAlmostZero
                     (Self.m_uniqueEdges.Element (Integer (p)) - edge)
                 or else IsAlmostZero
                            (Self.m_uniqueEdges.Element (Integer (p)) +
                             edge)
               then
                  found := True;
                  exit;
               end if;
            end loop;

            if not found then
               Self.m_uniqueEdges.Append (edge);
            end if;

            if Has_Element (edptr) then    --  /= null then
               pragma Assert (Element (edptr).m_face0 >= 0);
               pragma Assert (Element (edptr).m_face1 < 0);

               declare
                  the_Edge : btInternalEdge := Element (edptr);
               begin
                  the_Edge.m_face1 := Short_Integer (i);
                  edges.Replace_Element (edptr, the_Edge);
               end;
            else
               ed.m_face0 := Short_Integer (i);
               edges.Insert (vp, ed);
            end if;

         end loop;
      end loop;

      --  #ifdef USE_CONNECTED_FACES
      --          for(int i=0;i<m_faces.size();i++)
      --          {
      --                  int numVertices = m_faces[i].m_indices.size();
      --                  m_faces[i].m_connectedFaces.resize(numVertices);
      --
      --                  for(int j=0;j<numVertices;j++)
      --                  {
      --                          int k = (j+1)%numVertices;
      --                          btInternalVertexPair
      --vp(m_faces[i].m_indices[j],m_faces[i].m_indices[k]);
      --                          btInternalEdge* edptr = edges.find(vp);
      --                          btAssert(edptr);
      --                          btAssert(edptr->m_face0>=0);
      --                          btAssert(edptr->m_face1>=0);
      --
      --                          int connectedFace =
      --(edptr->m_face0==i)?edptr->m_face1:edptr->m_face0;
      --                          m_faces[i].m_connectedFaces[j] =
      --connectedFace;
      --                  }
      --          }
      --  #endif  -- USE_CONNECTED_FACES

      for i in 1 .. Self.m_faces.Length loop
         declare
            numVertices : constant Integer :=
              Integer (Self.m_faces.Element (Integer (i)).m_indices.Length);
            NbTris      : constant Integer := numVertices - 2;

            p0 : Math.Vector_3 renames Self.m_vertices.Element
                                         (Self.m_faces.Element
                                             (Integer (i)).m_indices.Element
                                             (1));
         begin

            for j in 2 .. NbTris loop
               declare
                  k : constant Integer := (j + 1) mod numVertices;

                  p1 : Math.Vector_3 renames Self.m_vertices.Element
                                               (Self.m_faces.Element
                                                   (Integer (i)).m_indices.
                    Element
                                                   (j));
                  p2 : Math.Vector_3 renames Self.m_vertices.Element
                                               (Self.m_faces.Element
                                                   (Integer (i)).m_indices.
                    Element
                                                   (k));

                  Area   : constant Math.Real     :=
                    length (cross (p0 - p1, p0 - p2)) * 0.5;
                  Center : constant Math.Vector_3 := (p0 + p1 + p2) / 3.0;
               begin
                  Self.m_localCenter := Self.m_localCenter + Area * Center;
                  TotalArea          := TotalArea + Area;
               end;
            end loop;

         end;
      end loop;

      Self.m_localCenter := Self.m_localCenter / TotalArea;

      --  #ifdef TEST_INTERNAL_OBJECTS
      --          if(1)
      --          {
      --                  m_radius = FLT_MAX;
      --                  for(int i=0;i<m_faces.size();i++)
      --                  {
      --                          const impact.d3.Vector
      --Normal(m_faces[i].m_plane[0], m_faces[i].m_plane[1],
      --m_faces[i].m_plane[2]);
      --                          const impact.d3.Scalar dist =
      --btFabs(m_localCenter.dot(Normal) + m_faces[i].m_plane[3]);
      --                          if(dist<m_radius)
      --                                  m_radius = dist;
      --                  }
      --
      --
      --                  impact.d3.Scalar MinX = FLT_MAX;
      --                  impact.d3.Scalar MinY = FLT_MAX;
      --                  impact.d3.Scalar MinZ = FLT_MAX;
      --                  impact.d3.Scalar MaxX = -FLT_MAX;
      --                  impact.d3.Scalar MaxY = -FLT_MAX;
      --                  impact.d3.Scalar MaxZ = -FLT_MAX;
      --                  for(int i=0; i<m_vertices.size(); i++)
      --                  {
      --                          const impact.d3.Vector& pt = m_vertices[i];
      --                          if(pt.x()<MinX)        MinX = pt.x();
      --                          if(pt.x()>MaxX)        MaxX = pt.x();
      --                          if(pt.y()<MinY)        MinY = pt.y();
      --                          if(pt.y()>MaxY)        MaxY = pt.y();
      --                          if(pt.z()<MinZ)        MinZ = pt.z();
      --                          if(pt.z()>MaxZ)        MaxZ = pt.z();
      --                  }
      --                  mC.setValue(MaxX+MinX, MaxY+MinY, MaxZ+MinZ);
      --                  mE.setValue(MaxX-MinX, MaxY-MinY, MaxZ-MinZ);
      --
      --
      --
      --  --                const impact.d3.Scalar r = m_radius / sqrtf(2.0f);
      --                  const impact.d3.Scalar r = m_radius / sqrtf(3.0f);
      --                  const int LargestExtent = mE.maxAxis();
      --                  const impact.d3.Scalar Step =
      --(mE[LargestExtent]*0.5f - r)/1024.0f;
      --                  m_extents[0] = m_extents[1] = m_extents[2] = r;
      --                  m_extents[LargestExtent] = mE[LargestExtent]*0.5f;
      --                  bool FoundBox = false;
      --                  for(int j=0;j<1024;j++)
      --                  {
      --                          if(testContainment())
      --                          {
      --                                  FoundBox = true;
      --                                  break;
      --                          }
      --
      --                          m_extents[LargestExtent] -= Step;
      --                  }
      --                  if(!FoundBox)
      --                  {
      --                          m_extents[0] = m_extents[1] = m_extents[2] =
      --r;
      --                  }
      --                  else
      --                  {
      --                          -- Refine the box
      --                          const impact.d3.Scalar Step = (m_radius -
      --r)/1024.0f;
      --                          const int e0 = (1<<LargestExtent) & 3;
      --                          const int e1 = (1<<e0) & 3;
      --
      --                          for(int j=0;j<1024;j++)
      --                          {
      --                                  const impact.d3.Scalar Saved0 =
      --m_extents[e0];
      --                                  const impact.d3.Scalar Saved1 =
      --m_extents[e1];
      --                                  m_extents[e0] += Step;
      --                                  m_extents[e1] += Step;
      --
      --                                  if(!testContainment())
      --                                  {
      --                                          m_extents[e0] = Saved0;
      --                                          m_extents[e1] = Saved1;
      --                                          break;
      --                                  }
      --                          }
      --                  }
      --          }
      --  #endif

   end initialize;

   function testContainment (Self : in Item) return Boolean is
   begin
      raise Program_Error;   -- tbd
      return False;
   end testContainment;

   --  #ifdef TEST_INTERNAL_OBJECTS
   --  bool impact.d3.convex_Polyhedron::testContainment() const
   --  {
   --          for(int p=0;p<8;p++)
   --          {
   --                  impact.d3.Vector LocalPt;
   --                  if(p==0)                LocalPt = m_localCenter +
   --impact.d3.Vector(m_extents[0], m_extents[1], m_extents[2]);
   --                  else if(p==1)        LocalPt = m_localCenter +
   --impact.d3.Vector(m_extents[0], m_extents[1], -m_extents[2]);
   --                  else if(p==2)        LocalPt = m_localCenter +
   --impact.d3.Vector(m_extents[0], -m_extents[1], m_extents[2]);
   --                  else if(p==3)        LocalPt = m_localCenter +
   --impact.d3.Vector(m_extents[0], -m_extents[1], -m_extents[2]);
   --                  else if(p==4)        LocalPt = m_localCenter +
   --impact.d3.Vector(-m_extents[0], m_extents[1], m_extents[2]);
   --                  else if(p==5)        LocalPt = m_localCenter +
   --impact.d3.Vector(-m_extents[0], m_extents[1], -m_extents[2]);
   --                  else if(p==6)        LocalPt = m_localCenter +
   --impact.d3.Vector(-m_extents[0], -m_extents[1], m_extents[2]);
   --                  else if(p==7)        LocalPt = m_localCenter +
   --impact.d3.Vector(-m_extents[0], -m_extents[1], -m_extents[2]);
   --
   --                  for(int i=0;i<m_faces.size();i++)
   --                  {
   --                          const impact.d3.Vector
   --Normal(m_faces[i].m_plane[0], m_faces[i].m_plane[1],
   --m_faces[i].m_plane[2]);
   --                          const impact.d3.Scalar d = LocalPt.dot(Normal)
   --+ m_faces[i].m_plane[3];
   --                          if(d>0.0f)
   --                                  return false;
   --                  }
   --          }
   --          return true;
   --  }
   --  #endif

   procedure project
     (Self     : in Item;
      trans    : in Transform_3d;
      dir      : in Math.Vector_3;
      min, max : out Math.Real)
   is
      use linear_Algebra_3d, Impact.d3.Vector, Math.Vectors;

      numVerts : Integer;
      pt       : Math.Vector_3;
      dp       : Math.Real;
      tmp      : Math.Real;

   begin
      min := Math.Infinity;
      max := -Math.Infinity;

      numVerts := Integer (Self.m_vertices.Length);

      for i in 1 .. numVerts loop
         pt := trans * Self.m_vertices.Element (i);
         dp := dot (pt, dir);

         if dp < min then
            min := dp;
         end if;
         if dp > max then
            max := dp;
         end if;
      end loop;

      if min > max then
         tmp := min;
         min := max;
         max := tmp;
      end if;
   end project;

end Impact.d3.convex_Polyhedron;
