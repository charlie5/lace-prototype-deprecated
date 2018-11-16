with impact.d3.Vector;
with impact.d3.graham_scan_2d_convex_Hull;

with impact.d3.convex_hull_Computer,
     impact.d3.aabb_Util,
     ada.Unchecked_Deallocation;
with Ada.Containers;
with impact.d3.Containers;
with impact.d3.Quaternions;
with impact.d3.Transform;



package body impact.d3.Shape.convex.internal.polyhedral
is



   procedure free is new ada.Unchecked_Deallocation (impact.d3.convex_Polyhedron.item'Class, Polyhedron_view);



   overriding procedure destruct (Self : in out Item)
   is
   begin
      free (self.m_polyhedron);
   end destruct;




   function getConvexPolyhedron (Self : in Item'Class) return access impact.d3.convex_Polyhedron.item'Class
   is
   begin
      return Self.m_polyhedron;
   end getConvexPolyhedron;






   function initializePolyhedralFeatures (Self : access Item'Class) return Boolean
   is
      orgVertices : c_Vector_3_array (1 .. Self.getNumVertices);

      conv   : aliased impact.d3.convex_hull_Computer.item;
      unused : math.Real;
      pragma Unreferenced (unused);

   begin
      free (self.m_polyhedron);
      Self.m_polyhedron := new impact.d3.convex_Polyhedron.item;


      for i in orgVertices'Range loop
         declare
            the_Vertex : math.Vector_3;
         begin
            Self.getVertex (i,  the_Vertex);
            orgVertices (i) := c_Vector_3 (the_Vertex);
         end;
      end loop;

      unused := conv.compute (orgVertices (1)(1)'Access,  math.Vector_3'Size / 8,  orgVertices'Length,  0.0,  0.0);



      declare
         use impact.d3.Vector, ada.Containers;

         type btFace_array is array (Positive range <>) of aliased impact.d3.convex_Polyhedron.btFace;


         numFaces    : constant Integer                       := Integer (conv.faces.Length);
         faceNormals : Vector_3_array (1 .. numFaces);

         convexUtil  : constant access impact.d3.convex_hull_Computer.Item'Class := conv'Access;

         tmpFaces    : btFace_array (1 .. numFaces);


         numVertices : constant Integer := Integer (convexUtil.vertices.Length);

      begin
         Self.m_polyhedron.m_vertices.set_Length (Count_type (numVertices));

         for p in 1 .. numVertices loop
            Self.m_polyhedron.m_vertices.replace_Element (p,  convexUtil.vertices.Element (p));
         end loop;


         for i in 1 .. numFaces loop
            declare   -- compute face normals
               use impact.d3.convex_hull_Computer.edge_Vectors;

               face       : constant Integer := convexUtil.faces.Element (i);                                              -- printf("face=%d\n",face);

--                 firstEdge  : access impact.d3.convex_HullComputer.Edge := convexUtil.edges (face)'access;
--                 edge       : access impact.d3.convex_HullComputer.Edge := firstEdge;
               firstEdge  : constant impact.d3.convex_hull_Computer.Edge_view := convexUtil.edges.Element (face);
               edge       : impact.d3.convex_hull_Computer.Edge_view := firstEdge;

               edges      : Vector_3_array (1 .. 3);
               numEdges   : Integer   :=  0;

               maxCross2  : math.Real := 0.0;
               chosenEdge : Integer   := -1;

               planeEq    : math.Real;
            begin
               loop
                  declare
                     use math.Vectors;
                     use type impact.d3.convex_hull_Computer.Edge_view;

                     src     : constant Integer       := edge.getSourceVertex;
                     targ    : constant Integer       := edge.getTargetVertex;

                     wa      : constant math.Vector_3 := convexUtil.vertices.Element (src);
                     wb      : constant math.Vector_3 := convexUtil.vertices.Element (targ);

                     newEdge : math.Vector_3 := wb - wa;
                  begin
                     tmpFaces (i).m_indices.append (src);
                     normalize (newEdge);

                     if numEdges < 2 then
                        numEdges         := numEdges + 1;
                        edges (numEdges) := newEdge;
                     end if;

                     edge := edge.all.getNextEdgeOfFace;
                     exit when edge = firstEdge;
                  end;
               end loop;

               planeEq := 1.0e30;


               if numEdges = 2 then
                  faceNormals (i) := cross (edges (1),  edges (2));
                  normalize (faceNormals (i));

                  tmpFaces (i).m_plane (1) := faceNormals (i)(1);
                  tmpFaces (i).m_plane (2) := faceNormals (i)(2);
                  tmpFaces (i).m_plane (3) := faceNormals (i)(3);
                  tmpFaces (i).m_plane (4) := planeEq;
               else
                  pragma Assert (False);      -- degenerate ?
                  faceNormals (i) := (0.0, 0.0, 0.0);
               end if;


               for v in 1 .. Integer (tmpFaces (i).m_indices.Length)
               loop
                  declare
                     eq : constant math.Real := dot (Self.m_polyhedron.m_vertices.Element (tmpFaces (i).m_indices.Element (v)),
                                            faceNormals (i));
                  begin
                     if planeEq > eq then
                        planeEq := eq;
                     end if;
                  end;
               end loop;

               tmpFaces (i).m_plane (4) := -planeEq;
            end;
         end loop;



         --  Merge coplanar faces and copy them to m_polyhedron.
         --
         declare

            faceWeldThreshold : math.Real     := 0.999;
            todoFaces         : impact.d3.Containers.Integer_vector;
         begin
            for i in 1 .. tmpFaces'Length loop
               todoFaces.append (i);
            end loop;


            while todoFaces.Length > 0 loop
               declare
                  coplanarFaceGroup :        d3.Containers.Integer_vector;
                  refFace           : constant Integer                   := todoFaces.last_Element;   --  (todoFaces.Length);
                  faceA             : constant access impact.d3.convex_Polyhedron.btFace := tmpFaces (refFace)'Access;
                  faceNormalA       : constant math.Vector_3             := (faceA.m_plane (1),  faceA.m_plane (2),  faceA.m_plane (3));

               begin
                  coplanarFaceGroup.append (refFace);
                  todoFaces.delete_Last;

                  for j in reverse 1 .. Integer (todoFaces.Length)     --   for (int j=todoFaces.size()-1;j>=0;j--)
                  loop
                     declare
                        i           : constant Integer       := todoFaces.Element (j);
                        faceB       : constant access impact.d3.convex_Polyhedron.btFace := tmpFaces (i)'Access;
                        faceNormalB : constant math.Vector_3 := (faceB.m_plane (1),  faceB.m_plane (2),  faceB.m_plane (3));
                     begin
                        if dot (faceNormalA, faceNormalB)  >  faceWeldThreshold then
                           coplanarFaceGroup.append (i);
                           todoFaces.delete (i);
                        end if;
                     end;
                  end loop;


                  if coplanarFaceGroup.Length > 1 then             -- do the merge: use Graham Scan 2d convex hull
                     declare
                        orgpoints     :        impact.d3.graham_scan_2d_convex_Hull.GrahamVector2_Vector;

                        face          : access impact.d3.convex_Polyhedron.btFace;
                        faceNormal    :        math.Vector_3;
                        xyPlaneNormal :        math.Vector_3;

                        rotationArc   :        math.Quaternion;

                     begin
                        for i in 1 .. Integer (coplanarFaceGroup.Length)                                     -- for (int i=0;i<coplanarFaceGroup.size();i++)
                        loop

                           --  // m_polyhedron->m_faces.push_back(tmpFaces[coplanarFaceGroup[i]]);

                           face          := tmpFaces (coplanarFaceGroup.Element (i))'Access;
                           faceNormal    := (face.m_plane (1),  face.m_plane (2),  face.m_plane (3));
                           xyPlaneNormal := (0.0,  0.0,  1.0);

                           rotationArc   := impact.d3.Quaternions.shortestArcQuat (faceNormal, xyPlaneNormal);

                           for f in 1 .. Integer (face.m_indices.Length)                                   -- for (int f=0;f<face.m_indices.size();f++)
                           loop
                              declare
                                 use math.Vectors;

                                 orgIndex  : constant Integer       := face.m_indices.Element (f);
                                 pt        : constant math.Vector_3 := Self.m_polyhedron.m_vertices.Element (orgIndex);
                                 rotatedPt : math.Vector_3 := impact.d3.Quaternions.quatRotate (rotationArc, pt);
                                 found : Boolean := False;

                              begin
                                 rotatedPt (3) := 0.0;

                                 for i in 1 .. Integer (orgpoints.Length)                                 -- for (int i=0;i<orgpoints.size();i++)
                                 loop
                                    if length2 (rotatedPt - orgpoints.Element (i).Vector)  <  0.001 then
                                       found := True;
                                       exit;
                                    end if;
                                 end loop;

                                 if not found then
                                    orgpoints.append (impact.d3.graham_scan_2d_convex_Hull.to_GrahamVector2 (rotatedPt, orgIndex));
                                 end if;
                              end;
                           end loop;
                        end loop;

                        declare
                           combinedFace : impact.d3.convex_Polyhedron.btFace;
                        begin
                           for i in 1 .. 4 loop
                              combinedFace.m_plane (i) := tmpFaces (coplanarFaceGroup.Element (1)).m_plane (i);
                           end loop;


                           declare
                              hull : impact.d3.graham_scan_2d_convex_Hull.GrahamVector2_Vector;
                           begin
                              impact.d3.graham_scan_2d_convex_Hull.GrahamScanConvexHull2D (orgpoints, hull);

                              for i in 1 .. Integer (hull.Length)                                        -- for (int i=0;i<hull.size();i++)
                              loop
                                 combinedFace.m_indices.append (hull.Element (i).m_orgIndex);
                              end loop;

                              Self.m_polyhedron.m_faces.append (combinedFace);
                           end;
                        end;
                     end;

                  else
                     for i in 1 .. Integer (coplanarFaceGroup.Length) loop                       --  for (int i=0;i<coplanarFaceGroup.size();i++)
                        Self.m_polyhedron.m_faces.append (tmpFaces (coplanarFaceGroup.Element (i)));
                     end loop;
                  end if;

               end;
            end loop;
         end;

         Self.m_polyhedron.initialize;

         return True;
      end;


   end initializePolyhedralFeatures;



   --- Below C code for 'initializePolyhedralFeatures' is ported but left here temporarily for reference.
   --

   --  bool        impact.d3.Shape.convex.internal.polyhedral::initializePolyhedralFeatures()
   --  {
   --
   --          if (m_polyhedron)
   --                  btAlignedFree(m_polyhedron);
   --
   --          void* mem = btAlignedAlloc(sizeof(impact.d3.convex_Polyhedron),16);
   --          m_polyhedron = new (mem) impact.d3.convex_Polyhedron;
   --
   --                  btAlignedObjectArray<impact.d3.Vector> orgVertices;
   --
   --          for (int i=0;i<getNumVertices();i++)
   --          {
   --                  impact.d3.Vector& newVertex = orgVertices.expand();
   --                  getVertex(i,newVertex);
   --          }
   --
   --  #if 0
   --          btAlignedObjectArray<impact.d3.Vector> planeEquations;
   --          btGeometryUtil::getPlaneEquationsFromVertices(orgVertices,planeEquations);
   --
   --          btAlignedObjectArray<impact.d3.Vector> shiftedPlaneEquations;
   --          for (int p=0;p<planeEquations.size();p++)
   --          {
   --                     impact.d3.Vector plane = planeEquations[p];
   --                     plane[3] -= getMargin();
   --                     shiftedPlaneEquations.push_back(plane);
   --          }
   --
   --          btAlignedObjectArray<impact.d3.Vector> tmpVertices;
   --
   --          btGeometryUtil::getVerticesFromPlaneEquations(shiftedPlaneEquations,tmpVertices);
   --          impact.d3.convex_HullComputer conv;
   --          conv.compute(&tmpVertices[0].getX(), sizeof(impact.d3.Vector),tmpVertices.size(),0.f,0.f);
   --
   --  #else
   --          impact.d3.convex_HullComputer conv;
   --          conv.compute(&orgVertices[0].getX(), sizeof(impact.d3.Vector),orgVertices.size(),0.f,0.f);
   --
   --  #endif
   --
   --
   --
   --          btAlignedObjectArray<impact.d3.Vector> faceNormals;
   --          int numFaces = conv.faces.size();
   --          faceNormals.resize(numFaces);
   --          impact.d3.convex_HullComputer* convexUtil = &conv;
   --
   --
   --          btAlignedObjectArray<btFace>        tmpFaces;
   --          tmpFaces.resize(numFaces);
   --
   --          int numVertices = convexUtil->vertices.size();
   --          m_polyhedron->m_vertices.resize(numVertices);
   --          for (int p=0;p<numVertices;p++)
   --          {
   --                  m_polyhedron->m_vertices[p] = convexUtil->vertices[p];
   --          }
   --
   --
   --          for (int i=0;i<numFaces;i++)
   --          {
   --                  int face = convexUtil->faces[i];
   --                  //printf("face=%d\n",face);
   --                  const impact.d3.convex_HullComputer::Edge*  firstEdge = &convexUtil->edges[face];
   --                  const impact.d3.convex_HullComputer::Edge*  edge = firstEdge;
   --
   --                  impact.d3.Vector edges[3];
   --                  int numEdges = 0;
   --                  //compute face normals
   --
   --                  impact.d3.Scalar maxCross2 = 0.f;
   --                  int chosenEdge = -1;
   --
   --                  do
   --                  {
   --
   --                          int src = edge->getSourceVertex();
   --                          tmpFaces[i].m_indices.push_back(src);
   --                          int targ = edge->getTargetVertex();
   --                          impact.d3.Vector wa = convexUtil->vertices[src];
   --
   --                          impact.d3.Vector wb = convexUtil->vertices[targ];
   --                          impact.d3.Vector newEdge = wb-wa;
   --                          newEdge.normalize();
   --                          if (numEdges<2)
   --                                  edges[numEdges++] = newEdge;
   --
   --                          edge = edge->getNextEdgeOfFace();
   --                  } while (edge!=firstEdge);
   --
   --                  impact.d3.Scalar planeEq = 1e30f;
   --
   --
   --                  if (numEdges==2)
   --                  {
   --                          faceNormals[i] = edges[0].cross(edges[1]);
   --                          faceNormals[i].normalize();
   --                          tmpFaces[i].m_plane[0] = faceNormals[i].getX();
   --                          tmpFaces[i].m_plane[1] = faceNormals[i].getY();
   --                          tmpFaces[i].m_plane[2] = faceNormals[i].getZ();
   --                          tmpFaces[i].m_plane[3] = planeEq;
   --
   --                  }
   --                  else
   --                  {
   --                          btAssert(0);//degenerate?
   --                          faceNormals[i].setZero();
   --                  }
   --
   --                  for (int v=0;v<tmpFaces[i].m_indices.size();v++)
   --                  {
   --                          impact.d3.Scalar eq = m_polyhedron->m_vertices[tmpFaces[i].m_indices[v]].dot(faceNormals[i]);
   --                          if (planeEq>eq)
   --                          {
   --                                  planeEq=eq;
   --                          }
   --                  }
   --                  tmpFaces[i].m_plane[3] = -planeEq;
   --          }
   --
   --          //merge coplanar faces and copy them to m_polyhedron
   --
   --          impact.d3.Scalar faceWeldThreshold= 0.999f;
   --          btAlignedObjectArray<int> todoFaces;
   --          for (int i=0;i<tmpFaces.size();i++)
   --                  todoFaces.push_back(i);
   --
   --          while (todoFaces.size())
   --          {
   --                  btAlignedObjectArray<int> coplanarFaceGroup;
   --                  int refFace = todoFaces[todoFaces.size()-1];
   --
   --                  coplanarFaceGroup.push_back(refFace);
   --                  btFace& faceA = tmpFaces[refFace];
   --                  todoFaces.pop_back();
   --
   --                  impact.d3.Vector faceNormalA(faceA.m_plane[0],faceA.m_plane[1],faceA.m_plane[2]);
   --                  for (int j=todoFaces.size()-1;j>=0;j--)
   --                  {
   --                          int i = todoFaces[j];
   --                          btFace& faceB = tmpFaces[i];
   --                          impact.d3.Vector faceNormalB(faceB.m_plane[0],faceB.m_plane[1],faceB.m_plane[2]);
   --                          if (faceNormalA.dot(faceNormalB)>faceWeldThreshold)
   --                          {
   --                                  coplanarFaceGroup.push_back(i);
   --                                  todoFaces.remove(i);
   --                          }
   --                  }
   --
   --
   --                  if (coplanarFaceGroup.size()>1)
   --                  {
   --                          //do the merge: use Graham Scan 2d convex hull
   --
   --                          btAlignedObjectArray<GrahamVector2> orgpoints;
   --
   --                          for (int i=0;i<coplanarFaceGroup.size();i++)
   --                          {
   --  //                                m_polyhedron->m_faces.push_back(tmpFaces[coplanarFaceGroup[i]]);
   --
   --                                  btFace& face = tmpFaces[coplanarFaceGroup[i]];
   --                                  impact.d3.Vector faceNormal(face.m_plane[0],face.m_plane[1],face.m_plane[2]);
   --                                  impact.d3.Vector xyPlaneNormal(0,0,1);
   --
   --                                  impact.d3.Quaternion rotationArc = shortestArcQuat(faceNormal,xyPlaneNormal);
   --
   --                                  for (int f=0;f<face.m_indices.size();f++)
   --                                  {
   --                                          int orgIndex = face.m_indices[f];
   --                                          impact.d3.Vector pt = m_polyhedron->m_vertices[orgIndex];
   --                                          impact.d3.Vector rotatedPt =  quatRotate(rotationArc,pt);
   --                                          rotatedPt.setZ(0);
   --                                          bool found = false;
   --
   --                                          for (int i=0;i<orgpoints.size();i++)
   --                                          {
   --                                                  if ((rotatedPt-orgpoints[i]).length2()<0.001)
   --                                                  {
   --                                                          found=true;
   --                                                          break;
   --                                                  }
   --                                          }
   --                                          if (!found)
   --                                                  orgpoints.push_back(GrahamVector2(rotatedPt,orgIndex));
   --                                  }
   --                          }
   --
   --                          btFace combinedFace;
   --                          for (int i=0;i<4;i++)
   --                                  combinedFace.m_plane[i] = tmpFaces[coplanarFaceGroup[0]].m_plane[i];
   --
   --                          btAlignedObjectArray<GrahamVector2> hull;
   --                          GrahamScanConvexHull2D(orgpoints,hull);
   --
   --                          for (int i=0;i<hull.size();i++)
   --                          {
   --                                  combinedFace.m_indices.push_back(hull[i].m_orgIndex);
   --                          }
   --                          m_polyhedron->m_faces.push_back(combinedFace);
   --                  } else
   --                  {
   --                          for (int i=0;i<coplanarFaceGroup.size();i++)
   --                          {
   --                                  m_polyhedron->m_faces.push_back(tmpFaces[coplanarFaceGroup[i]]);
   --                          }
   --
   --                  }
   --
   --
   --
   --          }
   --
   --          m_polyhedron->initialize();
   --
   --          return true;
   --  }







   overriding function localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, math.Functions, math.Vectors;

      supVec  : math.Vector_3 :=  math.Origin_3d;
      maxDot  : math.Real     := -BT_LARGE_FLOAT;

      the_vec : math.Vector_3 := vec;
      lenSqr  : constant math.Real     := length2 (the_vec);

      rlen    : math.Real;

      vtx     : math.Vector_3;
      newDot  : math.Real;

   begin
      if lenSqr < 0.0001 then
         the_vec  := (1.0, 0.0, 0.0);
      else
         rlen     := 1.0 / sqRt (lenSqr);
         the_vec  := the_vec * rlen;
      end if;


      for i in 1 .. Item'Class (Self).getNumVertices
      loop
         Item'Class (Self).getVertex (i, vtx);

         newDot := dot (the_vec, vtx);

         if newDot > maxDot then
            maxDot := newDot;
            supVec := vtx;
         end if;
      end loop;


      return supVec;
   end localGetSupportingVertexWithoutMargin;






   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                numVectors         : in     Integer)
   is
      vtx    : math.Vector_3;
      newDot : math.Real;

   begin

      for i in 1 .. numVectors
      loop
         supportVerticesOut (i) (4) := -BT_LARGE_FLOAT;
      end loop;


      for j in 1 .. numVectors
      loop
         declare
            use impact.d3.Vector;
            vec : math.Vector_3 renames vectors (j);

         begin
            for i in 1 .. Item'Class (Self).getNumVertices
            loop
               Item'Class (Self).getVertex (i, vtx);
               newDot := dot (vec, vtx);

               if newDot > supportVerticesOut (j)(4) then
                  supportVerticesOut (j)    := vtx;     -- WARNING: don't swap these lines. The w component would get overwritten !
                  supportVerticesOut (j)(4) := newDot;  --
               end if;
            end loop;
         end;
      end loop;

   end batchedUnitVectorGetSupportingVertexWithoutMargin;








   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      --  Not yet, return box inertia.

      margin  : constant math.Real        := Self.getMargin;
      ident   : constant Transform_3d := impact.d3.Transform.getIdentity;

      aabbMin,
      aabbMax : math.Vector_3;

   begin
      Self.getAabb (ident,  aabbMin, aabbMax);

      declare
         use math.Vectors;

         halfExtents :          math.Vector_3 := (aabbMax - aabbMin) * 0.5;

         lx          : constant math.Real     := 2.0 * (halfExtents (1) + margin);
         ly          : constant math.Real     := 2.0 * (halfExtents (2) + margin);
         lz          : constant math.Real     := 2.0 * (halfExtents (3) + margin);

         x2          : constant math.Real     :=  lx * lx;
         y2          : constant math.Real     :=  ly * ly;
         z2          : constant math.Real     :=  lz * lz;

         scaledmass  : constant math.Real     := mass * 0.08333333;

      begin
         inertia := scaledmass * (y2 + z2,  x2 + z2,  x2 + y2);
      end;
   end calculateLocalInertia;






   overriding procedure setLocalScaling   (Self : in out btPolyhedralConvexAabbCachingShape;   scaling : in math.Vector_3)
   is
   begin
      impact.d3.Shape.convex.internal.item (Self).setLocalScaling (scaling);   -- Call base class.
      Self.recalcLocalAabb;
   end setLocalScaling;





   overriding procedure getAabb (Self : in     btPolyhedralConvexAabbCachingShape;         t                : in     Transform_3d;
                      aabbMin, aabbMax :    out math.Vector_3)
   is
   begin
      Self.getNonvirtualAabb (t,  aabbMin, aabbMax,  Self.getMargin);
   end getAabb;




   procedure recalcLocalAabb   (Self : in out btPolyhedralConvexAabbCachingShape'Class)
   is
      directions : constant vector_3_Array := ((1.0,  0.0,  0.0),
                                               (0.0,  1.0,  0.0),
                                               (0.0,  0.0,  1.0),
                                               (-1.0,  0.0,  0.0),
                                               (0.0, -1.0,  0.0),
                                               (0.0,  0.0, -1.0));

      supporting :          vector_3_Array := ((0.0, 0.0, 0.0),
                                               (0.0, 0.0, 0.0),
                                               (0.0, 0.0, 0.0),
                                               (0.0, 0.0, 0.0),
                                               (0.0, 0.0, 0.0),
                                               (0.0, 0.0, 0.0));
   begin
      Self.m_isLocalAabbValid := True;

      Self.batchedUnitVectorGetSupportingVertexWithoutMargin (directions, supporting, 6);

      for i in 1 .. 3
      loop
         Self.m_localAabbMax (i) := supporting (i)(i) + Self.getMargin;
         Self.m_localAabbMin (i) := supporting (i + 3)(i) - Self.getMargin;
      end loop;
   end recalcLocalAabb;






   --- btPolyhedralConvexAabbCachingShape
   --



   procedure setCachedLocalAabb (Self : in out btPolyhedralConvexAabbCachingShape'Class;   aabbMin, aabbMax : in math.Vector_3)
   is
   begin
      Self.m_isLocalAabbValid := True;
      Self.m_localAabbMin     := aabbMin;
      Self.m_localAabbMax     := aabbMax;
   end setCachedLocalAabb;





   procedure getCachedLocalAabb (Self : in     btPolyhedralConvexAabbCachingShape'Class;   aabbMin, aabbMax : out math.Vector_3)
   is
   begin
      pragma Assert (Self.m_isLocalAabbValid);

      aabbMin := Self.m_localAabbMin;
      aabbMax := Self.m_localAabbMax;
   end getCachedLocalAabb;





   procedure getNonvirtualAabb (Self : in     btPolyhedralConvexAabbCachingShape'Class;   trans            : in     Transform_3d;
                                                                                          aabbMin, aabbMax :    out math.Vector_3;
                                margin           : in     math.Real)
   is
      pragma Assert (Self.m_isLocalAabbValid);

      use impact.d3.aabb_Util;
   begin
      Transform_Aabb (Self.m_localAabbMin, Self.m_localAabbMax,  margin, trans,  aabbMin, aabbMax);         -- lazy evaluation of local aabb
   end getNonvirtualAabb;



end impact.d3.Shape.convex.internal.polyhedral;
