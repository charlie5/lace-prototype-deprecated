--  #include "impact.d3.collision.polyhedral_contact_Clipping.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.convex_Polyhedron.h"
--
--  #include <float.h> //for FLT_MAX

with Impact.d3.Vector;
with Impact.d3.Transform;

package body Impact.d3.Collision.polyhedral_contact_Clipping
   --
   --  Separating axis rest based on work from Pierre Terdiman
   --  Contact clipping     based on work from Simon Hobbs
   --
is

   ------------
   --- Globals
   --

   gExpectedNbTests   : Integer := 0;
   gActualNbTests     : Integer := 0;
   gUseInternalObject : Boolean := True;

   gActualSATPairTests : Integer := 0;

   ------------
   --- Utility
   --

   function TestSepAxis
     (hullA, hullB   : in Impact.d3.convex_Polyhedron.Item'Class;
      transA, transB : in Transform_3d;
      sep_axis       : in Math.Vector_3;
      depth          : out Math.Real)
      return           Boolean
   is
      Min0, Max0, Min1, Max1, d0, d1 : Math.Real;

   begin
      hullA.project (transA, sep_axis, Min0, Max0);
      hullB.project (transB, sep_axis, Min1, Max1);

      if Max0 < Min1 or else Max1 < Min0 then
         return False;
      end if;

      d0 := Max0 - Min1;
      pragma Assert (d0 >= 0.0);
      d1 := Max1 - Min0;
      pragma Assert (d1 >= 0.0);

      if d0 < d1 then
         depth := d0;
      else
         depth := d1;
      end if;

      return True;
   end TestSepAxis;

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

   ---------------
   --- Operations
   --

   procedure clipHullAgainstHull
     (separatingNormal1 : in Math.Vector_3;
      hullA, hullB      : in Impact.d3.convex_Polyhedron.Item'Class;
      transA, transB    : in Transform_3d;
      minDist, maxDist  : in Math.Real;
      resultOut         : out Impact.d3.Collision.Detector.discrete.Result'
        Class)
   is
      use linear_Algebra_3d, Impact.d3.Vector, Math.Vectors,
        Impact.d3.Transform;

      separatingNormal : constant Math.Vector_3 :=
        Normalized (separatingNormal1);

      c0 : constant Math.Vector_3 := transA * hullA.m_localCenter;
      c1 : constant Math.Vector_3 := transB * hullB.m_localCenter;

      DeltaC2 : constant Math.Vector_3 := c0 - c1;

      curMaxDist   : Math.Real := maxDist;
      closestFaceB : Integer   := -1;
      dmax         : Math.Real := -Math.Real'Last;
      worldVertsB1 : btVertexArray;

   begin
      for face in 1 .. Integer (hullB.m_faces.Length) loop
         declare
            Normal      : constant Math.Vector_3 :=
              (hullB.m_faces.Element (face).m_plane (1),
               hullB.m_faces.Element (face).m_plane (2),
               hullB.m_faces.Element (face).m_plane (3));
            WorldNormal : constant Math.Vector_3 :=
              getBasis (transB) * Normal;
            d           : constant Math.Real     :=
              dot (WorldNormal, separatingNormal);
         begin
            if d > dmax then
               dmax         := d;
               closestFaceB := face;
            end if;
         end;
      end loop;

      declare
         polyB       : Impact.d3.convex_Polyhedron.btFace renames
           hullB.m_faces.Element(closestFaceB);
         numVertices : constant Integer := Integer (polyB.m_indices.Length);
      begin
         for e0 in 1 .. numVertices loop
            declare
               b : constant Math.Vector_3 :=
                 hullB.m_vertices.Element (polyB.m_indices.Element (e0));
            begin
               worldVertsB1.Append (transB * b);
            end;
         end loop;
      end;

      if closestFaceB >= 0 then
         clipFaceAgainstHull
           (separatingNormal,
            hullA,
            transA,
            worldVertsB1,
            minDist,
            maxDist,
            resultOut);
      end if;

   end clipHullAgainstHull;

   procedure clipFaceAgainstHull
     (separatingNormal : in Math.Vector_3;
      hullA            : in Impact.d3.convex_Polyhedron.Item'Class;
      transA           : in Transform_3d;
      worldVertsB1     : in out btVertexArray;
      minDist, maxDist : in Math.Real;
      resultOut        : out Impact.d3.Collision.Detector.discrete.Result'
        Class)
   is
      use Impact.d3.Transform, Impact.d3.Vector, Math.Vectors;

      worldVertsB2 : aliased btVertexArray;
      pVtxIn       : access btVertexArray := worldVertsB1'Access;
      pVtxOut      : access btVertexArray := worldVertsB2'Access;

      closestFaceA : Integer   := -1;
      dmin         : Math.Real := Math.Real'Last;

   begin
      pVtxOut.Reserve_Capacity (pVtxIn.Length);

      for face in 1 .. Integer (hullA.m_faces.Length) loop
         declare
            Normal : constant Math.Vector_3 :=
              (hullA.m_faces.Element (face).m_plane (1),
               hullA.m_faces.Element (face).m_plane (2),
               hullA.m_faces.Element (face).m_plane (3));

            faceANormalWS : constant Math.Vector_3 :=
              getBasis (transA) * Normal;

            d : constant Math.Real := dot (faceANormalWS, separatingNormal);
         begin
            if d < dmin then
               dmin         := d;
               closestFaceA := face;
            end if;
         end;
      end loop;

      if closestFaceA < 0 then
         return;
      end if;

      --  clip polygon to back of planes of all faces of hull A that are
      --adjacent to witness face
      --
      declare
         use Impact.d3.convex_Polyhedron.btFace_Vectors;
         polyA : constant Impact.d3.convex_Polyhedron.btFace :=
           hullA.m_faces.Element (closestFaceA);

         numContacts  : Integer          := Integer (pVtxIn.Length);
         numVerticesA : constant Integer := Integer (polyA.m_indices.Length);
      begin
         for e0 in 1 .. numVerticesA loop
            declare
               use linear_Algebra_3d,
                 Impact.d3.convex_Polyhedron.vector_3_Vectors,
                 Impact.d3.Containers.integer_Vectors;

               a : constant Math.Vector_3 :=
                 hullA.m_vertices.Element (polyA.m_indices.Element (e0));
               b : constant Math.Vector_3 :=
                 hullA.m_vertices.Element
                    (polyA.m_indices.Element ((e0 + 1) mod numVerticesA));

               edge0      : constant Math.Vector_3 := a - b;
               WorldEdge0 : constant Math.Vector_3 :=
                 getBasis (transA) * edge0;

               worldPlaneAnormal1 : constant Math.Vector_3 :=
                 getBasis (transA) *
                 (polyA.m_plane (1), polyA.m_plane (2), polyA.m_plane (3));

               planeNormalWS1 : constant Math.Vector_3 :=
                 -cross (WorldEdge0, worldPlaneAnormal1);   -- .cross(WorldEdge
                                                            --0);
               worldA1        : constant Math.Vector_3 := transA * a;
               planeEqWS1     : constant Math.Real     :=
                 -dot (worldA1, planeNormalWS1);

               planeNormalWS : constant Math.Vector_3 := planeNormalWS1;
               planeEqWS     : constant Math.Real     := planeEqWS1;
            begin
               clipFace (pVtxIn.all, pVtxOut.all, planeNormalWS, planeEqWS);
               -- clip face

               declare
                  Pad : constant access btVertexArray := pVtxIn;
               begin
                  pVtxIn  := pVtxOut;
                  pVtxOut := Pad;
               end; --  btSwap (pVtxIn,pVtxOut);

               pVtxOut.Set_Length (0);
            end;
         end loop;

         --  only keep points that are behind the witness face
         --
         declare
            point : Math.Vector_3;

            localPlaneNormal : constant Math.Vector_3 :=
              (polyA.m_plane (1),
               polyA.m_plane (2),
               polyA.m_plane (3));
            localPlaneEq     : constant Math.Real     := polyA.m_plane (4);

            planeNormalWS : constant Math.Vector_3 :=
              getBasis (transA) * localPlaneNormal;
            planeEqWS     : Math.Real              :=
              localPlaneEq - dot (planeNormalWS, getOrigin (transA));

            depth : Math.Real;
         begin
            for i in 1 .. Integer (pVtxIn.Length) loop
               depth := dot (planeNormalWS, pVtxIn.Element (i)) +
                        planeEqWS;

               if depth <= minDist then
                  depth := minDist;
               end if;

               if depth <= maxDist then
                  point := pVtxIn.Element (i);
                  resultOut.addContactPoint (separatingNormal, point, depth);
               end if;
            end loop;
         end;

      end;

   end clipFaceAgainstHull;

   function findSeparatingAxis
     (hullA, hullB   : in Impact.d3.convex_Polyhedron.Item'Class;
      transA, transB : in Transform_3d;
      sep            : out Math.Vector_3)
      return           Boolean
   is
      dmin          : Math.Real := Math.Real'Last;
      curPlaneTests : Integer   := 0;

      numFacesA : constant Integer := Integer (hullA.m_faces.Length);
      numFacesB : Integer;

   begin
      gActualSATPairTests := gActualSATPairTests + 1;

      --  //#ifdef TEST_INTERNAL_OBJECTS
      --          const impact.d3.Vector c0 = transA * hullA.m_localCenter;
      --          const impact.d3.Vector c1 = transB * hullB.m_localCenter;
      --          const impact.d3.Vector DeltaC2 = c0 - c1;
      --  //#endif

      --  Test normals from hullA
      --
      for i in 1 .. numFacesA loop
         declare
            use Impact.d3.Transform, Math.Vectors, Impact.d3.Vector;

            Normal : constant Math.Vector_3 :=
              (hullA.m_faces.Element (i).m_plane (1),
               hullA.m_faces.Element (i).m_plane (2),
               hullA.m_faces.Element (i).m_plane (3));

            faceANormalWS : constant Math.Vector_3 :=
              getBasis (transA) * Normal;

            d : Math.Real;
         begin
            --              if not dot (DeltaC2, faceANormalWS) < 0 then
            curPlaneTests := curPlaneTests + 1;

            --  #ifdef TEST_INTERNAL_OBJECTS
            --                  gExpectedNbTests++;
            --                  if(gUseInternalObject &&
            --!TestInternalObjects(transA,transB, DeltaC2, faceANormalWS,
            --hullA, hullB, dmin))
            --                          continue;
            --                  gActualNbTests++;
            --  #endif

            if not TestSepAxis
                     (hullA,
                      hullB,
                      transA,
                      transB,
                      faceANormalWS,
                      d)
            then
               return False;
            end if;

            if d < dmin then
               dmin := d;
               sep  := faceANormalWS;
            end if;
            --              end if;
         end;
      end loop;

      numFacesB := Integer (hullB.m_faces.Length);

      --  Test normals from hullB
      --
      for i in 1 .. numFacesB loop
         declare
            use Impact.d3.Transform, Math.Vectors, Impact.d3.Vector;

            Normal : constant Math.Vector_3 :=
              (hullB.m_faces.Element (i).m_plane (1),
               hullB.m_faces.Element (i).m_plane (2),
               hullB.m_faces.Element (i).m_plane (3));

            WorldNormal : constant Math.Vector_3 :=
              getBasis (transB) * Normal;

            d : Math.Real;

         begin
            --              if not dot (DeltaC2, WorldNormal) < 0 then
            curPlaneTests := curPlaneTests + 1;

            --  #ifdef TEST_INTERNAL_OBJECTS
            --                  gExpectedNbTests++;
            --                  if(gUseInternalObject &&
            --!TestInternalObjects(transA,transB,DeltaC2, WorldNormal, hullA,
            --hullB, dmin))
            --                          continue;
            --                  gActualNbTests++;
            --  #endif

            if not TestSepAxis
                     (hullA,
                      hullB,
                      transA,
                      transB,
                      WorldNormal,
                      d)
            then
               return False;
            end if;

            if d < dmin then
               dmin := d;
               sep  := WorldNormal;
            end if;
            --              end if;
         end;
      end loop;

      --  Test edges
      --
      declare
         use Impact.d3.Transform, Math.Vectors, Impact.d3.Vector;

         --           edgeAstart,  edgeAend,
         --           edgeBstart,  edgeBend : math.Vector_3;

         curEdgeEdge : Integer := 0;
      begin
         for e0 in 1 .. Integer (hullA.m_uniqueEdges.Length) loop
            declare
               edge0      : constant Math.Vector_3 :=
                 hullA.m_uniqueEdges.Element (e0);
               WorldEdge0 : constant Math.Vector_3 :=
                 getBasis (transA) * edge0;
            begin
               for e1 in 1 .. Integer (hullB.m_uniqueEdges.Length) loop
                  declare
                     edge1      : constant Math.Vector_3 :=
                       hullB.m_uniqueEdges.Element (e1);
                     WorldEdge1 : constant Math.Vector_3 :=
                       getBasis (transB) * edge1;

                     the_Cross : Math.Vector_3 :=
                       cross (WorldEdge0, WorldEdge1);
                     dist      : Math.Real;

                  begin
                     curEdgeEdge := curEdgeEdge + 1;

                     if not IsAlmostZero (the_Cross) then
                        the_Cross := Normalized (the_Cross);

                        --                          if not dot (DeltaC2,
                        --the_Cross) < 0 then
                        --  #ifdef TEST_INTERNAL_OBJECTS
                        --                                  gExpectedNbTests++;
                        --                                  if(gUseInternalObje
                        --ct && !TestInternalObjects(transA,transB,DeltaC2,
                        --Cross, hullA, hullB, dmin))
                        --                                          continue;
                        --                                  gActualNbTests++;
                        --  #endif

                        if not TestSepAxis
                                 (hullA,
                                  hullB,
                                  transA,
                                  transB,
                                  the_Cross,
                                  dist)
                        then
                           return False;
                        end if;

                        if dist < dmin then
                           dmin := dist;
                           sep  := the_Cross;
                        end if;
                        --                          end if;

                     end if;
                  end;
               end loop;
            end;
         end loop;
      end;

      declare
         use Impact.d3.Transform, Math.Vectors, Impact.d3.Vector;

         deltaC : constant Math.Vector_3 :=
           getOrigin (transB) - getOrigin (transA);
      begin
         if dot (deltaC, sep) > 0.0 then
            sep := -sep;
         end if;
      end;

      return True;
   end findSeparatingAxis;

   --  Clips a face to the back of a plane.
   --
   procedure clipFace
     (pVtxIn        : in btVertexArray;
      ppVtxOut      : out btVertexArray;
      planeNormalWS : in Math.Vector_3;
      planeEqWS     : in Math.Real)
   is
      use Impact.d3.Vector;

      ds, de   : Math.Real;
      numVerts : constant Integer := Integer (pVtxIn.Length);

      firstVertex, endVertex : Math.Vector_3;

   begin
      if numVerts < 2 then
         return;
      end if;

      firstVertex := pVtxIn.Element (Integer (pVtxIn.Length));
      endVertex   := pVtxIn.Element (1);
      ds          := dot (planeNormalWS, firstVertex) + planeEqWS;

      for ve in 1 .. numVerts loop
         endVertex := pVtxIn.Element (ve);
         de        := dot (planeNormalWS, endVertex) + planeEqWS;

         if ds < 0.0 then

            if de < 0.0 then   -- Start < 0, end < 0, so output endVertex
               ppVtxOut.Append (endVertex);
            else               -- Start < 0, end >= 0, so output intersection
               ppVtxOut.Append
                 (lerp
                     (firstVertex,
                      endVertex,
                      Math.Real (ds * 1.0 / (ds - de))));
            end if;

         else
            if de < 0.0 then     -- Start >= 0, end < 0 so output intersection
                                 --and end
               ppVtxOut.Append
                 (lerp
                     (firstVertex,
                      endVertex,
                      Math.Real (ds * 1.0 / (ds - de))));
               ppVtxOut.Append (endVertex);
            end if;
         end if;

         firstVertex := endVertex;
         ds          := de;
      end loop;
   end clipFace;

end Impact.d3.Collision.polyhedral_contact_Clipping;

--  #ifdef TEST_INTERNAL_OBJECTS
--
--  inline void BoxSupport(const impact.d3.Scalar extents[3], const
--impact.d3.Scalar sv[3], impact.d3.Scalar p[3])
--  {
--          // This version is ~11.000 cycles (4%) faster overall in one of
--the tests.
--  //        IR(p[0]) = IR(extents[0])|(IR(sv[0])&SIGN_BITMASK);
--  //        IR(p[1]) = IR(extents[1])|(IR(sv[1])&SIGN_BITMASK);
--  //        IR(p[2]) = IR(extents[2])|(IR(sv[2])&SIGN_BITMASK);
--          p[0] = sv[0] < 0.0f ? -extents[0] : extents[0];
--          p[1] = sv[1] < 0.0f ? -extents[1] : extents[1];
--          p[2] = sv[2] < 0.0f ? -extents[2] : extents[2];
--  }

--  void InverseTransformPoint3x3(impact.d3.Vector& out, const
--impact.d3.Vector& in, const impact.d3.Transform& tr)
--  {
--          const impact.d3.Matrix& rot = tr.getBasis();
--          const impact.d3.Vector& r0 = rot[0];
--          const impact.d3.Vector& r1 = rot[1];
--          const impact.d3.Vector& r2 = rot[2];
--
--          const impact.d3.Scalar x = r0.x()*in.x() + r1.x()*in.y() +
--r2.x()*in.z();
--          const impact.d3.Scalar y = r0.y()*in.x() + r1.y()*in.y() +
--r2.y()*in.z();
--          const impact.d3.Scalar z = r0.z()*in.x() + r1.z()*in.y() +
--r2.z()*in.z();
--
--          out.setValue(x, y, z);
--  }

--   bool TestInternalObjects( const impact.d3.Transform& trans0, const
--impact.d3.Transform& trans1, const impact.d3.Vector& delta_c, const
--impact.d3.Vector& axis, const impact.d3.convex_Polyhedron& convex0, const
--impact.d3.convex_Polyhedron& convex1, impact.d3.Scalar dmin)
--  {
--          const impact.d3.Scalar dp = delta_c.dot(axis);
--
--          impact.d3.Vector localAxis0;
--          InverseTransformPoint3x3(localAxis0, axis,trans0);
--          impact.d3.Vector localAxis1;
--          InverseTransformPoint3x3(localAxis1, axis,trans1);
--
--          impact.d3.Scalar p0[3];
--          BoxSupport(convex0.m_extents, localAxis0, p0);
--          impact.d3.Scalar p1[3];
--          BoxSupport(convex1.m_extents, localAxis1, p1);
--
--          const impact.d3.Scalar Radius0 = p0[0]*localAxis0.x() +
--p0[1]*localAxis0.y() + p0[2]*localAxis0.z();
--          const impact.d3.Scalar Radius1 = p1[0]*localAxis1.x() +
--p1[1]*localAxis1.y() + p1[2]*localAxis1.z();
--
--          const impact.d3.Scalar MinRadius = Radius0>convex0.m_radius ?
--Radius0 : convex0.m_radius;
--          const impact.d3.Scalar MaxRadius = Radius1>convex1.m_radius ?
--Radius1 : convex1.m_radius;
--
--          const impact.d3.Scalar MinMaxRadius = MaxRadius + MinRadius;
--          const impact.d3.Scalar d0 = MinMaxRadius + dp;
--          const impact.d3.Scalar d1 = MinMaxRadius - dp;
--
--          const impact.d3.Scalar depth = d0<d1 ? d0:d1;
--          if(depth>dmin)
--                  return false;
--          return true;
--  }

--  #endif //TEST_INTERNAL_OBJECTS
