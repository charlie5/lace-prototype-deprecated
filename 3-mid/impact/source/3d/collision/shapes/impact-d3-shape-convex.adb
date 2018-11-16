with ada.unchecked_Conversion;

with impact.d3.Shape.convex,
     impact.d3.Shape.convex.internal.polyhedral.triangle,
     impact.d3.Shape.convex.internal.sphere,
     impact.d3.Shape.convex.internal.cylinder,
     impact.d3.Shape.convex.internal.polyhedral.hull;

--       btCapsuleShape,   -- obsolete


with impact.d3.Transform,
     impact.d3.collision.Proxy,
     impact.d3.Shape.convex.internal.polyhedral.box,
     impact.d3.Shape.convex.internal.polyhedral.triangle,
--       impact.d3.Shape.convex.internal.cylinder,
--       impact.d3.convex_HullShape,
     impact.d3.Vector,
     impact.d3.Matrix,

     interfaces.c.Pointers;
with impact.d3.Scalar;
with impact.d3.Shape.convex.internal.polyhedral;



package body impact.d3.Shape.convex
--
--
--
is


   type Vector_3s is array (Positive range <>) of aliased Vector_3;

   package vector_Pointers is new interfaces.c.Pointers (Positive,  Vector_3,  Vector_3s,  (0.0, 0.0, 0.0));






   function convexHullSupport (localDirOrg  : in     Vector_3;
                               points       : access Vector_3;
                               numPoints    : in     Integer;
                               localScaling : in     Vector_3) return Vector_3
   is
      use Interfaces, vector_Pointers, impact.d3.Vector;

      the_Points : Vector_3s renames Value (points.all'Access,
                                            c.ptrdiff_t (numPoints));

      vec     : constant Vector_3 := Scaled (localDirOrg, by => localScaling);
      newDot  : Real;
      maxDot  : Real   := Real'First;
      ptIndex : Integer  := -1;

      supVec : Vector_3;
   begin

      for i in 1 .. numPoints loop
         newDot := dot (vec, the_Points (i));

         if newDot > maxDot then
            maxDot  := newDot;
            ptIndex := i;
         end if;
      end loop;

      pragma Assert (ptIndex >= 1);

      supVec := Scaled (the_Points (ptIndex),  by => localScaling);

      return supVec;
   end convexHullSupport;






   type Box_view      is access all impact.d3.Shape.convex.internal.polyhedral.box.Item'Class;
   type Triangle_view is access all impact.d3.Shape.convex.internal.polyhedral.triangle.Item'Class;


   function localGetSupportVertexWithoutMarginNonVirtual (Self : in Item'Class;   localDir : in Vector_3) return Vector_3
   is
      use impact.d3.collision.Proxy;

   begin
      case Self.getShapeType
      is
         when SPHERE_SHAPE_PROXYTYPE =>
            return (0.0, 0.0, 0.0);


         when BOX_SHAPE_PROXYTYPE =>
            declare
               use Standard.impact.d3.Scalar;
               convexShape : constant impact.d3.Shape.convex.internal.polyhedral.box.item'Class := impact.d3.Shape.convex.internal.polyhedral.box.item'Class (Self);
               halfExtents : constant Vector_3        := convexShape.getImplicitShapeDimensions;
            begin
               return (btFsels (localDir (1),  halfExtents (1),  -halfExtents (1)),
                       btFsels (localDir (2),  halfExtents (2),  -halfExtents (2)),
                       btFsels (localDir (3),  halfExtents (3),  -halfExtents (3)));
            end;


         when TRIANGLE_SHAPE_PROXYTYPE =>
            declare
               use impact.d3.Vector;

               triangleShape : impact.d3.Shape.convex.internal.polyhedral.triangle.item'Class      := impact.d3.Shape.convex.internal.polyhedral.triangle.item'Class (Self);
               dir           : constant Vector_3                        := (localDir (1),  localDir (2),  localDir (3));

               vertices      : constant array (1 .. 3) of access Vector_3 := (triangleShape.m_vertices1 (1)'Access,
                                                                   triangleShape.m_vertices1 (2)'Access,
                                                                   triangleShape.m_vertices1 (3)'Access);

               dots          : constant Vector_3 := (dot (dir, vertices (1).all),
                                            dot (dir, vertices (2).all),
                                            dot (dir, vertices (3).all));
               sup           : constant Vector_3 := vertices (maxAxis (dots)).all;
            begin
               return sup;
            end;


         when CYLINDER_SHAPE_PROXYTYPE =>
            declare
               cylShape : constant impact.d3.Shape.convex.internal.cylinder.item'Class := impact.d3.Shape.convex.internal.cylinder.item'Class (Self);

               --  mapping of halfextents/dimension onto radius/height depends on how cylinder local orientation is (upAxis)
               halfExtents    : Vector_3 := cylShape.getImplicitShapeDimensions;
               v              : Vector_3 := (localDir (1),  localDir (2),  localDir (3));
               cylinderUpAxis : constant Integer  := cylShape.getUpAxis;

               XX : Integer := 2;
               YY : Integer := 1;
               ZZ : Integer := 3;
            begin

               case cylinderUpAxis is
               when 1 =>
                  XX := 2;
                  YY := 1;
                  ZZ := 3;
               when 2 =>
                  XX := 1;
                  YY := 2;
                  ZZ := 3;
               when 3 =>
                  XX := 1;
                  YY := 3;
                  ZZ := 2;
               when others =>
                  pragma Assert (False);
                  raise Program_Error;
               end case;

               declare
                  use math.Functions;

                  radius     : constant Real := halfExtents (XX);
                  halfHeight : constant Real := halfExtents (cylinderUpAxis);

                  tmp        : Vector_3;
                  d          : Real;

                  s          : constant Real := sqRt (v (XX) * v (XX)  +  v (ZZ) * v (ZZ));
               begin
                  if s /= 0.0 then
                     d       := radius / s;
                     tmp (XX) := v (XX) * d;

                     if v (YY) < 0.0 then
                        tmp (YY) := -halfHeight;
                     else
                        tmp (YY) :=  halfHeight;
                     end if;

                     tmp (ZZ) := v (ZZ) * d;

                     return tmp;
                  else
                     tmp (XX) := radius;

                     if v (YY) < 0.0 then
                        tmp (YY) := -halfHeight;
                     else
                        tmp (YY) :=  halfHeight;
                     end if;

                     tmp (ZZ) := 0.0;

                     return tmp;
                  end if;
               end;
            end;


--           when CAPSULE_SHAPE_PROXYTYPE =>   -- nb: capsules are provided by multi-sphere, so this is probably obsolete ?
--              declare
--                 capsuleShape : access btCapsuleShape.item := btCapsuleShape.view (Self);
--
--                 vec0          : Vector_3 := localDir;
--                 halfHeight    : Scalar   := capsuleShape.getHalfHeight;
--                 capsuleUpAxis : Integer  := capsuleShape.getUpAxis;
--
--                 radius : Scalar   := capsuleShape.getRadius;
--                 supVec : Vector_3 := (0.0, 0.0, 0.0);
--
--                 maxDot : Scalar   := -BT_LARGE_FLOAT;
--
--                 vec    : Vector_3 := vec0;
--                 lenSqr : Scalar   := length2 (vec);
--
--                 pos,
--                 vtx    : Vector_3;
--
--                 newDot,
--                 rlen   : Scalar;
--              begin
--                 if lenSqr < 0.0001 then
--                    vec := (1.0, 0.0, 0.0);
--                 else
--                    rlen := 1.0 / sqRt (lenSqr);
--                    vec  := vec * rlen;
--                 end if;
--
--                 pos                 := (0.0, 0.0, 0.0);
--                 pos (capsuleUpAxis) := halfHeight;
--
--                 -- vtx = pos +vec*(radius);
--                 vtx    := pos  +  vec * capsuleShape.getLocalScalingNV * radius  -  vec * capsuleShape.getMarginNV;
--                 newDot := dot (vec, vtx);
--
--                 if newDot > maxDot then
--                    maxDot := newDot;
--                    supVec := vtx;
--                 end if;
--
--                 pos                 := (0.0, 0.0, 0.0);
--                 pos (capsuleUpAxis) := -halfHeight;
--
--                 -- vtx = pos +vec*(radius);
--                 vtx    := pos  +  vec * capsuleShape.getLocalScalingNV * radius  -  vec * capsuleShape.getMarginNV;
--                 newDot := dot (vec, vtx);
--
--                 if newDot > maxDot then
--                    maxDot := newDot;
--                    supVec := vtx;
--                 end if;
--
--                 return supVec;
--              end;
--

--           when CONVEX_POINT_CLOUD_SHAPE_PROXYTYPE =>
--              declare
--                 convexPointCloudShape : access impact.d3.Shape.convex.internal.polyhedral.point_cloud.item := impact.d3.Shape.convex.internal.polyhedral.point_cloud.view (Self);
--                 points                : access impact.d3.Vector                    := convexPointCloudShape.getUnscaledPoints;
--                 numPoints             :        Integer                      := convexPointCloudShape.getNumPoints;
--              begin
--                 return convexHullSupport (localDir, points, numPoints, convexPointCloudShape.getLocalScalingNV);
--              end;
--

         when CONVEX_HULL_SHAPE_PROXYTYPE =>
            declare
               package hull_Shape renames impact.d3.Shape.convex.internal.polyhedral.hull;

               type        Vector_3_view is access all math.Vector_3;
               type      c_Vector_3_view is access all c_Vector_3;
               function to_Vector_3_view is new ada.unchecked_Conversion (c_Vector_3_view,  Vector_3_view);   -- tbd: Remove/improve this.

               convexHullShape : constant hull_Shape.item'Class := hull_Shape.item'Class (Self);
               points          : constant Vector_3_view                := to_Vector_3_view (convexHullShape.getUnscaledPoints);
               numPoints       : constant Integer                      := convexHullShape.getNumPoints;
            begin
               return convexHullSupport (localDir, points, numPoints, convexHullShape.getLocalScalingNV);
            end;


         when others =>
--              null;
            return Self.localGetSupportingVertexWithoutMargin (localDir);
      end case;



      pragma Assert (False);      -- Should never reach here.
      return (0.0, 0.0, 0.0);

   end localGetSupportVertexWithoutMarginNonVirtual;








   function localGetSupportVertexNonVirtual (Self : in Item'Class;        vec : in Vector_3) return Vector_3
   is
      use impact.d3.Vector, math.Vectors;

      localDir     : math.Vector_3 renames vec;
      localDirNorm : math.Vector_3 := localDir;

   begin
      if length2 (localDirNorm) < impact.d3.Scalar.SIMD_EPSILON * impact.d3.Scalar.SIMD_EPSILON then
         localDirNorm := (-1.0, -1.0, -1.0);
      end if;

      normalize (localDirNorm);

      return Self.localGetSupportVertexWithoutMarginNonVirtual (localDirNorm)  +  Self.getMarginNonVirtual * localDirNorm;
   end localGetSupportVertexNonVirtual;







   --  TODO: This should be bumped up to impact.d3.Shape ()
   --
   function getMarginNonVirtual                          (Self : in Item'Class) return Real
   is
   begin
      case Self.getShapeType
      is
      when impact.d3.collision.Proxy.SPHERE_SHAPE_PROXYTYPE =>
         declare
            sphereShape : constant impact.d3.Shape.convex.internal.sphere.item'Class := impact.d3.Shape.convex.internal.sphere.item'Class (Self);
         begin
            return sphereShape.getRadius;
         end;


         when impact.d3.collision.Proxy.BOX_SHAPE_PROXYTYPE =>
         declare
               convexShape : constant impact.d3.Shape.convex.internal.polyhedral.box.item'Class := impact.d3.Shape.convex.internal.polyhedral.box.item'Class (Self);
         begin
            return convexShape.getMarginNV;
         end;

         when impact.d3.collision.Proxy.TRIANGLE_SHAPE_PROXYTYPE =>
         declare
               triangleShape : constant impact.d3.Shape.convex.internal.polyhedral.triangle.item'Class := impact.d3.Shape.convex.internal.polyhedral.triangle.item'Class (Self);
         begin
            return triangleShape.getMarginNV;
         end;

         when impact.d3.collision.Proxy.CYLINDER_SHAPE_PROXYTYPE =>
            raise Program_Error with "TBD";
--           declare
--              cylShape : impact.d3.Shape.convex.internal.cylinder.item'Class := impact.d3.Shape.convex.internal.cylinder.item'Class (Self);
--           begin
--              return cylShape.getMarginNV;
--           end;

         when impact.d3.collision.Proxy.CAPSULE_SHAPE_PROXYTYPE =>
            raise Program_Error with "TBD";
--           declare
--              capsuleShape : btCapsuleShape.item'Class := btCapsuleShape.item'Class (Self);
--           begin
--              return capsuleShape.getMarginNV;
--           end;

         when impact.d3.collision.Proxy.CONVEX_POINT_CLOUD_SHAPE_PROXYTYPE
           |  impact.d3.collision.Proxy.CONVEX_HULL_SHAPE_PROXYTYPE =>
            raise Program_Error with "TBD";
--           declare
--               convexHullShape : impact.d3.Shape.convex.internal.polyhedral.item'Class := impact.d3.Shape.convex.internal.polyhedral.item'Class (Self);
--           begin
--              return convexHullShape.getMarginNV;
--           end;

         when others =>
            return Self.getMargin;
      end case;


      --  should never reach here
      pragma Assert (False);
      return 0.0;
   end getMarginNonVirtual;







   procedure getAabbNonVirtual (Self : in Item'Class;   t                : in     Transform_3d;
                                aabbMin, aabbMax :    out Vector_3)
   is
      use impact.d3.Transform, impact.d3.Vector, impact.d3.Matrix,
          math.Vectors;

   begin
      case Self.getShapeType
      is
      when impact.d3.collision.Proxy.SPHERE_SHAPE_PROXYTYPE =>
         declare
            sphereShape : constant impact.d3.Shape.convex.internal.sphere.item'Class := impact.d3.Shape.convex.internal.sphere.Item'Class (Self);
            radius      : constant math.Real                := sphereShape.getImplicitShapeDimensions (1);    --  * convexShape->getLocalScaling().getX();
            margin      : constant math.Real                := radius + sphereShape.getMarginNonVirtual;
            center      : constant math.Vector_3            := getOrigin (t);
            extent      : constant math.Vector_3            := (margin, margin, margin);
         begin
            aabbMin := center - extent;
            aabbMax := center + extent;
         end;


      when impact.d3.collision.Proxy.CYLINDER_SHAPE_PROXYTYPE
         | impact.d3.collision.Proxy.BOX_SHAPE_PROXYTYPE =>
         declare
            convexShape : constant impact.d3.Shape.convex.internal.polyhedral.box.item'Class := impact.d3.Shape.convex.internal.polyhedral.box.item'Class (Self);

            margin      : math.Real       := convexShape.getMarginNonVirtual;
            halfExtents : constant math.Vector_3   :=   convexShape.getImplicitShapeDimensions
                                             + (margin, margin, margin);

            abs_b       : constant math.Matrix_3x3 := absolute (getBasis (t));

            center      : constant math.Vector_3   := getOrigin (t);
            extent      : constant math.Vector_3   := (dot (getRow (abs_b, 1),  halfExtents),
                                              dot (getRow (abs_b, 2),  halfExtents),
                                              dot (getRow (abs_b, 3),  halfExtents));
         begin
            aabbMin := center - extent;
            aabbMax := center + extent;
         end;


      when impact.d3.collision.Proxy.TRIANGLE_SHAPE_PROXYTYPE =>
         declare
            use linear_Algebra_3d;
            triangleShape : constant impact.d3.Shape.convex.internal.polyhedral.triangle.item'Class := impact.d3.Shape.convex.internal.polyhedral.triangle.item'Class (Self);
            margin        : math.Real                  := triangleShape.getMarginNonVirtual;

            vec, sv, tmp  : math.Vector_3;

         begin
            for i in 1 .. 3
                loop
                        vec         := (0.0, 0.0, 0.0);
                        vec (i)     := 1.0;

                        sv          := Self.localGetSupportVertexWithoutMarginNonVirtual (vec * getBasis (t));

                        tmp         := t * sv;
                        aabbMax (i) := tmp (i) + margin;
                        vec (i)     := -1.0;
                        tmp         := t * Self.localGetSupportVertexWithoutMarginNonVirtual (vec * getBasis (t));
                        aabbMin (i) := tmp (i) - margin;
            end loop;
         end;

         when impact.d3.collision.Proxy.CAPSULE_SHAPE_PROXYTYPE =>
         raise Program_Error with "TBD";
--          declare
--                  btCapsuleShape* capsuleShape := (btCapsuleShape*)this;
--                  impact.d3.Vector halfExtents(capsuleShape->getRadius(),capsuleShape->getRadius(),capsuleShape->getRadius());
--                  int m_upAxis := capsuleShape->getUpAxis();
--                  halfExtents[m_upAxis] := capsuleShape->getRadius() + capsuleShape->getHalfHeight();
--                  halfExtents += impact.d3.Vector(capsuleShape->getMarginNonVirtual(),capsuleShape->getMarginNonVirtual(),capsuleShape->getMarginNonVirtual());
--                  impact.d3.Matrix abs_b := t.getBasis().absolute();
--                  impact.d3.Vector center := t.getOrigin();
--                  impact.d3.Vector extent := impact.d3.Vector(abs_b[0].dot(halfExtents),abs_b[1].dot(halfExtents),abs_b[2].dot(halfExtents));
--           begin
--                  aabbMin := center - extent;
--                  aabbMax := center + extent;
--           end;

         when impact.d3.collision.Proxy.CONVEX_POINT_CLOUD_SHAPE_PROXYTYPE
            | impact.d3.collision.Proxy.CONVEX_HULL_SHAPE_PROXYTYPE =>

            raise Program_Error with "TBD32";
--           declare
--              convexHullShape : btPolyhedralConvexAabbCachingShape.item'Class := btPolyhedralConvexAabbCachingShape.item'Class (Self);
--              margin          : math.Real                                     := convexHullShape.getMarginNonVirtual;
--           begin
--              convexHullShape.getNonvirtualAabb (t, aabbMin, aabbMax, margin);
--           end;

      when others =>
         Self.getAabb (t, aabbMin, aabbMax);

      end case;

      --  should never reach here
      pragma Assert (False);

   end getAabbNonVirtual;






   procedure project (Self : in Item;         t        : in     Transform_3d;
                                              dir      : in     Vector_3;
                      min, max :    out Real)
   is
      use linear_Algebra_3d,  impact.d3.Transform, impact.d3.Vector, Math.Vectors;

      localAxis : constant Vector_3 := dir * getBasis (t);
      vtx1      : constant Vector_3 := t * Item'Class (Self).localGetSupportingVertex (localAxis);
      vtx2      : constant Vector_3 := t * Item'Class (Self).localGetSupportingVertex (-localAxis);

      tmp       : Real;
   begin
      min := dot (vtx1, dir);
      max := dot (vtx2, dir);

      if min > max then
         tmp := min;
         min := max;
         max := tmp;
      end if;
   end project;




end impact.d3.Shape.convex;







---------------------------------------------------------------
--- below is left for reference (it has been ported above) ...
--

--  impact.d3.Vector impact.d3.Shape.convex::localGetSupportVertexWithoutMarginNonVirtual (const impact.d3.Vector& localDir) const
--  {
--          switch (m_shapeType)
--          {
--      case SPHERE_SHAPE_PROXYTYPE:
--          {
--                  return impact.d3.Vector(0,0,0);
--      }
--          case BOX_SHAPE_PROXYTYPE:
--          {
--                  impact.d3.Shape.convex.internal.polyhedral.box* convexShape = (impact.d3.Shape.convex.internal.polyhedral.box*)this;
--                  const impact.d3.Vector& halfExtents = convexShape->getImplicitShapeDimensions();
--
--                  return impact.d3.Vector(btFsels(localDir.x(), halfExtents.x(), -halfExtents.x()),
--                          btFsels(localDir.y(), halfExtents.y(), -halfExtents.y()),
--                          btFsels(localDir.z(), halfExtents.z(), -halfExtents.z()));
--          }
--          case TRIANGLE_SHAPE_PROXYTYPE:
--          {
--                  impact.d3.Shape.convex.internal.polyhedral.triangle* triangleShape = (impact.d3.Shape.convex.internal.polyhedral.triangle*)this;
--                  impact.d3.Vector dir(localDir.getX(),localDir.getY(),localDir.getZ());
--                  impact.d3.Vector* vertices = &triangleShape->m_vertices1[0];
--                  impact.d3.Vector dots(dir.dot(vertices[0]), dir.dot(vertices[1]), dir.dot(vertices[2]));
--                  impact.d3.Vector sup = vertices[dots.maxAxis()];
--                  return impact.d3.Vector(sup.getX(),sup.getY(),sup.getZ());
--          }
--          case CYLINDER_SHAPE_PROXYTYPE:
--          {
--                  impact.d3.Shape.convex.internal.cylinder* cylShape = (impact.d3.Shape.convex.internal.cylinder*)this;
--                  //mapping of halfextents/dimension onto radius/height depends on how cylinder local orientation is (upAxis)
--
--                  impact.d3.Vector halfExtents = cylShape->getImplicitShapeDimensions();
--                  impact.d3.Vector v(localDir.getX(),localDir.getY(),localDir.getZ());
--                  int cylinderUpAxis = cylShape->getUpAxis();
--                  int XX(1),YY(0),ZZ(2);
--
--                  switch (cylinderUpAxis)
--                  {
--                  case 0:
--                  {
--                          XX = 1;
--                          YY = 0;
--                          ZZ = 2;
--                  }
--                  break;
--                  case 1:
--                  {
--                          XX = 0;
--                          YY = 1;
--                          ZZ = 2;
--                  }
--                  break;
--                  case 2:
--                  {
--                          XX = 0;
--                          YY = 2;
--                          ZZ = 1;
--
--                  }
--                  break;
--                  default:
--                          btAssert(0);
--                  break;
--                  };
--
--                  impact.d3.Scalar radius = halfExtents[XX];
--                  impact.d3.Scalar halfHeight = halfExtents[cylinderUpAxis];
--
--                  impact.d3.Vector tmp;
--                  impact.d3.Scalar d ;
--
--                  impact.d3.Scalar s = btSqrt(v[XX] * v[XX] + v[ZZ] * v[ZZ]);
--                  if (s != impact.d3.Scalar(0.0))
--                  {
--                          d = radius / s;
--                          tmp[XX] = v[XX] * d;
--                          tmp[YY] = v[YY] < 0.0 ? -halfHeight : halfHeight;
--                          tmp[ZZ] = v[ZZ] * d;
--                          return impact.d3.Vector(tmp.getX(),tmp.getY(),tmp.getZ());
--                  } else {
--                          tmp[XX] = radius;
--                          tmp[YY] = v[YY] < 0.0 ? -halfHeight : halfHeight;
--                          tmp[ZZ] = impact.d3.Scalar(0.0);
--                          return impact.d3.Vector(tmp.getX(),tmp.getY(),tmp.getZ());
--                  }
--          }
--          case CAPSULE_SHAPE_PROXYTYPE:
--          {
--                  impact.d3.Vector vec0(localDir.getX(),localDir.getY(),localDir.getZ());
--
--                  btCapsuleShape* capsuleShape = (btCapsuleShape*)this;
--                  impact.d3.Scalar halfHeight = capsuleShape->getHalfHeight();
--                  int capsuleUpAxis = capsuleShape->getUpAxis();
--
--                  impact.d3.Scalar radius = capsuleShape->getRadius();
--                  impact.d3.Vector supVec(0,0,0);
--
--                  impact.d3.Scalar maxDot(impact.d3.Scalar(-BT_LARGE_FLOAT));
--
--                  impact.d3.Vector vec = vec0;
--                  impact.d3.Scalar lenSqr = vec.length2();
--                  if (lenSqr < impact.d3.Scalar(0.0001))
--                  {
--                          vec.setValue(1,0,0);
--                  } else
--                  {
--                          impact.d3.Scalar rlen = impact.d3.Scalar(1.) / btSqrt(lenSqr );
--                          vec *= rlen;
--                  }
--                  impact.d3.Vector vtx;
--                  impact.d3.Scalar newDot;
--                  {
--                          impact.d3.Vector pos(0,0,0);
--                          pos[capsuleUpAxis] = halfHeight;
--
--                          //vtx = pos +vec*(radius);
--                          vtx = pos +vec*capsuleShape->getLocalScalingNV()*(radius) - vec * capsuleShape->getMarginNV();
--                          newDot = vec.dot(vtx);
--
--
--                          if (newDot > maxDot)
--                          {
--                                  maxDot = newDot;
--                                  supVec = vtx;
--                          }
--                  }
--                  {
--                          impact.d3.Vector pos(0,0,0);
--                          pos[capsuleUpAxis] = -halfHeight;
--
--                          //vtx = pos +vec*(radius);
--                          vtx = pos +vec*capsuleShape->getLocalScalingNV()*(radius) - vec * capsuleShape->getMarginNV();
--                          newDot = vec.dot(vtx);
--                          if (newDot > maxDot)
--                          {
--                                  maxDot = newDot;
--                                  supVec = vtx;
--                          }
--                  }
--                  return impact.d3.Vector(supVec.getX(),supVec.getY(),supVec.getZ());
--          }
--          case CONVEX_POINT_CLOUD_SHAPE_PROXYTYPE:
--          {
--                  impact.d3.Shape.convex.internal.polyhedral.point_cloud* convexPointCloudShape = (impact.d3.Shape.convex.internal.polyhedral.point_cloud*)this;
--                  impact.d3.Vector* points = convexPointCloudShape->getUnscaledPoints ();
--                  int numPoints = convexPointCloudShape->getNumPoints ();
--                  return convexHullSupport (localDir, points, numPoints,convexPointCloudShape->getLocalScalingNV());
--          }
--          case CONVEX_HULL_SHAPE_PROXYTYPE:
--          {
--                  impact.d3.convex_HullShape* convexHullShape = (impact.d3.convex_HullShape*)this;
--                  impact.d3.Vector* points = convexHullShape->getUnscaledPoints();
--                  int numPoints = convexHullShape->getNumPoints ();
--                  return convexHullSupport (localDir, points, numPoints,convexHullShape->getLocalScalingNV());
--          }
--      default:
--  #ifndef __SPU__
--                  return this->localGetSupportingVertexWithoutMargin (localDir);
--  #else
--                  btAssert (0);
--  #endif
--          }
--
--          // should never reach here
--          btAssert (0);
--          return impact.d3.Vector (impact.d3.Scalar(0.0f), impact.d3.Scalar(0.0f), impact.d3.Scalar(0.0f));
--  }
