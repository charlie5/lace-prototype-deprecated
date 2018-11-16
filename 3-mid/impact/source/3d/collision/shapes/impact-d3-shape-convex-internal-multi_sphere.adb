with impact.d3.Vector;
with impact.d3.collision.Proxy;
with Ada.Containers;
with impact.d3.Scalar;

--  #include "impact.d3.Shape.convex.internal.multi_sphere.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.collision.Margin.h"
--  #include "LinearMath/impact.d3.Quaternion.h"
--  #include "LinearMath/btSerializer.h"



package body impact.d3.Shape.convex.internal.multi_sphere
is




   function to_multi_sphere_Shape (positions  : in vector_3_Array;
                                   radi       : in math.Vector ;
                                   numSpheres : in Natural     ) return Item
   is
      use ada.Containers;

      Self : Item;

      --  // impact.d3.Scalar startMargin = impact.d3.Scalar(BT_LARGE_FLOAT);
   begin
      Self.setShapeType (impact.d3.collision.Proxy.MULTI_SPHERE_SHAPE_PROXYTYPE);

      Self.m_localPositionArray.set_Length (Count_type (numSpheres));
      Self.m_radiArray         .set_Length (Count_type (numSpheres));

      for i in 1 .. numSpheres
      loop
         Self.m_localPositionArray (i) := positions (i);
         Self.m_radiArray (i)          := radi (i);
      end loop;

      Self.recalcLocalAabb;


      return Self;
   end to_multi_sphere_Shape;







   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3)
   is
      localAabbMin,
      localAabbMax : aliased math.Vector_3;

   begin
      --  As an approximation, take the inertia of the box that bounds the spheres.
      --
      Self.getCachedLocalAabb (localAabbMin, localAabbMax);


      declare
         use math.Vectors;

         halfExtents : math.Vector_3 := (localAabbMax - localAabbMin) * 0.5;

         lx          : constant math.Real := 2.0 * halfExtents (1);
         ly          : constant math.Real := 2.0 * halfExtents (2);
         lz          : constant math.Real := 2.0 * halfExtents (3);
      begin
         inertia := ((mass / 12.0) * (ly * ly + lz*lz),
                     (mass / 12.0) * (lx * lx + lz*lz),
                     (mass / 12.0) * (lx * lx + ly*ly));

      end;

   end calculateLocalInertia;










   overriding function  getName   (Self : in     Item)        return String
   is
      pragma Unreferenced (Self);
   begin
      return "MultiSphere";
   end getName;







   overriding function localGetSupportingVertexWithoutMargin        (Self : in Item;   vec : in Math.Vector_3) return math.Vector_3
   is
      use impact.d3.Vector, math.Functions, math.Vectors;

      supVec : math.Vector_3 :=  math.Origin_3d;
      maxDot : math.Real     := -BT_LARGE_FLOAT;

      the_vec    : math.Vector_3 :=  vec;
      lenSqr : constant math.Real     :=  length2 (the_vec);

      rlen   : math.Real;

   begin
      if lenSqr < impact.d3.Scalar.SIMD_EPSILON * impact.d3.Scalar.SIMD_EPSILON then
         the_vec  := (1.0, 0.0, 0.0);
      else
         rlen    := 1.0 / sqRt (lenSqr);
         the_vec := the_vec * rlen;
      end if;


      declare
         vtx        : math.Vector_3;
         newDot     : math.Real;

         numSpheres : constant Natural := Natural (Self.m_localPositionArray.Length);

      begin
         for i in 1 .. numSpheres
         loop
            declare
               pos : constant math.Vector_3 := Self.m_localPositionArray (i);
               rad : math.Real     := Self.m_radiArray          (i);
            begin
               vtx    := pos  +  Scaled (the_vec, by => Self.getLocalScaling) * rad  -  the_vec * Self.getMargin;
               newDot := dot (the_vec, vtx);

               if newDot > maxDot then
                  maxDot := newDot;
                  supVec := vtx;
               end if;
            end;
         end loop;
      end;


      return supVec;
   end localGetSupportingVertexWithoutMargin;







   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                numVectors         : in     Integer            )
   is
   begin

      for j in 1 .. numVectors
      loop
         declare
            vec        : math.Vector_3 renames vectors (j);

            maxDot     : math.Real     := -BT_LARGE_FLOAT;
            vtx        : math.Vector_3;
            newDot     : math.Real;

            numSpheres : constant Natural       := Natural (Self.m_localPositionArray.Length);

         begin
            for i in 1 .. numSpheres
            loop
               declare
                  use impact.d3.Vector, math.Vectors;

                  pos : math.Vector_3 renames Self.m_localPositionArray (i);
                  rad : math.Real     renames Self.m_radiArray          (i);
               begin
                  vtx    := pos  +  Scaled (vec, by => Self.getLocalScaling) * rad  -  vec * Self.getMargin;
                  newDot := dot (vec, vtx);

                  if newDot > maxDot then
                     maxDot                 := newDot;
                     supportVerticesOut (j) := vtx;
                  end if;

               end;
            end loop;
         end;
      end loop;

   end batchedUnitVectorGetSupportingVertexWithoutMargin;






   function getSphereCount    (Self : in Item) return Natural
   is
   begin
      return Natural (Self.m_localPositionArray.Length);
   end getSphereCount;




   function getSpherePosition (Self : in Item;   index : in Positive) return math.Vector_3
   is
   begin
      return Self.m_localPositionArray (index);
   end getSpherePosition;




   function getSphereRadius   (Self : in Item;   index : in Positive) return math.Real
   is
   begin
      return Self.m_radiArray (index);
   end getSphereRadius;


end impact.d3.Shape.convex.internal.multi_sphere;












