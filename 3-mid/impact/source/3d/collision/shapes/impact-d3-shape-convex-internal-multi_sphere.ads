with impact.d3.Shape.convex.internal;
with impact.d3.Containers;



package impact.d3.Shape.convex.internal.multi_sphere
--
--  The impact.d3.Shape.convex.internal.multi_sphere represents the convex hull of a collection of spheres. You can create special capsules or other smooth volumes.
--
--  It is possible to animate the spheres for deformation, but call 'recalcLocalAabb' after changing any sphere position/radius
--
is

   type Item is new impact.d3.Shape.convex.internal.btConvexInternalAabbCachingShape with private;



   function to_multi_sphere_Shape (positions  : in vector_3_Array;
                                   radi       : in math.Vector ;
                                   numSpheres : in Natural     ) return Item;




   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);



   overriding function  getName   (Self : in     Item)        return String;


--          virtual const char*        getName()const
--          {
--                  return "MultiSphere";
--          }





   overriding function localGetSupportingVertexWithoutMargin        (Self : in Item;   vec : in Math.Vector_3) return math.Vector_3;

   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer            );



   function getSphereCount    (Self : in Item) return Natural;

--          int        getSphereCount() const
--          {
--                  return m_localPositionArray.size();
--          }


   function getSpherePosition (Self : in Item;   index : in Positive) return math.Vector_3;

--          const impact.d3.Vector&        getSpherePosition(int index) const
--          {
--                  return m_localPositionArray[index];
--          }


   function getSphereRadius   (Self : in Item;   index : in Positive) return math.Real;

--          impact.d3.Scalar        getSphereRadius(int index) const
--          {
--                  return m_radiArray[index];
--          }






private

   type Item is new impact.d3.Shape.convex.internal.btConvexInternalAabbCachingShape with
      record
         m_localPositionArray : Containers.vector_3_Vector;
         m_radiArray          : Containers.real_Vector;
      end record;

end impact.d3.Shape.convex.internal.multi_sphere;
