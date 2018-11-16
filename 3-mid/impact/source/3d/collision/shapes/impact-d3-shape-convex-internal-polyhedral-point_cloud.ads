with impact.d3.Shape.convex.internal.polyhedral;


--  #include "impact.d3.Shape.convex.internal.polyhedral.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.Proxy.h" // for the types
--  #include "LinearMath/btAlignedObjectArray.h"


package impact.d3.Shape.convex.internal.polyhedral.point_cloud
--
--  The impact.d3.Shape.convex.internal.polyhedral.point_cloud implements an implicit convex hull of an array of vertices.
--
is

   type Item is new impact.d3.Shape.convex.internal.polyhedral.btPolyhedralConvexAabbCachingShape with private;




   --- Forge
   --

   function to_point_cloud_Shape return Item;


--          impact.d3.Shape.convex.internal.polyhedral.point_cloud()
--          {
--                  m_localScaling.setValue(1.f,1.f,1.f);
--                  m_shapeType = CONVEX_POINT_CLOUD_SHAPE_PROXYTYPE;
--                  m_unscaledPoints = 0;
--                  m_numPoints = 0;
--          }



   function to_point_cloud_Shape (points       : access Vector_3_Array;
                                  numPoints    : in     Natural;
                                  localScaling : in     math.Vector_3;
                                  computeAabb  : in     Boolean      := True) return Item;

--          impact.d3.Shape.convex.internal.polyhedral.point_cloud(impact.d3.Vector* points,int numPoints, const impact.d3.Vector& localScaling,bool computeAabb = true)
--          {
--                  m_localScaling = localScaling;
--                  m_shapeType = CONVEX_POINT_CLOUD_SHAPE_PROXYTYPE;
--                  m_unscaledPoints = points;
--                  m_numPoints = numPoints;
--
--                  if (computeAabb)
--                          recalcLocalAabb();
--          }





   overriding function  getName   (Self : in     Item)        return String;

--          //debugging
--          virtual const char*        getName()const {return "ConvexPointCloud";}




   overriding function getNumVertices (Self : in Item) return Integer;
   overriding function getNumEdges    (Self : in Item) return Integer;
   overriding function getNumPlanes   (Self : in Item) return Integer;





   overriding procedure getVertex (Self : in Item;   i   : in     Integer;
                                          vtx :    out math.Vector_3);

   overriding procedure getEdge   (Self : in Item;   i      : in     Integer;
                                          pa, pb :    out math.Vector_3);


   overriding procedure getPlane (Self : in Item;   planeNormal  :    out math.Vector_3;
                                         planeSupport :    out math.Vector_3;
                                         i            : in     Integer    );


   overriding function isInside   (Self : in Item;   pt        : in math.Vector_3;
                                          tolerance : in math.Real) return Boolean;




   procedure setPoints (Self : in out Item;   points       : access Vector_3_Array;
                                              numPoints    : in     Natural;
                                              computeAabb  : in     Boolean       := True;
                        localScaling : in     math.Vector_3 := (1.0, 1.0, 1.0));

--          void setPoints (impact.d3.Vector* points, int numPoints, bool computeAabb = true,const impact.d3.Vector& localScaling=impact.d3.Vector(1.f,1.f,1.f))
--          {
--                  m_unscaledPoints = points;
--                  m_numPoints = numPoints;
--                  m_localScaling = localScaling;
--
--                  if (computeAabb)
--                          recalcLocalAabb();
--          }



   function getUnscaledPoints (Self : in Item) return access Vector_3_Array;

--          SIMD_FORCE_INLINE        impact.d3.Vector* getUnscaledPoints()
--          {
--                  return m_unscaledPoints;
--          }


   function getNumPoints   (Self : in Item) return Natural;

--          SIMD_FORCE_INLINE        int getNumPoints() const
--          {
--                  return m_numPoints;
--          }





   function getScaledPoint   (Self : in Item;   index : Positive) return math.Vector_3;

--          SIMD_FORCE_INLINE        impact.d3.Vector        getScaledPoint( int index) const
--          {
--                  return m_unscaledPoints[index] * m_localScaling;
--          }



   overriding function localGetSupportingVertex              (Self : in Item;   vec : in math.Vector_3) return math.Vector_3;
   overriding function localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in math.Vector_3) return math.Vector_3;


   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer);


   overriding procedure setLocalScaling (Self : in out Item;   scaling : in math.Vector_3);
   --
   --  In case we receive negative scaling.







private


   type Item is new impact.d3.Shape.convex.internal.polyhedral.btPolyhedralConvexAabbCachingShape with
      record
        m_unscaledPoints : access Vector_3_Array;
        m_numPoints      :        Natural;
      end record;


end impact.d3.Shape.convex.internal.polyhedral.point_cloud;
