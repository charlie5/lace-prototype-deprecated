with impact.d3.Shape.convex.internal.polyhedral;
--  #include "impact.d3.Shape.convex.internal.polyhedral.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.Proxy.h" // for the types
--  #include "LinearMath/btAlignedObjectArray.h"



package impact.d3.Shape.convex.internal.polyhedral.hull
--
--  The impact.d3.convex_HullShape implements an implicit convex hull of an array of vertices.
--
--  Bullet provides a general and fast collision detector for convex shapes based on GJK and EPA using localGetSupportingVertex.
--
is


   type Item is new impact.d3.Shape.convex.internal.polyhedral.btPolyhedralConvexAabbCachingShape with private;




   ----------
   --- Forge
   --

   function to_hull_Shape (points    : access math.Real := null;
                           numPoints : in     Natural   := 0;
                           stride    : in     Integer   := math.Vector_3'Size / 8) return Item;
   --
   --  This constructor optionally takes in a pointer to points. Each point is assumed to be 3 consecutive impact.d3.Scalar (x,y,z), the
   --  striding defines the number of bytes between each point, in memory.
   --
   --  It is easier to not pass any points in the constructor, and just add one point at a time, using addPoint.
   --
   --  Makes an internal copy of the points.






   ---------------
   --- Attributes
   --

   overriding function  getName        (Self : in     Item) return String;

--          virtual const char*        getName()const {return "Convex";}



   overriding function  getNumVertices (Self : in Item) return Integer;
   overriding function  getNumEdges    (Self : in Item) return Integer;
   overriding function  getNumPlanes   (Self : in Item) return Integer;

   overriding procedure getVertex (Self : in Item;   i            : in     Integer;
                                          vtx          :    out math.Vector_3);

   overriding procedure getEdge   (Self : in Item;   i            : in     Integer;
                                          pa, pb       :    out math.Vector_3);

   overriding procedure getPlane  (Self : in Item;   planeNormal  :    out math.Vector_3;
                                          planeSupport :    out math.Vector_3;
                                          i            : in     Integer);

   overriding function  isInside  (Self : in Item;   pt           : in math.Vector_3;
                                          tolerance    : in math.Real) return Boolean;


   procedure addPoint          (Self : in out Item;   point : in math.Vector_3);

   function  getUnscaledPoints (Self : in     Item                    ) return access c_Vector_3;
   function  getScaledPoint    (Self : in     Item;   i     : in Integer) return math.Vector_3;
   function  getNumPoints      (Self : in     Item                    ) return Natural;



   overriding function  localGetSupportingVertex              (Self : in Item;   vec : in Math.Vector_3) return Math.Vector_3;
   overriding function  localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in Math.Vector_3) return Math.Vector_3;

   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer);


   procedure project (Self : in Item'Class;   t        : in     Transform_3d;
                                              dir      : in     Math.Vector_3;
                                              min, max :    out Real);



   overriding procedure setLocalScaling (Self : in out Item;   scaling : in Math.Vector_3);
   --
   --  In case we receive negative scaling.






private

   type access_c_Vector_3_array is access c_Vector_3_array;


   type Item is new impact.d3.Shape.convex.internal.polyhedral.btPolyhedralConvexAabbCachingShape with
      record
         m_unscaledPoints : access_c_Vector_3_array;
         m_point_Count    : Natural := 0;
      end record;


end impact.d3.Shape.convex.internal.polyhedral.hull;
