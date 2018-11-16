with impact.d3.Shape.convex.internal,
     impact.d3.convex_Polyhedron;





package impact.d3.Shape.convex.internal.polyhedral
--
--  The impact.d3.Shape.convex.internal.polyhedral is an internal interface class for polyhedral convex shapes.
--
is


   type Item is abstract new impact.d3.Shape.convex.internal.item with private;
   type View is access all Item'Class;



   overriding procedure destruct (Self : in out Item);



   function initializePolyhedralFeatures (Self : access Item'Class) return Boolean;
   --
   --  optional method mainly used to generate multiple contact points by clipping polyhedral features (faces/edges)


   function getConvexPolyhedron (Self : in Item'Class) return access impact.d3.convex_Polyhedron.item'Class;




   --- brute force implementations
   --

   overriding function localGetSupportingVertexWithoutMargin (Self : in Item;   vec : in math.Vector_3) return math.Vector_3;


   overriding procedure batchedUnitVectorGetSupportingVertexWithoutMargin (Self : in Item;   vectors            : in     Vector_3_array;
                                                                                  supportVerticesOut :    out Vector_3_array;
                                                                                  numVectors         : in     Integer);


   overriding procedure calculateLocalInertia (Self : in Item;   mass    : in     math.Real;
                                                      inertia :    out math.Vector_3);


   function getNumVertices (Self : in Item) return Integer   is abstract;
   function getNumEdges    (Self : in Item) return Integer   is abstract;
   function getNumPlanes   (Self : in Item) return Integer   is abstract;


   procedure getVertex (Self : in Item;   i   : in     Integer;
                                          vtx :    out math.Vector_3)      is abstract;

   procedure getEdge   (Self : in Item;   i      : in     Integer;
                                          pa, pb :    out math.Vector_3)   is abstract;


   procedure getPlane (Self : in Item;   planeNormal  :    out math.Vector_3;
                                         planeSupport :    out math.Vector_3;
                                         i            : in     Integer)    is abstract;


   --  //        virtual int getIndex(int i) const = 0 ;


   function isInside   (Self : in Item;   pt        : in math.Vector_3;
                                          tolerance : in math.Real) return Boolean   is abstract;













   --- 'btPolyhedralConvexAabbCachingShape'
   --
   --  The btPolyhedralConvexAabbCachingShape adds aabb caching to the impact.d3.Shape.convex.internal.polyhedral.
   --

   type btPolyhedralConvexAabbCachingShape is abstract new impact.d3.Shape.convex.internal.polyhedral.item with private;



   procedure getNonvirtualAabb (Self : in     btPolyhedralConvexAabbCachingShape'Class;   trans            : in     Transform_3d;
                                                                                          aabbMin, aabbMax :    out math.Vector_3;
                                                                                          margin           : in     math.Real);

   overriding procedure setLocalScaling   (Self : in out btPolyhedralConvexAabbCachingShape;         scaling : in math.Vector_3);


   overriding procedure getAabb           (Self : in     btPolyhedralConvexAabbCachingShape;         t                : in     Transform_3d;
                                                                                          aabbMin, aabbMax :    out math.Vector_3);


   procedure recalcLocalAabb   (Self : in out btPolyhedralConvexAabbCachingShape'Class);







private


   type Polyhedron_view is access all impact.d3.convex_Polyhedron.item'Class;


   type Item is abstract new impact.d3.Shape.convex.internal.item with
      record
         m_polyhedron : Polyhedron_view;
      end record;






   type btPolyhedralConvexAabbCachingShape is abstract new impact.d3.Shape.convex.internal.polyhedral.item with
      record
         m_localAabbMin     : math.Vector_3 := (1.0,  1.0,  1.0);
         m_localAabbMax     : math.Vector_3 := (-1.0, -1.0, -1.0);
         m_isLocalAabbValid : Boolean       := False;
      end record;




   procedure setCachedLocalAabb (Self : in out btPolyhedralConvexAabbCachingShape'Class;   aabbMin, aabbMax : in math.Vector_3);
   procedure getCachedLocalAabb (Self : in     btPolyhedralConvexAabbCachingShape'Class;   aabbMin, aabbMax : out math.Vector_3);



end impact.d3.Shape.convex.internal.polyhedral;
