with impact.d3.collision.Algorithm.activating,
     impact.d3.Manifold,
     impact.d3.collision.manifold_Result,

     impact.d3.Dispatcher,
     impact.d3.Object,
     impact.d3.collision.create_Func,
     impact.d3.collision.Algorithm,

     impact.d3.triangle_Callback;



package impact.d3.collision.Algorithm.activating.convex_concave
--
--  impact.d3.collision.Algorithm.activating.convex_concave  supports collision between convex shapes and (concave) trianges meshes.
--
is




   --  For each triangle in the concave mesh that overlaps with the AABB of a convex (m_convexProxy), processTriangle is called.
   --
   type btConvexTriangleCallback is new impact.d3.triangle_Callback.item with private;




   function to_btConvexTriangleCallback (dispatcher   : access impact.d3.Dispatcher.item'Class;
                                         body0, body1 : access impact.d3.Object.item'Class;
                                         isSwapped    : in     Boolean                   ) return btConvexTriangleCallback;

   overriding procedure destruct               (Self : in out btConvexTriangleCallback);


   procedure setTimeStepAndCounters (Self : in out btConvexTriangleCallback;   collisionMarginTriangle : in     math.Real;
                                                                               dispatchInfo            : in     impact.d3.Dispatcher.DispatcherInfo;
                                                                               resultOut               : access impact.d3.collision.manifold_Result.item);

   overriding procedure processTriangle        (Self : in out btConvexTriangleCallback;   triangle      : access math.Matrix_3x3;
                                                                               partId        : in     Integer;
                                                                               triangleIndex : in     Integer);

   procedure clearCache             (Self : in out btConvexTriangleCallback);


   function getAabbMin              (Self : in btConvexTriangleCallback) return math.Vector_3;
   function getAabbMax              (Self : in btConvexTriangleCallback) return math.Vector_3;

   function m_triangleCount         (Self : in btConvexTriangleCallback) return Integer;
   function m_manifoldPtr           (Self : in btConvexTriangleCallback) return access impact.d3.Manifold.item'Class;











   --- impact.d3.collision.Algorithm.activating.convex_concave
   --

   type Item is new impact.d3.collision.Algorithm.activating.item with private;



   function to_convex_concave_Algorithm (ci           : in     AlgorithmConstructionInfo;
                                         body0, body1 : access impact.d3.Object.item'Class;
                                         isSwapped    : in     Boolean                                                ) return Item'Class;

   overriding procedure destruct (Self : in out Item);


   overriding procedure processCollision (Self : in out Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    :    out impact.d3.collision.manifold_Result.item);


   overriding function calculateTimeOfImpact (Self : in Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                     dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                     resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real;

   overriding
   procedure getAllContactManifolds (Self : in out Item;   manifoldArray : out impact.d3.collision.Algorithm.btManifoldArray);


   procedure clearCache (Self : in out Item);





   --- Create Functions
   --

   type CreateFunc        is new Create_Func.item with null record;


   overriding
   function CreateCollisionAlgorithm (Self : in CreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                              body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view;



   type SwappedCreateFunc is new Create_Func.item with null record;

   overriding
   function CreateCollisionAlgorithm (Self : in SwappedCreateFunc;   ci           : in     AlgorithmConstructionInfo;
                                                                     body0, body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view;








private




   type btConvexTriangleCallback is new impact.d3.triangle_Callback.item with
      record
         m_convexBody              : access impact.d3.Object.item'Class;
         m_triBody                 : access impact.d3.Object.item'Class;

         m_aabbMin,
         m_aabbMax                 : math.Vector_3;

         m_resultOut               : access impact.d3.collision.manifold_Result.item;
         m_dispatcher              : access impact.d3.Dispatcher.item'Class;
         m_dispatchInfoPtr         : access constant impact.d3.Dispatcher.DispatcherInfo;
         m_collisionMarginTriangle : math.Real;

         m_triangleCount           : Integer;

         m_manifoldPtr             : access impact.d3.Manifold.item;
      end record;





   type Item is new impact.d3.collision.Algorithm.activating.item with
      record
         m_isSwapped                : Boolean;
         m_btConvexTriangleCallback : btConvexTriangleCallback;

      end record;






end impact.d3.collision.Algorithm.activating.convex_concave;
