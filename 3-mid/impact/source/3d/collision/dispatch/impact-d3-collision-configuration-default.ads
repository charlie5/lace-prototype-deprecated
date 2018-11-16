with -- impact.d3.collision.Configuration,
     impact.d3.Collision.create_Func,
     impact.d3.collision.simplex_Solver.voronoi,
     impact.d3.collision.convex_penetration_depth_Solver;
with impact.d3.collision.Proxy;




package impact.d3.Collision.Configuration.default
is
   type Item is new impact.d3.collision.Configuration.item with private;



   --- construction
   --

   type btDefaultCollisionConstructionInfo is
      record
         m_defaultMaxPersistentManifoldPoolSize   : Integer := 4096;
         m_defaultMaxCollisionAlgorithmPoolSize   : Integer := 4096;
         m_customCollisionAlgorithmMaxElementSize : Integer :=    0;
         m_defaultStackAllocatorSize              : Integer :=    0;
         m_useEpaPenetrationAlgorithm             : Boolean := True;
      end record;

   function to_btDefaultCollisionConstructionInfo return btDefaultCollisionConstructionInfo;



   function   to_default_Configuration (constructionInfo : in btDefaultCollisionConstructionInfo := to_btDefaultCollisionConstructionInfo) return Item;
   function  new_default_Configuration (constructionInfo : in btDefaultCollisionConstructionInfo := to_btDefaultCollisionConstructionInfo) return access Item'Class;

   overriding procedure destruct (Self : in out Item);




   --- attributes
   --

   overriding function getCollisionAlgorithmCreateFunc (Self : in Item;   proxyType0,
                                                               proxyType1 : impact.d3.collision.Proxy.BroadphaseNativeTypes) return access create_Func.item'Class;


   function getSimplexSolver (Self : in Item) return access impact.d3.collision.simplex_Solver.voronoi.item'Class;


--     function getCollisionAlgorithmCreateFunc (Self : in Item;   proxyType0, proxyType1 : in impact.d3.collision.Proxy.BroadphaseNativeTypes) return access btCollisionCreateFunc.impact.d3.collision.AlgorithmCreateFunc;



   procedure setConvexConvexMultipointIterations (Self : in out Item;   numPerturbationIterations          : in Integer := 3;
                                                                        minimumPointsPerturbationThreshold : in Integer := 3);
   --
   --          Use this method to allow to generate multiple contact points between at once, between two objects using the generic convex-convex algorithm.
   --          By default, this feature is disabled for best performance.
   --
   --          'numPerturbationIterations'            controls the number of collision queries. Set it to zero to disable the feature.
   --          'minimumPointsPerturbationThreshold'   is the minimum number of points in the contact cache, above which the feature is disabled.
   --
   --          3 is a good value for both params, if you want to enable the feature. This is because the default contact cache contains
   --   a maximum of 4 points, and one collision query at the unperturbed orientation is performed first.
   --
   --          See Bullet/Demos/CollisionDemo for an example how this feature gathers multiple points.
   --
   --   todo: we could add a per-object setting of those parameters, for level-of-detail collision detection.



   procedure setPlaneConvexMultipointIterations (Self : in out Item;   numPerturbationIterations          : in Integer := 3;
                                                                       minimumPointsPerturbationThreshold : in Integer := 3);





private

--     use create_Func.item;



   type Item is new impact.d3.collision.Configuration.item with
      record
         m_persistentManifoldPoolSize     : Integer;

         m_ownsStackAllocator             : Boolean;
         m_ownsPersistentManifoldPool     : Boolean;
         m_ownsCollisionAlgorithmPool     : Boolean;

         --  default simplex/penetration depth solvers
         m_simplexSolver                  : access impact.d3.collision.simplex_Solver.voronoi.item'Class;
         m_pdSolver                       : access impact.d3.collision.convex_penetration_depth_Solver.item'Class;

         --  default CreationFunctions, filling the m_doubleDispatch table
         m_convexConvexCreateFunc         : access create_Func.item'Class;
         m_convexConcaveCreateFunc        : access create_Func.item'Class;
         m_swappedConvexConcaveCreateFunc : access create_Func.item'Class;
         m_compoundCreateFunc             : access create_Func.item'Class;
         m_swappedCompoundCreateFunc      : access create_Func.item'Class;
         m_emptyCreateFunc                : access create_Func.item'Class;
         m_sphereSphereCF                 : access create_Func.item'Class;

         --  #ifdef USE_BUGGY_SPHERE_BOX_ALGORITHM
         --          impact.d3.collision.AlgorithmCreateFunc* m_sphereBoxCF;
         --          impact.d3.collision.AlgorithmCreateFunc* m_boxSphereCF;
         --  #endif //USE_BUGGY_SPHERE_BOX_ALGORITHM

         m_boxBoxCF                       : access create_Func.item'Class;
         m_sphereTriangleCF               : access create_Func.item'Class;
         m_triangleSphereCF               : access create_Func.item'Class;
         m_planeConvexCF                  : access create_Func.item'Class;
         m_convexPlaneCF                  : access create_Func.item'Class;
      end record;



end impact.d3.Collision.Configuration.default;
