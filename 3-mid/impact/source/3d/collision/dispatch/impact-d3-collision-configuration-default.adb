with impact.d3.collision.convex_penetration_depth_Solver.gjk_epa,
     impact.d3.collision.convex_penetration_depth_Solver.minkowski,

     impact.d3.collision.Algorithm.activating.convex_convex,
     impact.d3.collision.Algorithm.activating.convex_concave,
     impact.d3.collision.Algorithm.activating.compound,
     impact.d3.collision.Algorithm.empty,
     impact.d3.collision.Algorithm.activating.sphere_sphere,
     impact.d3.collision.Algorithm.activating.sphere_triangle,
     impact.d3.collision.Algorithm.activating.box_box,
     impact.d3.collision.Algorithm.convex_plane
;



package body impact.d3.Collision.Configuration.default
is

   --- construction
   --

   function to_btDefaultCollisionConstructionInfo return btDefaultCollisionConstructionInfo
   is
   begin
      return btDefaultCollisionConstructionInfo'(others => <>);
   end to_btDefaultCollisionConstructionInfo;




   function  to_default_Configuration (constructionInfo : in btDefaultCollisionConstructionInfo := to_btDefaultCollisionConstructionInfo) return Item
   is

      Self : Item;

--        maxSize,
--        maxSize2,
--        maxSize3,
--        s1,
--        collisionAlgorithmMaxElementSize : Integer;
   begin
      Self.m_simplexSolver := new impact.d3.collision.simplex_Solver.voronoi.item;

      if constructionInfo.m_useEpaPenetrationAlgorithm then
         Self.m_pdSolver := new impact.d3.collision.convex_penetration_depth_Solver.gjk_epa.item;
      else
         Self.m_pdSolver := new impact.d3.collision.convex_penetration_depth_Solver.minkowski.item;
      end if;

      --  Default CreationFunctions, filling the m_doubleDispatch table.
      --
      Self.m_convexConvexCreateFunc         := impact.d3.collision.Algorithm.activating.convex_convex              .new_CreateFunc (Self.m_simplexSolver.all'Access, Self.m_pdSolver.all'Access).all'Access;
      Self.m_convexConcaveCreateFunc        := new impact.d3.collision.Algorithm.activating.convex_concave.CreateFunc;
      Self.m_swappedConvexConcaveCreateFunc := new impact.d3.collision.Algorithm.activating.convex_concave.SwappedCreateFunc;
      Self.m_compoundCreateFunc             := new impact.d3.collision.Algorithm.activating.compound     .CreateFunc;
      Self.m_swappedCompoundCreateFunc      := new impact.d3.collision.Algorithm.activating.compound     .SwappedCreateFunc;
      Self.m_emptyCreateFunc                := new impact.d3.collision.Algorithm.empty        .CreateFunc;
      Self.m_sphereSphereCF                 := new impact.d3.collision.Algorithm.activating.sphere_sphere .CreateFunc;

      --        if USE_BUGGY_SPHERE_BOX_ALGORITHM then
      --           Self.m_sphereBoxCF           := new btSphereBoxCollisionAlgorithm.new_CreateFunc;
      --           Self.m_boxSphereCF           := new btSphereBoxCollisionAlgorithm.new_CreateFunc;
      --           Self.m_boxSphereCF.m_swapped := true;
      --        end if;

      Self.m_sphereTriangleCF               := new impact.d3.collision.Algorithm.activating.sphere_triangle.CreateFunc;

      Self.m_triangleSphereCF               := new impact.d3.collision.Algorithm.activating.sphere_triangle.CreateFunc;
      Self.m_triangleSphereCF.m_swapped     := True;

      Self.m_boxBoxCF                       := new impact.d3.collision.Algorithm.activating.box_box.CreateFunc;



      --  convex versus plane
      Self.m_convexPlaneCF           := new impact.d3.collision.Algorithm.convex_plane.CreateFunc;
      Self.m_planeConvexCF           := new impact.d3.collision.Algorithm.convex_plane.CreateFunc;
      Self.m_planeConvexCF.m_swapped := True;



      --  calculate maximum element size, big enough to fit any collision algorithm in the memory pool
--        maxSize  := sizeof (impact.d3.collision.Algorithm.activating.convex_convex);
--        maxSize2 := sizeof (impact.d3.collision.Algorithm.activating.convex_concave);
--        maxSize3 := sizeof (impact.d3.collision.Algorithm.activating.compound);
--        sl       := sizeof (btConvexSeparatingDistanceUtil);
--        sl       := sizeof (impact.d3.collision.Detector.discrete.gjk_pair);                  -- tbd: ?!

--        collisionAlgorithmMaxElementSize := btMax (maxSize,constructionInfo.m_customCollisionAlgorithmMaxElementSize);
--        collisionAlgorithmMaxElementSize := btMax (collisionAlgorithmMaxElementSize,maxSize2);
--        collisionAlgorithmMaxElementSize := btMax (collisionAlgorithmMaxElementSize,maxSize3);

--        if constructionInfo.m_stackAlloc then
--           Self.m_ownsStackAllocator := false;
--           Self.m_stackAlloc         := constructionInfo.m_stackAlloc;
--        else
--           Self.m_ownsStackAllocator := true;
--           Self.m_stackAlloc         := new btStackAlloc (constructionInfo.m_defaultStackAllocatorSize);
--        end if;
--
--        if constructionInfo.m_persistentManifoldPool then
--           Self.m_ownsPersistentManifoldPool := false;
--           Self.m_persistentManifoldPool     := constructionInfo.m_persistentManifoldPool;
--        else
--           Self.m_ownsPersistentManifoldPool := true;
--           Self.m_persistentManifoldPool     := new btPoolAllocator (sizeof(impact.d3.Manifold),constructionInfo.m_defaultMaxPersistentManifoldPoolSize);
--        end if;

--        if constructionInfo.m_collisionAlgorithmPool then
--           Self.m_ownsCollisionAlgorithmPool := false;
--           Self.m_collisionAlgorithmPool     := constructionInfo.m_collisionAlgorithmPool;
--        else
--           Self.m_ownsCollisionAlgorithmPool := true;
--           Self.m_collisionAlgorithmPool     := new btPoolAllocator (collisionAlgorithmMaxElementSize,constructionInfo.m_defaultMaxCollisionAlgorithmPoolSize);
--        end if;


      return Self;
   end to_default_Configuration;






   function  new_default_Configuration (constructionInfo : in btDefaultCollisionConstructionInfo := to_btDefaultCollisionConstructionInfo) return access Item'Class
   is
   begin
      return new impact.d3.collision.Configuration.default.item'(to_default_Configuration (constructionInfo));
   end new_default_Configuration;









   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;










   --- Attributes
   --

   overriding function getCollisionAlgorithmCreateFunc (Self : in Item;   proxyType0,
                                             proxyType1 : impact.d3.collision.Proxy.BroadphaseNativeTypes) return access create_Func.item'Class
   is
      use impact.d3.collision.Proxy;
   begin

      if         proxyType0 = SPHERE_SHAPE_PROXYTYPE
        and then proxyType1 = SPHERE_SHAPE_PROXYTYPE
      then
         return        Self.m_sphereSphereCF;
      end if;


      if         proxyType0 = SPHERE_SHAPE_PROXYTYPE
        and then proxyType1 = TRIANGLE_SHAPE_PROXYTYPE
      then
         return        Self.m_sphereTriangleCF;
      end if;


      if         proxyType0 = TRIANGLE_SHAPE_PROXYTYPE
        and then proxyType1 = SPHERE_SHAPE_PROXYTYPE
      then
         return        Self.m_triangleSphereCF;
      end if;


      if         proxyType0 = BOX_SHAPE_PROXYTYPE
        and then proxyType1 = BOX_SHAPE_PROXYTYPE
      then
         return Self.m_boxBoxCF;
      end if;


      if         impact.d3.collision.Proxy.isConvex (proxyType0)
        and then proxyType1 = STATIC_PLANE_PROXYTYPE
      then
         return Self.m_convexPlaneCF;
      end if;


      if         impact.d3.collision.Proxy.isConvex (proxyType1)
        and then proxyType0 = STATIC_PLANE_PROXYTYPE
      then
         return Self.m_planeConvexCF;
      end if;



      if         impact.d3.collision.Proxy.isConvex (proxyType0)
        and then impact.d3.collision.Proxy.isConvex (proxyType1)
      then
         return Self.m_convexConvexCreateFunc;
      end if;


      if         impact.d3.collision.Proxy.isConvex (proxyType0)
        and then impact.d3.collision.Proxy.isConcave (proxyType1)
      then
         return Self.m_convexConcaveCreateFunc;
      end if;


      if         impact.d3.collision.Proxy.isConvex  (proxyType1)
        and then impact.d3.collision.Proxy.isConcave (proxyType0)
      then
         return Self.m_swappedConvexConcaveCreateFunc;
      end if;


      if impact.d3.collision.Proxy.isCompound (proxyType0) then
         return Self.m_compoundCreateFunc;
      else
         if impact.d3.collision.Proxy.isCompound (proxyType1) then
            return Self.m_swappedCompoundCreateFunc;
         end if;
      end if;



      return Self.m_emptyCreateFunc;    -- Failed to find an algorithm.
   end getCollisionAlgorithmCreateFunc;








   function getSimplexSolver (Self : in Item) return access impact.d3.collision.simplex_Solver.voronoi.item'Class
   is
   begin
      raise Program_Error;
      return Self.m_simplexSolver;
   end getSimplexSolver;








   procedure setConvexConvexMultipointIterations (Self : in out Item;   numPerturbationIterations          : in Integer := 3;
                                                                        minimumPointsPerturbationThreshold : in Integer := 3)
   is
   begin
      raise Program_Error;

   end setConvexConvexMultipointIterations;


   procedure setPlaneConvexMultipointIterations (Self : in out Item;   numPerturbationIterations          : in Integer := 3;
                                                                       minimumPointsPerturbationThreshold : in Integer := 3)
   is
   begin
      raise Program_Error;

   end setPlaneConvexMultipointIterations;






--     function getCollisionAlgorithmCreateFunc (Self : in Item;   proxyType0,
--                                                                 proxyType1 : Integer) return btCollisionCreateFunc.impact.d3.collision.AlgorithmCreateFunc'Class
--     is
--     begin
--        null;
--
--     end;




--     function getSimplexSolver (Self : in Item) return access impact.d3.collision.simplex_Solver.voronoi.item'Class
--     is
--     begin
--        return Self.m_simplexSolver;
--     end;


end impact.d3.Collision.Configuration.default;





--  #include "impact.d3.collision.Configuration.default.h"
--
--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Algorithm.activating.convex_convex.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Algorithm.empty.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Algorithm.activating.convex_concave.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Algorithm.activating.compound.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Algorithm.convex_plane.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Algorithm.activating.box_box.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Algorithm.activating.sphere_sphere.h"

--  #ifdef USE_BUGGY_SPHERE_BOX_ALGORITHM
--  #include "BulletCollision/CollisionDispatch/btSphereBoxCollisionAlgorithm.h"
--  #endif //USE_BUGGY_SPHERE_BOX_ALGORITHM

--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Algorithm.activating.sphere_triangle.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.convex_penetration_depth_Solver.gjk_epa.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.convex_penetration_depth_Solver.minkowski.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.simplex_Solver.voronoi.h"
--
--
--
--  #include "LinearMath/btStackAlloc.h"
--  #include "LinearMath/btPoolAllocator.h"




--
--  impact.d3.collision.Configuration.default::impact.d3.collision.Configuration.default(const btDefaultCollisionConstructionInfo& constructionInfo)
--  //impact.d3.collision.Configuration.default::impact.d3.collision.Configuration.default(btStackAlloc*        stackAlloc,btPoolAllocator*        persistentManifoldPool,btPoolAllocator*        collisionAlgorithmPool)
--  {
--
--          void* mem = btAlignedAlloc(sizeof(impact.d3.collision.simplex_Solver.voronoi),16);
--          m_simplexSolver = new (mem)impact.d3.collision.simplex_Solver.voronoi();
--
--          if (constructionInfo.m_useEpaPenetrationAlgorithm)
--          {
--                  mem = btAlignedAlloc(sizeof(impact.d3.collision.convex_penetration_depth_Solver.gjk_epa),16);
--                  m_pdSolver = new (mem)impact.d3.collision.convex_penetration_depth_Solver.gjk_epa;
--          }else
--          {
--                  mem = btAlignedAlloc(sizeof(impact.d3.collision.convex_penetration_depth_Solver.minkowski),16);
--                  m_pdSolver = new (mem)impact.d3.collision.convex_penetration_depth_Solver.minkowski;
--          }
--
--          //default CreationFunctions, filling the m_doubleDispatch table
--          mem = btAlignedAlloc(sizeof(impact.d3.collision.Algorithm.activating.convex_convex::CreateFunc),16);
--          m_convexConvexCreateFunc = new(mem) impact.d3.collision.Algorithm.activating.convex_convex::CreateFunc(m_simplexSolver,m_pdSolver);

--          mem = btAlignedAlloc(sizeof(impact.d3.collision.Algorithm.activating.convex_concave::CreateFunc),16);
--          m_convexConcaveCreateFunc = new (mem)impact.d3.collision.Algorithm.activating.convex_concave::CreateFunc;

--          mem = btAlignedAlloc(sizeof(impact.d3.collision.Algorithm.activating.convex_concave::CreateFunc),16);
--          m_swappedConvexConcaveCreateFunc = new (mem)impact.d3.collision.Algorithm.activating.convex_concave::SwappedCreateFunc;

--          mem = btAlignedAlloc(sizeof(impact.d3.collision.Algorithm.activating.compound::CreateFunc),16);
--          m_compoundCreateFunc = new (mem)impact.d3.collision.Algorithm.activating.compound::CreateFunc;

--          mem = btAlignedAlloc(sizeof(impact.d3.collision.Algorithm.activating.compound::SwappedCreateFunc),16);
--          m_swappedCompoundCreateFunc = new (mem)impact.d3.collision.Algorithm.activating.compound::SwappedCreateFunc;

--          mem = btAlignedAlloc(sizeof(btEmptyAlgorithm::CreateFunc),16);
--          m_emptyCreateFunc = new(mem) btEmptyAlgorithm::CreateFunc;
--
--          mem = btAlignedAlloc(sizeof(impact.d3.collision.Algorithm.activating.sphere_sphere::CreateFunc),16);
--          m_sphereSphereCF = new(mem) impact.d3.collision.Algorithm.activating.sphere_sphere::CreateFunc;

--  #ifdef USE_BUGGY_SPHERE_BOX_ALGORITHM
--          mem = btAlignedAlloc(sizeof(btSphereBoxCollisionAlgorithm::CreateFunc),16);
--          m_sphereBoxCF = new(mem) btSphereBoxCollisionAlgorithm::CreateFunc;
--          mem = btAlignedAlloc(sizeof(btSphereBoxCollisionAlgorithm::CreateFunc),16);
--          m_boxSphereCF = new (mem)btSphereBoxCollisionAlgorithm::CreateFunc;
--          m_boxSphereCF->m_swapped = true;
--  #endif //USE_BUGGY_SPHERE_BOX_ALGORITHM

--
--          mem = btAlignedAlloc(sizeof(impact.d3.collision.Algorithm.activating.sphere_triangle::CreateFunc),16);
--          m_sphereTriangleCF = new (mem)impact.d3.collision.Algorithm.activating.sphere_triangle::CreateFunc;
--          mem = btAlignedAlloc(sizeof(impact.d3.collision.Algorithm.activating.sphere_triangle::CreateFunc),16);
--          m_triangleSphereCF = new (mem)impact.d3.collision.Algorithm.activating.sphere_triangle::CreateFunc;
--          m_triangleSphereCF->m_swapped = true;
--
--          mem = btAlignedAlloc(sizeof(impact.d3.collision.Algorithm.activating.box_box::CreateFunc),16);
--          m_boxBoxCF = new(mem)impact.d3.collision.Algorithm.activating.box_box::CreateFunc;
--
--          //convex versus plane
--          mem = btAlignedAlloc (sizeof(impact.d3.collision.Algorithm.convex_plane::CreateFunc),16);
--          m_convexPlaneCF = new (mem) impact.d3.collision.Algorithm.convex_plane::CreateFunc;
--          mem = btAlignedAlloc (sizeof(impact.d3.collision.Algorithm.convex_plane::CreateFunc),16);
--          m_planeConvexCF = new (mem) impact.d3.collision.Algorithm.convex_plane::CreateFunc;
--          m_planeConvexCF->m_swapped = true;
--
--          ///calculate maximum element size, big enough to fit any collision algorithm in the memory pool
--          int maxSize = sizeof(impact.d3.collision.Algorithm.activating.convex_convex);
--          int maxSize2 = sizeof(impact.d3.collision.Algorithm.activating.convex_concave);
--          int maxSize3 = sizeof(impact.d3.collision.Algorithm.activating.compound);
--          int sl = sizeof(btConvexSeparatingDistanceUtil);
--          sl = sizeof(impact.d3.collision.Detector.discrete.gjk_pair);
--          int        collisionAlgorithmMaxElementSize = btMax(maxSize,constructionInfo.m_customCollisionAlgorithmMaxElementSize);
--          collisionAlgorithmMaxElementSize = btMax(collisionAlgorithmMaxElementSize,maxSize2);
--          collisionAlgorithmMaxElementSize = btMax(collisionAlgorithmMaxElementSize,maxSize3);
--
--          if (constructionInfo.m_stackAlloc)
--          {
--                  m_ownsStackAllocator = false;
--                  this->m_stackAlloc = constructionInfo.m_stackAlloc;
--          } else
--          {
--                  m_ownsStackAllocator = true;
--                  void* mem = btAlignedAlloc(sizeof(btStackAlloc),16);
--                  m_stackAlloc = new(mem)btStackAlloc(constructionInfo.m_defaultStackAllocatorSize);
--          }
--
--          if (constructionInfo.m_persistentManifoldPool)
--          {
--                  m_ownsPersistentManifoldPool = false;
--                  m_persistentManifoldPool = constructionInfo.m_persistentManifoldPool;
--          } else
--          {
--                  m_ownsPersistentManifoldPool = true;
--                  void* mem = btAlignedAlloc(sizeof(btPoolAllocator),16);
--                  m_persistentManifoldPool = new (mem) btPoolAllocator(sizeof(impact.d3.Manifold),constructionInfo.m_defaultMaxPersistentManifoldPoolSize);
--          }
--
--          if (constructionInfo.m_collisionAlgorithmPool)
--          {
--                  m_ownsCollisionAlgorithmPool = false;
--                  m_collisionAlgorithmPool = constructionInfo.m_collisionAlgorithmPool;
--          } else
--          {
--                  m_ownsCollisionAlgorithmPool = true;
--                  void* mem = btAlignedAlloc(sizeof(btPoolAllocator),16);
--                  m_collisionAlgorithmPool = new(mem) btPoolAllocator(collisionAlgorithmMaxElementSize,constructionInfo.m_defaultMaxCollisionAlgorithmPoolSize);
--          }
--
--
--  }





--  impact.d3.collision.Configuration.default::~impact.d3.collision.Configuration.default()
--  {
--          if (m_ownsStackAllocator)
--          {
--                  m_stackAlloc->destroy();
--                  m_stackAlloc->~btStackAlloc();
--                  btAlignedFree(m_stackAlloc);
--          }
--          if (m_ownsCollisionAlgorithmPool)
--          {
--                  m_collisionAlgorithmPool->~btPoolAllocator();
--                  btAlignedFree(m_collisionAlgorithmPool);
--          }
--          if (m_ownsPersistentManifoldPool)
--          {
--                  m_persistentManifoldPool->~btPoolAllocator();
--                  btAlignedFree(m_persistentManifoldPool);
--          }
--
--          m_convexConvexCreateFunc->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree(        m_convexConvexCreateFunc);
--
--          m_convexConcaveCreateFunc->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_convexConcaveCreateFunc);
--          m_swappedConvexConcaveCreateFunc->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_swappedConvexConcaveCreateFunc);
--
--          m_compoundCreateFunc->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_compoundCreateFunc);
--
--          m_swappedCompoundCreateFunc->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_swappedCompoundCreateFunc);
--
--          m_emptyCreateFunc->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_emptyCreateFunc);
--
--          m_sphereSphereCF->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_sphereSphereCF);
--
--  #ifdef USE_BUGGY_SPHERE_BOX_ALGORITHM
--          m_sphereBoxCF->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_sphereBoxCF);
--          m_boxSphereCF->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_boxSphereCF);
--  #endif //USE_BUGGY_SPHERE_BOX_ALGORITHM
--
--          m_sphereTriangleCF->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_sphereTriangleCF);
--          m_triangleSphereCF->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_triangleSphereCF);
--          m_boxBoxCF->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_boxBoxCF);
--
--          m_convexPlaneCF->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_convexPlaneCF);
--          m_planeConvexCF->~impact.d3.collision.AlgorithmCreateFunc();
--          btAlignedFree( m_planeConvexCF);
--
--          m_simplexSolver->~impact.d3.collision.simplex_Solver.voronoi();
--          btAlignedFree(m_simplexSolver);
--
--          m_pdSolver->~impact.d3.collision.convex_penetration_depth_Solver();
--
--          btAlignedFree(m_pdSolver);
--
--
--  }










--  void impact.d3.collision.Configuration.default::setConvexConvexMultipointIterations(int numPerturbationIterations, int minimumPointsPerturbationThreshold)
--  {
--          impact.d3.collision.Algorithm.activating.convex_convex::CreateFunc* convexConvex = (impact.d3.collision.Algorithm.activating.convex_convex::CreateFunc*) m_convexConvexCreateFunc;
--          convexConvex->m_numPerturbationIterations = numPerturbationIterations;
--          convexConvex->m_minimumPointsPerturbationThreshold = minimumPointsPerturbationThreshold;
--  }




--  void        impact.d3.collision.Configuration.default::setPlaneConvexMultipointIterations(int numPerturbationIterations, int minimumPointsPerturbationThreshold)
--  {
--          impact.d3.collision.Algorithm.convex_plane::CreateFunc* planeCreateFunc = (impact.d3.collision.Algorithm.convex_plane::CreateFunc*)m_planeConvexCF;
--          planeCreateFunc->m_numPerturbationIterations = numPerturbationIterations;
--          planeCreateFunc->m_minimumPointsPerturbationThreshold = minimumPointsPerturbationThreshold;
--  }
