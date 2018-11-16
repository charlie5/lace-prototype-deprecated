--  #include "impact.d3.collision.Algorithm.activating.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.Detector.discrete.gjk_pair.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.Manifold.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.Proxy.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.simplex_Solver.voronoi.h"
--  #include "btCollisionCreateFunc.h"
--  #include "impact.d3.Dispatcher.collision.h"
--  #include "LinearMath/impact.d3.TransformUtil.h" //for btConvexSeparatingDistanceUtil

--
--  class impact.d3.collision.convex_penetration_depth_Solver;


with impact.d3.collision.Algorithm.activating,

     impact.d3.collision.simplex_Solver,
     impact.d3.collision.convex_penetration_depth_Solver,
     impact.d3.Manifold,

     impact.d3.collision.create_Func,

     impact.d3.transform_Util;
with impact.d3.collision.Algorithm;
with impact.d3.Object;
with impact.d3.Dispatcher;
with impact.d3.collision.manifold_Result;




package impact.d3.collision.Algorithm.activating.convex_convex
--
--  The convexConvexAlgorithm collision algorithm implements time of impact, convex closest points and penetration depth calculations between two convex objects.
--
--  Multiple contact points are calculated by perturbing the orientation of the smallest object orthogonal to the separating normal.
--  This idea was described by Gino van den Bergen in this forum topic http://www.bulletphysics.com/Bullet/phpBB3/viewtopic.php?f=4&t=288&p=888#p888
--
is
   use impact.d3.transform_Util,
       Math;


   USE_SEPDISTANCE_UTIL2 : constant Boolean := True;
   --
   --  Enabling USE_SEPDISTANCE_UTIL2 requires 100% reliable distance computation. However, when using large size ratios GJK can be imprecise
   --  so the distance is not conservative. In that case, enabling this USE_SEPDISTANCE_UTIL2 would result in failing/missing collisions.
   --  Either improve GJK for large size ratios (testing a 100 units versus a 0.1 unit object) or only enable the util
   --  for certain pairs that have a small size ratio



   type Item is new impact.d3.collision.Algorithm.activating.item with private;





   --- CreateFunc
   --

   type CreateFunc is new create_Func.item with
      record
         m_pdSolver      : access impact.d3.collision.convex_penetration_depth_Solver.item'Class;
         m_simplexSolver : access impact.d3.collision.simplex_Solver.item'Class;

         m_numPerturbationIterations          : Integer;
         m_minimumPointsPerturbationThreshold : Integer;
      end record;


   function  to_CreateFunc  (simplexSolver : access impact.d3.collision.simplex_Solver.item'Class;
                             pdSolver      : access impact.d3.collision.convex_penetration_depth_Solver.item'Class) return CreateFunc;

   function  new_CreateFunc (simplexSolver : access impact.d3.collision.simplex_Solver.item'Class;
                             pdSolver      : access impact.d3.collision.convex_penetration_depth_Solver.item'Class) return access CreateFunc'Class;

   overriding procedure destruct                 (Self : in out CreateFunc);

   overriding function  CreateCollisionAlgorithm (Self : in     CreateFunc;   info  : in     AlgorithmConstructionInfo;
                                                                   body0,
                                                                   body1 : access impact.d3.Object.item'Class) return impact.d3.Dispatcher.Algorithm_view;



--          struct CreateFunc :public         impact.d3.collision.AlgorithmCreateFunc
--          {
--
--
--                  virtual        impact.d3.collision.Algorithm* CreateCollisionAlgorithm(impact.d3.collision.AlgorithmConstructionInfo& ci, impact.d3.Object* body0,impact.d3.Object* body1)
--                  {
--                          void* mem = ci.m_dispatcher1->allocateCollisionAlgorithm(sizeof(impact.d3.collision.Algorithm.activating.convex_convex));
--                          return new(mem) impact.d3.collision.Algorithm.activating.convex_convex(ci.m_manifold,ci,body0,body1,m_simplexSolver,m_pdSolver,m_numPerturbationIterations,m_minimumPointsPerturbationThreshold);
--                  }
--          };




   --- impact.d3.collision.Algorithm.activating.convex_convex
   --

   function to_convex_convex_Algorithm (mf                                 : access impact.d3.Manifold.item'Class;
                                        ci                                 : in     AlgorithmConstructionInfo;
                                        body0, body1                       : access impact.d3.Object.item'Class;
                                        simplexSolver                      : access impact.d3.collision.simplex_Solver.item'Class;
                                        pdSolver                           : access impact.d3.collision.convex_penetration_depth_Solver.item'Class;
                                        numPerturbationIterations          : in     Integer;
                                        minimumPointsPerturbationThreshold : in     Integer) return impact.d3.collision.Algorithm.activating.convex_convex.item;

   overriding procedure destruct (Self : in out Item);





   overriding procedure processCollision       (Self : in out Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                           dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                           resultOut    :    out impact.d3.collision.manifold_Result.item);

   overriding function  calculateTimeOfImpact  (Self : in     Item;   body0, body1 : access impact.d3.Object.item'Class;
                                                           dispatchInfo : in     impact.d3.Dispatcher.DispatcherInfo;
                                                           resultOut    : access impact.d3.collision.manifold_Result.item) return math.Real;

   overriding procedure getAllContactManifolds (Self : in out Item;   manifoldArray :    out impact.d3.collision.Algorithm.btManifoldArray);

   procedure setLowLevelOfDetail    (Self : in out Item;   useLowLevel : in Boolean);

   function  getManifold            (Self : in     Item) return access impact.d3.Manifold.item'Class;





private


   type Item is new impact.d3.collision.Algorithm.activating.item with
      record
         m_sepDistance      : btConvexSeparatingDistanceUtil;               -- Used by USE_SEPDISTANCE_UTIL2.

         m_simplexSolver    : access impact.d3.collision.simplex_Solver.item'Class;
         m_pdSolver         : access impact.d3.collision.convex_penetration_depth_Solver.item'Class;


         m_ownManifold      : Boolean;
         m_manifoldPtr      : access impact.d3.Manifold.item'Class;
         m_lowLevelOfDetail : Boolean;

         m_numPerturbationIterations          : Integer;
         m_minimumPointsPerturbationThreshold : Integer;

         --  todo: cache separating vector to speedup collision detection
      end record;

end impact.d3.collision.Algorithm.activating.convex_convex;
