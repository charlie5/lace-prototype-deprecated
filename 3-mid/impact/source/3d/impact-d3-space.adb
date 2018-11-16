with impact.d3.triangle_Callback.raycast;

with impact.d3.Shape.convex.internal.sphere;
with impact.d3.collision.convex_Raycast;
with impact.d3.collision.convex_Raycast.subsimplex;
with impact.d3.Shape.concave.triangle_mesh.bvh;

with impact.d3.Transform;
with impact.d3.Vector;
with System;
with impact.d3.Manifold;
with impact.d3.collision.simplex_Solver.voronoi;
with impact.d3.Shape.concave.triangle_mesh;
with impact.d3.Shape.concave;
with impact.d3.collision.bounding_volume_Tree;
with impact.d3.Shape.compound;
with Ada.Text_IO; use Ada.Text_IO;
with impact.d3.collision.Proxy;


--  #include "impact.d3.Space.h"
--  #include "impact.d3.Dispatcher.collision.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.Object.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.convex_penetration_depth_Solver.gjk_epa.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.convex.internal.sphere.h" //for raycasting
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.concave.triangle_mesh.bvh.h" //for raycasting
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.triangle_Callback.raycast.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.Shape.compound.h"
--  #include "BulletCollision/NarrowPhaseCollision/btSubSimplexConvexCast.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.convex_Raycast.gjk.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.collision.convex_Raycast.continuous_convex.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.Algorithm.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.Broadphase.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.collision.bounding_volume_Tree.h"
--  #include "LinearMath/btAabbUtil2.h"
--  #include "LinearMath/btQuickprof.h"
--  #include "LinearMath/btStackAlloc.h"
--  #include "LinearMath/btSerializer.h"
--  #include "BulletCollision/CollisionShapes/impact.d3.convex_Polyhedron.h"

--  //When the user doesn't provide dispatcher or broadphase, create basic versions (and delete them in destructor)
--  #include "BulletCollision/CollisionDispatch/impact.d3.Dispatcher.collision.h"
--  #include "BulletCollision/BroadphaseCollision/btSimpleBroadphase.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.collision.Configuration.h"




package body impact.d3.Space
is


   --- Constants
   --


   --  //#define DISABLE_DBVT_COMPOUNDSHAPE_RAYCAST_ACCELERATION


   --  //#define USE_BRUTEFORCE_RAYBROADPHASE 1
   --  //RECALCULATE_AABB is slower, but benefit is that you don't need to call 'stepSimulation'  or 'updateAabbs' before using a rayTest
   --  //#define RECALCULATE_AABB_RAYCAST 1







   --- Forge
   --

--     package body Forge
--     is
--
--        function to_impact.d3.Space (dispatcher             : access impact.d3.Dispatcher.impact.d3.Dispatcher'Class;
--                                      broadphasePairCache    : access impact.d3.collision.Broadphase.item'Class;
--                                      collisionConfiguration : access impact.d3.collision.Configuration.item'Class) return Item'Class
--        is
--           Self : Item;
--        begin
--           define (Self,   dispatcher, broadphasePairCache, collisionConfiguration);
--
--           Self.m_dispatcher1         := dispatcher;
--           Self.m_broadphasePairCache := broadphasePairCache;
--           Self.m_forceUpdateAllAabbs := True;
--
--           return Self;
--        end;
--
--
--
--        procedure define  (Self : out Iten;   dispatcher             : access impact.d3.Dispatcher.impact.d3.Dispatcher'Class;
--                                              broadphasePairCache    : access impact.d3.collision.Broadphase.item'Class;
--                                              collisionConfiguration : access impact.d3.collision.Configuration.item'Class)
--        is
--        begin
--           Self.m_dispatcher1         := dispatcher;
--           Self.m_broadphasePairCache := broadphasePairCache;
--           Self.m_forceUpdateAllAabbs := True;
--
--        end;
--
--     end Forge;





   procedure destroy (Self : in out Item)
   is
   begin
      --  clean up remaining objects
      --
      for i in 1 .. Integer (Self.m_collisionObjects.Length)
      loop
         declare
            collisionObject : constant access impact.d3.Object.item'Class := Self.m_collisionObjects.Element (i);
            bp              : constant access impact.d3.collision.Proxy.item'Class := collisionObject.getBroadphaseHandle;
         begin
            if bp /= null then   -- only clear the cached algorithms
               Self.getBroadphase.all.getOverlappingPairCache.cleanProxyFromPairs (bp, Self.m_dispatcher1);
               Self.getBroadphase                            .destroyProxy        (bp, Self.m_dispatcher1);
               collisionObject                               .setBroadphaseHandle (null);
            end if;
         end;
      end loop;
   end destroy;








   --- Attributes
   --

   procedure setBroadphase (Self : in out Item;   pairCache : access impact.d3.collision.Broadphase.item'Class)
   is
   begin
      Self.m_broadphasePairCache := pairCache;
   end setBroadphase;






   function  getBroadphase (Self : in     Item)        return access impact.d3.collision.Broadphase.item'Class
   is
   begin
      return Self.m_broadphasePairCache;
   end getBroadphase;






   function  getPairCache (Self : in     Item)        return access impact.d3.collision.overlapped_pair_Callback.cached.item'Class
   is
   begin
      return Self.m_broadphasePairCache.getOverlappingPairCache;
   end getPairCache;





   function  getDispatcher (Self : in     Item)        return access impact.d3.Dispatcher.item'Class
   is
   begin
      return Self.m_dispatcher1;
   end getDispatcher;






   procedure updateSingleAabb (Self : in out Item;   colObj : access impact.d3.Object.item'Class)
   is
      use impact.d3.Manifold, impact.d3.Vector;
      use type Flags;

      minAabb, maxAabb : math.Vector_3;
      contactThreshold : math.Vector_3;

      bp               : access impact.d3.collision.Broadphase.Item'Class;

   begin
      colObj.getCollisionShape.getAabb (colObj.getWorldTransform.all,  minAabb, maxAabb);

      --  need to increase the aabb for contact thresholds
      contactThreshold := (gContactBreakingThreshold,
                           gContactBreakingThreshold,
                           gContactBreakingThreshold);

      minAabb := minAabb - contactThreshold;
      maxAabb := maxAabb + contactThreshold;


      if         Self.getDispatchInfo.m_useContinuous
        and then colObj.getInternalType = impact.d3.Object.CO_RIGID_BODY
      then
         declare
            minAabb2, maxAabb2 : math.Vector_3;
         begin
            colObj.getCollisionShape.getAabb (colObj.getInterpolationWorldTransform,  minAabb2, maxAabb2);

            minAabb2 := minAabb2 - contactThreshold;
            maxAabb2 := maxAabb2 + contactThreshold;

            setMin (minAabb,  minAabb2);
            setMax (maxAabb,  maxAabb2);
         end;
      end if;


      bp := Self.m_broadphasePairCache;


      --  moving objects should be moderately sized, probably something wrong if not
      --
      if        colObj.isStaticObject
        or else length2 (maxAabb - minAabb)  <  1.0e12
      then
         bp.setAabb (colObj.getBroadphaseHandle,  minAabb, maxAabb,  Self.m_dispatcher1);

      else   -- something went wrong, investigate
         --  raise program_Error with "something went wrong, investigate";
         colObj.setActivationState (impact.d3.Object.DISABLE_SIMULATION);
      end if;

   exception
      when others =>
         put_Line ("Exception in imapce.d3.space.updateSingleAabb !");
   end updateSingleAabb;












   procedure updateAabbs      (Self : in out Item)
   is
--        predictedTrans : Transform_3d;
      colObj         : access impact.d3.Object.item'Class;
   begin

      for i in 1 .. Integer (Self.m_collisionObjects.Length)
      loop
         colObj := Self.m_collisionObjects.Element (i);

         --  only update aabb of active objects
         --
         if        Self.m_forceUpdateAllAabbs
           or else colObj.isActive
         then
            Self.updateSingleAabb (colObj);
         end if;
      end loop;

   end updateAabbs;









   --- LocalRayResult
   --

   function to_LocalRayResult (collisionObject : access impact.d3.Object.item'Class;
                               localShapeInfo  : access impact.d3.Space.LocalShapeInfo;
                               hitNormalLocal  : in     math.Vector_3;
                               hitFraction     : in     math.Real) return LocalRayResult
   is
      Self : LocalRayResult;
   begin
      Self.m_collisionObject := collisionObject;
      Self.m_localShapeInfo  := localShapeInfo;
      Self.m_hitNormalLocal  := hitNormalLocal;
      Self.m_hitFraction     := hitFraction;

      return Self;
   end to_LocalRayResult;












   --- RayResultCallback
   --

   function  hasHit   (Self : in     RayResultCallback) return Boolean
   is
   begin
      return Self.m_collisionObject /= null;
   end hasHit;





   function  needsCollision (Self : in     RayResultCallback;   proxy0 : access impact.d3.collision.Proxy.item'Class) return Boolean
   is
      use type impact.d3.collision.Proxy.CollisionFilterGroups;
      collides : Boolean := (proxy0.m_collisionFilterGroup and Self.m_collisionFilterMask)  /=  0;
   begin
      collides := collides
                  and then ((Self.m_collisionFilterGroup and proxy0.m_collisionFilterMask) /= 0);

      return Collides;
   end needsCollision;















   --- ClosestRayResultCallback
   --


   function to_ClosestRayResultCallback (rayFromWorld,
                                         rayToWorld  : in math.Vector_3) return ClosestRayResultCallback
   is
      Self : ClosestRayResultCallback;
   begin
      Self.m_rayFromWorld := rayFromWorld;
      Self.m_rayToWorld   := rayToWorld;

      return Self;
   end to_ClosestRayResultCallback;





   overriding function  addSingleResult (Self : access ClosestRayResultCallback;   rayResult          : access LocalRayResult;
                                                                        normalInWorldSpace : in     Boolean     ) return math.Real
   is
      --  caller already does the filter on the m_closestHitFraction
      pragma Assert (rayResult.m_hitFraction <= Self.m_closestHitFraction);

      use impact.d3.Transform, impact.d3.Vector;

   begin
      Self.m_closestHitFraction := rayResult.m_hitFraction;
      Self.m_collisionObject    := rayResult.m_collisionObject;


      if normalInWorldSpace then
         Self.m_hitNormalWorld := rayResult.m_hitNormalLocal;

      else   -- need to transform normal into worldspace
         Self.m_hitNormalWorld := getBasis (Self.m_collisionObject.getWorldTransform).all * rayResult.m_hitNormalLocal;
      end if;


      setInterpolate3 (Self.m_hitPointWorld,   Self.m_rayFromWorld,  Self.m_rayToWorld,
                                               rayResult.m_hitFraction);

      return rayResult.m_hitFraction;
   end addSingleResult;









   --- AllHitsRayResultCallback
   --



   function to_AllHitsRayResultCallback (rayFromWorld,
                                         rayToWorld  : in math.Vector_3) return AllHitsRayResultCallback
   is
      Self : AllHitsRayResultCallback;
   begin
      Self.m_rayFromWorld := rayFromWorld;
      Self.m_rayToWorld   := rayToWorld;

      return Self;
   end to_AllHitsRayResultCallback;






   overriding function  addSingleResult (Self : access AllHitsRayResultCallback;   rayResult          : access LocalRayResult;
                                                                        normalInWorldSpace : in     Boolean     ) return math.Real
   is
      use impact.d3.Transform, impact.d3.Vector;

      hitNormalWorld,
      hitPointWorld : math.Vector_3;

   begin
      Self.m_collisionObject := rayResult.m_collisionObject;
      Self.m_collisionObjects.append (rayResult.m_collisionObject.all'Access);


      if normalInWorldSpace then
         hitNormalWorld := rayResult.m_hitNormalLocal;

      else   -- need to transform normal into worldspace
         hitNormalWorld := getBasis (Self.m_collisionObject.getWorldTransform).all  *  rayResult.m_hitNormalLocal;
      end if;


      Self.m_hitNormalWorld.append (hitNormalWorld);

      setInterpolate3 (hitPointWorld,   Self.m_rayFromWorld,
                                        Self.m_rayToWorld,
                                        rayResult.m_hitFraction);

      Self.m_hitPointWorld.append (hitPointWorld);
      Self.m_hitFractions .append (rayResult.m_hitFraction);


      return Self.m_closestHitFraction;
   end addSingleResult;










   --- LocalConvexResult
   --


   function to_LocalConvexResult (hitCollisionObject : access impact.d3.Object.item;
                                  localShapeInfo     : access impact.d3.Space.LocalShapeInfo;

                                  hitNormalLocal,
                                  hitPointLocal      : in     math.Vector_3;

                                  hitFraction        : in     math.Real) return LocalConvexResult
   is
      Self : LocalConvexResult;
   begin
      Self.m_hitCollisionObject := hitCollisionObject;
      Self.m_localShapeInfo     := localShapeInfo;
      Self.m_hitNormalLocal     := hitNormalLocal;
      Self.m_hitPointLocal      := hitPointLocal;
      Self.m_hitFraction        := hitFraction;

      return Self;
   end to_LocalConvexResult;













   --- ConvexResultCallback
   --



   function  hasHit   (Self : in     ConvexResultCallback) return Boolean
   is
   begin
      return Self.m_closestHitFraction < 1.0;
   end hasHit;




   function  needsCollision (Self : in     ConvexResultCallback;   proxy0 : access impact.d3.collision.Proxy.item'Class) return Boolean
   is
      use impact.d3.collision.Proxy;

      collides : Boolean := (proxy0.m_collisionFilterGroup and Self.m_collisionFilterMask) /= 0;
   begin
      collides := collides and then (Self.m_collisionFilterGroup and proxy0.m_collisionFilterMask) /= 0;

      return collides;
   end needsCollision;











   --- ClosestConvexResultCallback
   --

   package body Forge
   is

      function to_Space (dispatcher             : access impact.d3.Dispatcher.item'Class;
                         broadphasePairCache    : access impact.d3.collision.Broadphase.item'Class;
                         collisionConfiguration : access impact.d3.collision.Configuration.item'Class) return Item'Class
      is
         Self : Item;
      begin
         define (Self,   dispatcher, broadphasePairCache, collisionConfiguration);
         return  Self;
      end to_Space;




      procedure define (Self : out Item;   dispatcher             : access impact.d3.Dispatcher.item'Class;
                                            broadphasePairCache    : access impact.d3.collision.Broadphase.item'Class;
                        collisionConfiguration : access impact.d3.collision.Configuration.item'Class)
      is
         pragma Unreferenced (collisionConfiguration);
      begin
         Self.m_dispatcher1         := dispatcher;
         Self.m_broadphasePairCache := broadphasePairCache;
         Self.m_forceUpdateAllAabbs := True;
      end define;



      function to_ClosestConvexResultCallback (convexFromWorld, convexToWorld : in math.Vector_3) return ClosestConvexResultCallback
      is
         Self : ClosestConvexResultCallback;
      begin
         Self.m_convexFromWorld    := convexFromWorld;
         Self.m_convexToWorld      := convexToWorld;
         Self.m_hitCollisionObject := null;

         return Self;
      end to_ClosestConvexResultCallback;

   end Forge;




   overriding function  addSingleResult (Self : access ClosestConvexResultCallback;   convexResult       : access LocalConvexResult;
                                                                           normalInWorldSpace : in     Boolean        ) return math.Real
   is
      use impact.d3.Transform;

      --  caller already does the filter on the m_closestHitFraction
      pragma Assert (convexResult.m_hitFraction <= Self.m_closestHitFraction);

   begin
      Self.m_closestHitFraction := convexResult.m_hitFraction;
      Self.m_hitCollisionObject := convexResult.m_hitCollisionObject;


      if normalInWorldSpace then
         Self.m_hitNormalWorld := convexResult.m_hitNormalLocal;

      else   -- need to transform normal into worldspace
         Self.m_hitNormalWorld := getBasis (Self.m_hitCollisionObject.getWorldTransform).all  *  convexResult.m_hitNormalLocal;
      end if;


      Self.m_hitPointWorld := convexResult.m_hitPointLocal;


      return convexResult.m_hitFraction;
   end addSingleResult;















   --- ContactResultCallback
   --



   function  needsCollision (Self : in     ContactResultCallback;   proxy0 : access impact.d3.collision.Proxy.item) return Boolean
   is
      use impact.d3.collision.Proxy;
      collides : Boolean := (proxy0.m_collisionFilterGroup and Self.m_collisionFilterMask)  /=  0;
   begin
      collides := collides
                  and then (Self.m_collisionFilterGroup and proxy0.m_collisionFilterMask) /= 0;

      return collides;
   end needsCollision;







   function  getNumCollisionObjects (Self : in Item) return Natural
   is
   begin
      return Natural (Self.m_collisionObjects.Length);
   end getNumCollisionObjects;





   ------------
   --- Raytest
   --

   type btSingleRayCallback is new collision.Broadphase.btBroadphaseRayCallback with
      record
         m_rayFromWorld,
         m_rayToWorld     :  Vector_3;

         m_rayFromTrans,
         m_rayToTrans     : aliased Transform_3d;

         m_hitNormal      : Vector_3;

         m_world          : Space.view;
         m_resultCallback : RayResultCallback_view;
      end record;

   overriding
   function  process  (Self : access btSingleRayCallback;   proxy : in  impact.d3.collision.Proxy.item'Class) return Boolean;


   overriding function  process  (Self : access btSingleRayCallback;   proxy : in  impact.d3.collision.Proxy.item'Class) return Boolean
   is
      collisionObject : Object.view;

   begin
      --  terminate further ray tests, once the closestHitFraction reached zero
      --
      if Self.m_resultCallback.m_closestHitFraction = 0.0 then
         return False;
      end if;

      collisionObject := Object.view (proxy.m_clientObject);

      if Self.m_resultCallback.needsCollision (collisionObject.getBroadphaseHandle) then   -- only perform raycast if filterMask matches
         --  Culling already done by broadphase.

         rayTestSingle (Self.m_rayFromTrans'Access, Self.m_rayToTrans'Access,
                        collisionObject,
                        collisionObject.getCollisionShape,
                        collisionObject.getWorldTransform,
                        Self.m_resultCallback);
      end if;

      return True;
   end process;



--          virtual bool        process(const impact.d3.collision.Proxy* proxy)
--          {
--                  ///terminate further ray tests, once the closestHitFraction reached zero
--                  if (m_resultCallback.m_closestHitFraction == impact.d3.Scalar(0.f))
--                          return false;
--
--                  impact.d3.Object*        collisionObject = (impact.d3.Object*)proxy->m_clientObject;
--
--                  //only perform raycast if filterMask matches
--                  if(m_resultCallback.needsCollision(collisionObject->getBroadphaseHandle()))
--                  {
--                          //RigidcollisionObject* collisionObject = ctrl->GetRigidcollisionObject();
--                          //impact.d3.Vector collisionObjectAabbMin,collisionObjectAabbMax;
--  #if 0
--  #ifdef RECALCULATE_AABB
--                          impact.d3.Vector collisionObjectAabbMin,collisionObjectAabbMax;
--                          collisionObject->getCollisionShape()->getAabb(collisionObject->getWorldTransform(),collisionObjectAabbMin,collisionObjectAabbMax);
--  #else
--                          //getBroadphase()->getAabb(collisionObject->getBroadphaseHandle(),collisionObjectAabbMin,collisionObjectAabbMax);
--                          const impact.d3.Vector& collisionObjectAabbMin = collisionObject->getBroadphaseHandle()->m_aabbMin;
--                          const impact.d3.Vector& collisionObjectAabbMax = collisionObject->getBroadphaseHandle()->m_aabbMax;
--  #endif
--  #endif
--                          //impact.d3.Scalar hitLambda = m_resultCallback.m_closestHitFraction;
--                          //culling already done by broadphase
--                          //if (btRayAabb(m_rayFromWorld,m_rayToWorld,collisionObjectAabbMin,collisionObjectAabbMax,hitLambda,m_hitNormal))
--                          {
--                                  m_world->rayTestSingle(m_rayFromTrans,m_rayToTrans,
--                                          collisionObject,
--                                          collisionObject->getCollisionShape(),
--                                          collisionObject->getWorldTransform(),
--                                          m_resultCallback);
--                          }
--                  }
--                  return true;
--          }


   function to_btSingleRayCallback (rayFromWorld, rayToWorld : in Vector_3;
                                    world                    : in Space.view;
                                    resultCallback           : in RayResultCallback_view) return btSingleRayCallback
   is
      use impact.d3.Vector,  math.Algebra.linear;

      Self   : btSingleRayCallback;
      rayDir : Vector_3           := Normalised (rayToWorld - rayFromWorld);

   begin
      Self.m_rayFromWorld   := rayFromWorld;
      Self.m_rayToWorld     := rayToWorld;

      Self.m_world          := world;
      Self.m_resultCallback := resultCallback;

      Self.m_rayFromTrans             := null_Transform_3d;
      Self.m_rayFromTrans.Translation := Self.m_rayFromWorld;

      Self.m_rayToTrans               := null_Transform_3d;
      Self.m_rayToTrans.Translation   := Self.m_rayToWorld;


      --  what about division by zero? --> just set rayDirection[i] to INF/BT_LARGE_FLOAT
      Self.m_rayDirectionInverse (1) := (if rayDir (1) = 0.0 then BT_LARGE_FLOAT else 1.0 / rayDir (1));
      Self.m_rayDirectionInverse (2) := (if rayDir (2) = 0.0 then BT_LARGE_FLOAT else 1.0 / rayDir (2));
      Self.m_rayDirectionInverse (3) := (if rayDir (3) = 0.0 then BT_LARGE_FLOAT else 1.0 / rayDir (3));

      Self.m_signs (1) := Boolean'Pos (Self.m_rayDirectionInverse (1) < 0.0);
      Self.m_signs (2) := Boolean'Pos (Self.m_rayDirectionInverse (2) < 0.0);
      Self.m_signs (3) := Boolean'Pos (Self.m_rayDirectionInverse (3) < 0.0);

      Self.m_lambda_max := dot (rayDir,  Self.m_rayToWorld - Self.m_rayFromWorld);


      return Self;
   end to_btSingleRayCallback;


--  struct btSingleRayCallback : public btBroadphaseRayCallback
--  {
--
--
--          btSingleRayCallback(const impact.d3.Vector& rayFromWorld,const impact.d3.Vector& rayToWorld,
--                         const impact.d3.Space* world,impact.d3.Space::RayResultCallback& resultCallback)
--                  :m_rayFromWorld(rayFromWorld),
--                  m_rayToWorld(rayToWorld),
--                  m_world(world),
--                  m_resultCallback(resultCallback)
--          {
--                  m_rayFromTrans.setIdentity();
--                  m_rayFromTrans.setOrigin(m_rayFromWorld);
--                  m_rayToTrans.setIdentity();
--                  m_rayToTrans.setOrigin(m_rayToWorld);
--
--                  impact.d3.Vector rayDir = (rayToWorld-rayFromWorld);
--
--                  rayDir.normalize ();
--                  ///what about division by zero? --> just set rayDirection[i] to INF/BT_LARGE_FLOAT
--                  m_rayDirectionInverse[0] = rayDir[0] == impact.d3.Scalar(0.0) ? impact.d3.Scalar(BT_LARGE_FLOAT) : impact.d3.Scalar(1.0) / rayDir[0];
--                  m_rayDirectionInverse[1] = rayDir[1] == impact.d3.Scalar(0.0) ? impact.d3.Scalar(BT_LARGE_FLOAT) : impact.d3.Scalar(1.0) / rayDir[1];
--                  m_rayDirectionInverse[2] = rayDir[2] == impact.d3.Scalar(0.0) ? impact.d3.Scalar(BT_LARGE_FLOAT) : impact.d3.Scalar(1.0) / rayDir[2];
--                  m_signs[0] = m_rayDirectionInverse[0] < 0.0;
--                  m_signs[1] = m_rayDirectionInverse[1] < 0.0;
--                  m_signs[2] = m_rayDirectionInverse[2] < 0.0;
--
--                  m_lambda_max = rayDir.dot(m_rayToWorld-m_rayFromWorld);
--
--          }



--  };




   --  rayTest performs a raycast on all objects in the impact.d3.Space, and calls the resultCallback
   --  This allows for several queries: first hit, all hits, any hit, dependent on the value returned by the callback.
   --
   procedure rayTest (Self : access Item;   rayFromWorld, rayToWorld : in     math.Vector_3;
                                            resultCallback           : access RayResultCallback'Class)
   is
      --  Use the broadphase to accelerate the search for objects, based on their aabb
      --  and for each object with ray-aabb overlap, perform an exact ray test.

      rayCB : aliased btSingleRayCallback := to_btSingleRayCallback (rayFromWorld,
                                                             rayToWorld,
                                                             Space            .view (Self),
                                                             RayResultCallback_view (resultCallback));
      unused : Boolean;
      pragma Unreferenced (unused);

   begin
      for i in 1 .. Self.getNumCollisionObjects
      loop
         unused := rayCB.process (Self.m_collisionObjects.Element (i).getBroadphaseHandle.all);
      end loop;
   end rayTest;




--  void        impact.d3.Space::rayTest(const impact.d3.Vector& rayFromWorld, const impact.d3.Vector& rayToWorld, RayResultCallback& resultCallback) const
--  {
--          //BT_PROFILE("rayTest");
--          btSingleRayCallback rayCB(rayFromWorld,rayToWorld,this,resultCallback);
--
--  #ifndef USE_BRUTEFORCE_RAYBROADPHASE
--          m_broadphasePairCache->rayTest(rayFromWorld,rayToWorld,rayCB);
--  #else
--          for (int i=0;i<this->getNumCollisionObjects();i++)
--          {
--                  rayCB.process(m_collisionObjects[i]->getBroadphaseHandle());
--          }
--  #endif //USE_BRUTEFORCE_RAYBROADPHASE
--
--  }




   --- convexSweepTest
   --

   --  convexTest performs a swept convex cast on all objects in the impact.d3.Space, and calls the resultCallback
   --  This allows for several queries: first hit, all hits, any hit, dependent on the value return by the callback.
   --
   procedure convexSweepTest (Self : in Item;   castShape             : in     impact.d3.Shape.convex.item'Class;
                                                From, To              : in     math.Transform_3d;
                                                resultCallback        : access ConvexResultCallback'Class;
                                                allowedCcdPenetration : in     math.Real := 0.0)
   is
   begin
      raise Program_Error with "tbd";
      return;
   end convexSweepTest;




   --- contactTest
   --

   --  contactTest performs a discrete collision test between colObj against all objects in the impact.d3.Space, and calls the resultCallback.
   --  it reports one or more contact points for every overlapping object (including the one with deepest penetration)
   --
   procedure contactTest     (Self : in Item;   colObj         : in     impact.d3.Object.item'Class;
                                                resultCallback : access ContactResultCallback 'Class)
   is
   begin
      raise Program_Error with "tbd";
      return;
   end contactTest;




   --- contactPairTest
   --

   --  contactPairTest performs a discrete collision test between two collision objects and calls the resultCallback if overlap if detected.
   --  it reports one or more contact points (including the one with deepest penetration)
   --
   procedure contactPairTest (Self : in Item;   colObjA, colObjB : in     impact.d3.Object.item'Class;
                                                resultCallback   : access ContactResultCallback 'Class)
   is
   begin
      raise Program_Error with "tbd";
      return;
   end contactPairTest;




   --- rayTestSingle
   --

   --  rayTestSingle performs a raycast call and calls the resultCallback. It is used internally by rayTest.
   --  In a future implementation, we consider moving the ray test as a virtual method in impact.d3.Shape.
   --  This allows more customization.
   --
   procedure rayTestSingle (rayFromTrans, rayToTrans : access math.Transform_3d;
                            collisionObject          : access impact.d3.Object.item'Class;
                            collisionShape           : access impact.d3.Shape.item'Class;
                            colObjWorldTransform     : access math.Transform_3d;
                            resultCallback           : access RayResultCallback'Class)
   is
      use impact.d3.Shape.convex.internal.sphere, impact.d3.collision.Proxy;

      pointShape : aliased impact.d3.Shape.convex.internal.sphere.item'Class := to_sphere_Shape (0.0);
      castShape  :         impact.d3.Shape.convex.view;

   begin
      pointShape.setMargin (0.0);

      castShape := pointShape'Unchecked_Access;


      if collisionShape.isConvex then
         declare
            use impact.d3.Vector, impact.d3.Transform;

            castResult    : aliased impact.d3.collision.convex_Raycast.castResult;

            convexShape   : constant impact.d3.Shape.convex.view         := impact.d3.Shape.convex.view (collisionShape);
            simplexSolver : aliased impact.d3.collision.simplex_Solver.voronoi.item;

            USE_SUBSIMPLEX_CONVEX_CAST : constant Boolean := True;
            convexCaster               : aliased impact.d3.collision.convex_Raycast.subsimplex.item;

         begin
            castResult.m_fraction := resultCallback.m_closestHitFraction;


            if USE_SUBSIMPLEX_CONVEX_CAST then
               convexCaster := impact.d3.collision.convex_Raycast.subsimplex.to_convex_Raycast (castShape,  convexShape,  simplexSolver'Access);
            else
               null;
               --  impact.d3.collision.convex_Raycast.gjk        convexCaster(castShape,convexShape,&simplexSolver);
               --  impact.d3.collision.convex_Raycast.continuous_convex convexCaster(castShape,convexShape,&simplexSolver,0);
            end if;


            if convexCaster.calcTimeOfImpact (rayFromTrans.all, rayToTrans.all,
                                              colObjWorldTransform.all, colObjWorldTransform.all,
                                              castResult'Access)
            then   -- add hit
               if length2 (castResult.m_normal) > 0.0001 then

                  if castResult.m_fraction < resultCallback.m_closestHitFraction then

                     if USE_SUBSIMPLEX_CONVEX_CAST then
                        --  rotate normal into worldspace
                        castResult.m_normal := getBasis (rayFromTrans.all) * castResult.m_normal;
                     end if;

                     normalize (castResult.m_normal);

                     declare
                        localRayResult : aliased impact.d3.Space.LocalRayResult := to_LocalRayResult (collisionObject,
                                                                                                       null,
                                                                                                       castResult.m_normal,
                                                                                                       castResult.m_fraction);
                        normalInWorldSpace : constant Boolean := True;
                        unused             : math.Real;
                        pragma Unreferenced (unused);
                     begin
                        unused := resultCallback.addSingleResult (localRayResult'Access,  normalInWorldSpace);
                     end;

                  end if;
               end if;
            end if;
         end;


      else
         if collisionShape.isConcave then

            if collisionShape.getShapeType = TRIANGLE_MESH_SHAPE_PROXYTYPE then
               declare
                  use linear_Algebra_3d, impact.d3.Transform;

                  --  optimized version for impact.d3.Shape.concave.triangle_mesh.bvh
                  triangleMesh           : constant access impact.d3.Shape.concave.triangle_mesh.bvh.Item'Class := impact.d3.Shape.concave.triangle_mesh.bvh.view (collisionShape);
                  worldTocollisionObject : constant Transform_3d                  := inverse (colObjWorldTransform.all);

                  rayFromLocal           : constant math.Vector_3                 := worldTocollisionObject * getOrigin (rayFromTrans.all);
                  rayToLocal             : constant math.Vector_3                 := worldTocollisionObject * getOrigin (rayToTrans.all);


                  --  ConvexCast::CastResult
                  --
                  type BridgeTriangleRaycastCallback is new impact.d3.triangle_Callback.raycast.btTriangleRaycastCallback with
                     record
                        m_resultCallback  : access impact.d3.Space.RayResultCallback'Class;
                        m_collisionObject : access impact.d3.Object.item'Class;
                        m_triangleMesh    : access impact.d3.Shape.concave.triangle_mesh.Item'Class;

                        m_colObjWorldTransform : Transform_3d;
                     end record;


                  overriding function reportHit (Self : access BridgeTriangleRaycastCallback;   hitNormalLocal : in math.Vector_3;
                                                                                     hitFraction    : in math.Real;
                                                                                     partId         : in Integer;
                                                                                     triangleIndex  : in Integer) return math.Real;



                  function to_BridgeTriangleRaycastCallback (from, to             : in     math.Vector_3;
                                                             resultCallback       : access impact.d3.Space.RayResultCallback'Class;
                                                             collisionObject      : access impact.d3.Object.item'Class;
                                                             triangleMesh         : access impact.d3.Shape.concave.triangle_mesh.Item'Class;
                                                             colObjWorldTransform : in     Transform_3d) return BridgeTriangleRaycastCallback
                  is
                     Self : BridgeTriangleRaycastCallback := (impact.d3.triangle_Callback.raycast.btTriangleRaycastCallback with
                                                              m_resultCallback       => resultCallback,
                                                              m_collisionObject      => collisionObject,
                                                              m_triangleMesh         => triangleMesh,
                                                              m_colObjWorldTransform => colObjWorldTransform);
                  begin
                     define (Self,  from, to,  resultCallback.m_flags);
                     return  Self;
                  end to_BridgeTriangleRaycastCallback;



                  overriding function reportHit (Self : access BridgeTriangleRaycastCallback;   hitNormalLocal : in math.Vector_3;
                                                                                     hitFraction    : in math.Real;
                                                                                     partId         : in Integer;
                                                                                     triangleIndex  : in Integer) return math.Real
                  is
                     shapeInfo          : aliased impact.d3.Space.LocalShapeInfo;
                     normalInWorldSpace : constant Boolean := True;
                     hitNormalWorld     : math.Vector_3;
                  begin
                     shapeInfo.m_shapePart     := partId;
                     shapeInfo.m_triangleIndex := triangleIndex;

                     hitNormalWorld := getBasis (Self.m_colObjWorldTransform) * hitNormalLocal;

                     declare
                        rayResult : aliased impact.d3.Space.LocalRayResult := to_LocalRayResult (Self.m_collisionObject,
                                                                                                  shapeInfo'Access,
                                                                                                  hitNormalWorld,
                                                                                                  hitFraction);
                     begin
                        return Self.m_resultCallback.addSingleResult (rayResult'Access, normalInWorldSpace);
                     end;
                  end reportHit;


                  rcb : aliased BridgeTriangleRaycastCallback := to_BridgeTriangleRaycastCallback (rayFromLocal, rayToLocal,
                                                                                                   resultCallback,
                                                                                                   collisionObject,
                                                                                                   triangleMesh,
                                                                                                   colObjWorldTransform.all);
               begin
                  rcb.m_hitFraction := resultCallback.m_closestHitFraction;
                  triangleMesh.performRaycast (rcb'Access,  rayFromLocal, rayToLocal);
               end;


            else
               declare
                  use linear_Algebra_3d, impact.d3.Transform;

                  --  generic (slower) case
                  concaveShape           : constant impact.d3.Shape.concave.view := impact.d3.Shape.concave.view (collisionShape);
                  worldTocollisionObject : constant Transform_3d    := inverse (colObjWorldTransform.all);

                  rayFromLocal           : constant math.Vector_3 := worldTocollisionObject * getOrigin (rayFromTrans.all);
                  rayToLocal             : constant math.Vector_3 := worldTocollisionObject * getOrigin (rayToTrans.all);


                  --  ConvexCast::CastResult
                  --
                  type BridgeTriangleRaycastCallback is new impact.d3.triangle_Callback.raycast.btTriangleRaycastCallback with
                     record
                        m_resultCallback       : access impact.d3.Space.RayResultCallback 'Class;
                        m_collisionObject      : access impact.d3.Object.item'Class;
                        m_triangleMesh         : access impact.d3.Shape.concave.Item                'Class;

                        m_colObjWorldTransform : Transform_3d;
                     end record;


                  overriding function reportHit (Self : access BridgeTriangleRaycastCallback;   hitNormalLocal : in math.Vector_3;
                                                                                 hitFraction    : in math.Real;
                                                                                 partId         : in Integer;
                                                                                 triangleIndex  : in Integer) return math.Real;


                  function to_BridgeTriangleRaycastCallback (from, to             : in     math.Vector_3;
                                                             resultCallback       : access impact.d3.Space.RayResultCallback 'Class;
                                                             collisionObject      : access impact.d3.Object.item'Class;
                                                             triangleMesh         : access impact.d3.Shape.concave.Item                'Class;
                                                             colObjWorldTransform : in     Transform_3d) return BridgeTriangleRaycastCallback
                  is
                     Self : BridgeTriangleRaycastCallback := (impact.d3.triangle_Callback.raycast.btTriangleRaycastCallback with
                                                              m_resultCallback       => resultCallback,
                                                              m_collisionObject      => collisionObject,
                                                              m_triangleMesh         => triangleMesh,
                                                              m_colObjWorldTransform => colObjWorldTransform);
                  begin
                     define (Self,  from, to,  resultCallback.m_flags);
                     return Self;
                  end to_BridgeTriangleRaycastCallback;


                  overriding function reportHit (Self : access BridgeTriangleRaycastCallback;   hitNormalLocal : in math.Vector_3;
                                                                                 hitFraction    : in math.Real;
                                                                                 partId         : in Integer;
                                                                                 triangleIndex  : in Integer) return math.Real
                  is
                     shapeInfo          : aliased impact.d3.Space.LocalShapeInfo;
                     normalInWorldSpace : constant Boolean := True;
                     hitNormalWorld     :         math.Vector_3;
                  begin
                     shapeInfo.m_shapePart     := partId;
                     shapeInfo.m_triangleIndex := triangleIndex;

                     hitNormalWorld := getBasis (Self.m_colObjWorldTransform) * hitNormalLocal;

                     declare
                        rayResult : aliased impact.d3.Space.LocalRayResult := to_LocalRayResult (Self.m_collisionObject,
                                                                                                  shapeInfo'Access,
                                                                                                  hitNormalWorld,
                                                                                                  hitFraction);
                     begin
                        return Self.m_resultCallback.addSingleResult (rayResult'Access, normalInWorldSpace);
                     end;
                  end reportHit;


                  rcb : aliased BridgeTriangleRaycastCallback := to_BridgeTriangleRaycastCallback (rayFromLocal, rayToLocal,
                                                                                                   resultCallback,
                                                                                                   collisionObject,
                                                                                                   concaveShape,
                                                                                                   colObjWorldTransform.all);

                  rayAabbMinLocal : math.Vector_3 := rayFromLocal;
                  rayAabbMaxLocal : math.Vector_3 := rayFromLocal;

                  use impact.d3.Vector;
               begin
                  rcb.m_hitFraction := resultCallback.m_closestHitFraction;

                  setMin (rayAabbMinLocal,  rayToLocal);
                  setMax (rayAabbMaxLocal,  rayToLocal);

                  concaveShape.processAllTriangles (rcb'Access,  rayAabbMinLocal, rayAabbMaxLocal);
               end;
            end if;


         else
            if collisionShape.isCompound then
               declare

                  type LocalInfoAdder2 is new RayResultCallback with
                     record
                        m_userCallback : access RayResultCallback'Class;
                        m_i            :        Integer;
                     end record;


                  overriding function needsCollision  (Self : in     LocalInfoAdder2;   p : access impact.d3.collision.Proxy.item'Class) return Boolean;
                  overriding function addSingleResult (Self : access LocalInfoAdder2;   r : access impact.d3.Space.LocalRayResult;
                                                                             b : in     Boolean) return math.Real;


                  function to_LocalInfoAdder2 (i    : in     Integer;
                                               user : access RayResultCallback) return LocalInfoAdder2
                  is
                     Self : LocalInfoAdder2;
                  begin
                     Self.m_userCallback       := user;
                     Self.m_i                  := i;
                     Self.m_closestHitFraction := Self.m_userCallback.m_closestHitFraction;

                     return Self;
                  end to_LocalInfoAdder2;


                  overriding function needsCollision (Self : in LocalInfoAdder2;   p : access impact.d3.collision.Proxy.item'Class) return Boolean
                  is
                  begin
                     return Self.m_userCallback.needsCollision (p);
                  end needsCollision;


                  overriding function addSingleResult (Self : access LocalInfoAdder2;   r : access impact.d3.Space.LocalRayResult;
                                                                             b : in     Boolean) return math.Real
                  is
                     shapeInfo : aliased impact.d3.Space.LocalShapeInfo;
                     result    : math.Real;
                  begin
                     shapeInfo.m_shapePart     := -1;
                     shapeInfo.m_triangleIndex := Self.m_i;

                     if r.m_localShapeInfo = null then
                        r.m_localShapeInfo := shapeInfo'Unchecked_Access;
                     end if;

                     result                    := Self.m_userCallback.addSingleResult (r, b);
                     Self.m_closestHitFraction := Self.m_userCallback.m_closestHitFraction;

                     return result;
                  end addSingleResult;


                  type RayTester is new impact.d3.collision.bounding_volume_Tree.ICollide with
                     record
                        m_collisionObject     : access impact.d3.Object.item'Class;
                        m_compoundShape       : access impact.d3.Shape.compound.item'Class;

                        m_colObjWorldTransform,
                        m_rayFromTrans,
                        m_rayToTrans          : access Transform_3d;

                        m_resultCallback      : access RayResultCallback;
                     end record;


                  procedure Process (Self : in out RayTester'Class;   i : in Integer);


                  function to_RayTester (collisionObject     : access impact.d3.Object.item'Class;
                                         compoundShape       : access impact.d3.Shape.compound.Item'Class;

                                         colObjWorldTransform,
                                         rayFromTrans,
                                         rayToTrans          : access Transform_3d;

                                         resultCallback      : access RayResultCallback'Class) return RayTester
                  is
                     Self : RayTester;
                  begin
                     Self.m_collisionObject      := collisionObject;
                     Self.m_compoundShape        := compoundShape;
                     Self.m_colObjWorldTransform := colObjWorldTransform;
                     Self.m_rayFromTrans         := rayFromTrans;
                     Self.m_rayToTrans           := rayToTrans;
                     Self.m_resultCallback       := resultCallback;

                     return Self;
                  end to_RayTester;


                  procedure Process (Self : in out RayTester'Class;   i : in Integer)
                  is
                     use linear_Algebra_3d, impact.d3.Transform;

                     childCollisionShape : constant impact.d3.Shape.view := Self.m_compoundShape.getChildShape     (i).all'Access;
                     childTrans          : constant Transform_3d      := Self.m_compoundShape.getChildTransform (i).all;
                     childWorldTrans     : aliased  Transform_3d      := Self.m_colObjWorldTransform.all * childTrans;

                     --  replace collision shape so that callback can determine the triangle
                     saveCollisionShape : constant access impact.d3.Shape.Item'Class := Self.m_collisionObject.getCollisionShape;

                  begin
                     Self.m_collisionObject.internalSetTemporaryCollisionShape (impact.d3.Shape.view (childCollisionShape));

                     declare
                        my_cb : aliased LocalInfoAdder2 := to_LocalInfoAdder2 (i, Self.m_resultCallback);
                     begin
                        rayTestSingle (Self.m_rayFromTrans,
                                       Self.m_rayToTrans,
                                       Self.m_collisionObject,
                                       childCollisionShape,
                                       childWorldTrans'Access,
                                       my_cb'Access);
                     end;

                     Self.m_collisionObject.internalSetTemporaryCollisionShape (saveCollisionShape);   -- restore
                  end Process;


                  procedure Process (Self : in out RayTester;   leaf : in impact.d3.collision.bounding_volume_Tree.Node'Class)
                  is
                  begin
                     Self.Process (i => leaf.state.dataAsInt);   -- tbd: check this
                  end Process;



                  compoundShape : constant impact.d3.Shape.compound.view := impact.d3.Shape.compound.view (collisionShape);
                  dbvt          : constant access  impact.d3.collision.bounding_volume_Tree.item          := compoundShape.getDynamicAabbTree;


                  rayCB         : aliased RayTester            := to_RayTester (collisionObject,
                                                                                compoundShape,
                                                                                colObjWorldTransform,
                                                                                rayFromTrans,
                                                                                rayToTrans,
                                                                                resultCallback);
               begin

                  if         dbvt /= null
                    and then not DISABLE_DBVT_COMPOUNDSHAPE_RAYCAST_ACCELERATION
                  then
                     declare
                        use impact.d3.Transform;
                        localRayFrom : constant math.Vector_3 := getOrigin (inverseTimes (colObjWorldTransform.all, rayFromTrans.all));
                        localRayTo   : constant math.Vector_3 := getOrigin (inverseTimes (colObjWorldTransform.all, rayToTrans.all));
                     begin
                        impact.d3.collision.bounding_volume_Tree.rayTest (dbvt.Root,  localRayFrom, localRayTo,  rayCB'Access);
                     end;

                  else
                     for i in 1 .. compoundShape.getNumChildShapes --   (int i = 0, n = compoundShape->getNumChildShapes(); i < n; ++i)
                     loop
                        rayCB.Process (i);
                     end loop;
                  end if;

               end;
            end if;
         end if;

      end if;

   end rayTestSingle;




--  void        impact.d3.Space::rayTestSingle(const impact.d3.Transform& rayFromTrans,const impact.d3.Transform& rayToTrans,
--                                                                                  impact.d3.Object* collisionObject,
--                                                                                  const impact.d3.Shape* collisionShape,
--                                                                                  const impact.d3.Transform& colObjWorldTransform,
--                                                                                  RayResultCallback& resultCallback)
--  {
--          impact.d3.Shape.convex.internal.sphere pointShape(impact.d3.Scalar(0.0));
--          pointShape.setMargin(0.f);
--          const impact.d3.Shape.convex* castShape = &pointShape;
--
--          if (collisionShape->isConvex())
--          {
--                  //                BT_PROFILE("rayTestConvex");
--                  impact.d3.collision.convex_Raycast::CastResult castResult;
--                  castResult.m_fraction = resultCallback.m_closestHitFraction;
--
--                  impact.d3.Shape.convex* convexShape = (impact.d3.Shape.convex*) collisionShape;
--                  impact.d3.collision.simplex_Solver.voronoi        simplexSolver;
--  #define USE_SUBSIMPLEX_CONVEX_CAST 1
--  #ifdef USE_SUBSIMPLEX_CONVEX_CAST
--                  impact.d3.collision.convex_Raycast.subsimplex convexCaster(castShape,convexShape,&simplexSolver);
--  #else
--                  //impact.d3.collision.convex_Raycast.gjk        convexCaster(castShape,convexShape,&simplexSolver);
--                  //impact.d3.collision.convex_Raycast.continuous_convex convexCaster(castShape,convexShape,&simplexSolver,0);
--  #endif //#USE_SUBSIMPLEX_CONVEX_CAST
--
--                  if (convexCaster.calcTimeOfImpact(rayFromTrans,rayToTrans,colObjWorldTransform,colObjWorldTransform,castResult))
--                  {
--                          //add hit
--                          if (castResult.m_normal.length2() > impact.d3.Scalar(0.0001))
--                          {
--                                  if (castResult.m_fraction < resultCallback.m_closestHitFraction)
--                                  {
--  #ifdef USE_SUBSIMPLEX_CONVEX_CAST
--                                          //rotate normal into worldspace
--                                          castResult.m_normal = rayFromTrans.getBasis() * castResult.m_normal;
--  #endif //USE_SUBSIMPLEX_CONVEX_CAST
--
--                                          castResult.m_normal.normalize();
--                                          impact.d3.Space::LocalRayResult localRayResult
--                                                  (
--                                                  collisionObject,
--                                                  0,
--                                                  castResult.m_normal,
--                                                  castResult.m_fraction
--                                                  );
--
--                                          bool normalInWorldSpace = true;
--                                          resultCallback.addSingleResult(localRayResult, normalInWorldSpace);
--
--                                  }
--                          }
--                  }
--          } else {
--                  if (collisionShape->isConcave())
--                  {
--                          //                        BT_PROFILE("rayTestConcave");
--                          if (collisionShape->getShapeType()==TRIANGLE_MESH_SHAPE_PROXYTYPE)
--                          {
--                                  ///optimized version for impact.d3.Shape.concave.triangle_mesh.bvh
--                                  impact.d3.Shape.concave.triangle_mesh.bvh* triangleMesh = (impact.d3.Shape.concave.triangle_mesh.bvh*)collisionShape;
--                                  impact.d3.Transform worldTocollisionObject = colObjWorldTransform.inverse();
--                                  impact.d3.Vector rayFromLocal = worldTocollisionObject * rayFromTrans.getOrigin();
--                                  impact.d3.Vector rayToLocal = worldTocollisionObject * rayToTrans.getOrigin();
--
--                                  //ConvexCast::CastResult
--                                  struct BridgeTriangleRaycastCallback : public btTriangleRaycastCallback
--                                  {
--                                          impact.d3.Space::RayResultCallback* m_resultCallback;
--                                          impact.d3.Object*        m_collisionObject;
--                                          impact.d3.Shape.concave.triangle_mesh*        m_triangleMesh;
--
--                                          impact.d3.Transform m_colObjWorldTransform;
--
--                                          BridgeTriangleRaycastCallback( const impact.d3.Vector& from,const impact.d3.Vector& to,
--                                                  impact.d3.Space::RayResultCallback* resultCallback, impact.d3.Object* collisionObject,impact.d3.Shape.concave.triangle_mesh*        triangleMesh,const impact.d3.Transform& colObjWorldTransform):
--                                          //@BP Mod
--                                          btTriangleRaycastCallback(from,to, resultCallback->m_flags),
--                                                  m_resultCallback(resultCallback),
--                                                  m_collisionObject(collisionObject),
--                                                  m_triangleMesh(triangleMesh),
--                                                  m_colObjWorldTransform(colObjWorldTransform)
--                                          {
--                                          }
--
--
--                                          virtual impact.d3.Scalar reportHit(const impact.d3.Vector& hitNormalLocal, impact.d3.Scalar hitFraction, int partId, int triangleIndex )
--                                          {
--                                                  impact.d3.Space::LocalShapeInfo        shapeInfo;
--                                                  shapeInfo.m_shapePart = partId;
--                                                  shapeInfo.m_triangleIndex = triangleIndex;
--
--                                                  impact.d3.Vector hitNormalWorld = m_colObjWorldTransform.getBasis() * hitNormalLocal;
--
--                                                  impact.d3.Space::LocalRayResult rayResult
--                                                          (m_collisionObject,
--                                                          &shapeInfo,
--                                                          hitNormalWorld,
--                                                          hitFraction);
--
--                                                  bool        normalInWorldSpace = true;
--                                                  return m_resultCallback->addSingleResult(rayResult,normalInWorldSpace);
--                                          }
--
--                                  };
--
--                                  BridgeTriangleRaycastCallback rcb(rayFromLocal,rayToLocal,&resultCallback,collisionObject,triangleMesh,colObjWorldTransform);
--                                  rcb.m_hitFraction = resultCallback.m_closestHitFraction;
--                                  triangleMesh->performRaycast(&rcb,rayFromLocal,rayToLocal);
--                          } else
--                          {
--                                  //generic (slower) case
--                                  impact.d3.Shape.concave* concaveShape = (impact.d3.Shape.concave*)collisionShape;
--
--                                  impact.d3.Transform worldTocollisionObject = colObjWorldTransform.inverse();
--
--                                  impact.d3.Vector rayFromLocal = worldTocollisionObject * rayFromTrans.getOrigin();
--                                  impact.d3.Vector rayToLocal = worldTocollisionObject * rayToTrans.getOrigin();
--
--                                  //ConvexCast::CastResult
--
--                                  struct BridgeTriangleRaycastCallback : public btTriangleRaycastCallback
--                                  {
--                                          impact.d3.Space::RayResultCallback* m_resultCallback;
--                                          impact.d3.Object*        m_collisionObject;
--                                          impact.d3.Shape.concave*        m_triangleMesh;
--
--                                          impact.d3.Transform m_colObjWorldTransform;
--
--                                          BridgeTriangleRaycastCallback( const impact.d3.Vector& from,const impact.d3.Vector& to,
--                                                  impact.d3.Space::RayResultCallback* resultCallback, impact.d3.Object* collisionObject,impact.d3.Shape.concave*        triangleMesh, const impact.d3.Transform& colObjWorldTransform):
--                                          //@BP Mod
--                                          btTriangleRaycastCallback(from,to, resultCallback->m_flags),
--                                                  m_resultCallback(resultCallback),
--                                                  m_collisionObject(collisionObject),
--                                                  m_triangleMesh(triangleMesh),
--                                                  m_colObjWorldTransform(colObjWorldTransform)
--                                          {
--                                          }
--
--
--                                          virtual impact.d3.Scalar reportHit(const impact.d3.Vector& hitNormalLocal, impact.d3.Scalar hitFraction, int partId, int triangleIndex )
--                                          {
--                                                  impact.d3.Space::LocalShapeInfo        shapeInfo;
--                                                  shapeInfo.m_shapePart = partId;
--                                                  shapeInfo.m_triangleIndex = triangleIndex;
--
--                                                  impact.d3.Vector hitNormalWorld = m_colObjWorldTransform.getBasis() * hitNormalLocal;
--
--                                                  impact.d3.Space::LocalRayResult rayResult
--                                                          (m_collisionObject,
--                                                          &shapeInfo,
--                                                          hitNormalWorld,
--                                                          hitFraction);
--
--                                                  bool        normalInWorldSpace = true;
--                                                  return m_resultCallback->addSingleResult(rayResult,normalInWorldSpace);
--                                          }
--
--                                  };
--
--
--                                  BridgeTriangleRaycastCallback        rcb(rayFromLocal,rayToLocal,&resultCallback,collisionObject,concaveShape, colObjWorldTransform);
--                                  rcb.m_hitFraction = resultCallback.m_closestHitFraction;
--
--                                  impact.d3.Vector rayAabbMinLocal = rayFromLocal;
--                                  rayAabbMinLocal.setMin(rayToLocal);
--                                  impact.d3.Vector rayAabbMaxLocal = rayFromLocal;
--                                  rayAabbMaxLocal.setMax(rayToLocal);
--
--                                  concaveShape->processAllTriangles(&rcb,rayAabbMinLocal,rayAabbMaxLocal);
--                          }
--                  } else {
--                          //                        BT_PROFILE("rayTestCompound");
--                          if (collisionShape->isCompound())
--                          {
--                                  struct LocalInfoAdder2 : public RayResultCallback
--                                  {
--                                          RayResultCallback* m_userCallback;
--                                          int m_i;
--
--                                          LocalInfoAdder2 (int i, RayResultCallback *user)
--                                                  : m_userCallback(user), m_i(i)
--                                          {
--                                                  m_closestHitFraction = m_userCallback->m_closestHitFraction;
--                                          }
--                                          virtual bool needsCollision(impact.d3.collision.Proxy* p) const
--                                          {
--                                                  return m_userCallback->needsCollision(p);
--                                          }
--
--                                          virtual impact.d3.Scalar addSingleResult (impact.d3.Space::LocalRayResult &r, bool b)
--                                          {
--                                                  impact.d3.Space::LocalShapeInfo shapeInfo;
--                                                  shapeInfo.m_shapePart = -1;
--                                                  shapeInfo.m_triangleIndex = m_i;
--                                                  if (r.m_localShapeInfo == NULL)
--                                                          r.m_localShapeInfo = &shapeInfo;
--
--                                                  const impact.d3.Scalar result = m_userCallback->addSingleResult(r, b);
--                                                  m_closestHitFraction = m_userCallback->m_closestHitFraction;
--                                                  return result;
--                                          }
--                                  };
--
--                                  struct RayTester : impact.d3.collision.bounding_volume_Tree::ICollide
--                                  {
--                                          impact.d3.Object* m_collisionObject;
--                                          const impact.d3.Shape.compound* m_compoundShape;
--                                          const impact.d3.Transform& m_colObjWorldTransform;
--                                          const impact.d3.Transform& m_rayFromTrans;
--                                          const impact.d3.Transform& m_rayToTrans;
--                                          RayResultCallback& m_resultCallback;
--
--                                          RayTester(impact.d3.Object* collisionObject,
--                                                          const impact.d3.Shape.compound* compoundShape,
--                                                          const impact.d3.Transform& colObjWorldTransform,
--                                                          const impact.d3.Transform& rayFromTrans,
--                                                          const impact.d3.Transform& rayToTrans,
--                                                          RayResultCallback& resultCallback):
--                                                  m_collisionObject(collisionObject),
--                                                  m_compoundShape(compoundShape),
--                                                  m_colObjWorldTransform(colObjWorldTransform),
--                                                  m_rayFromTrans(rayFromTrans),
--                                                  m_rayToTrans(rayToTrans),
--                                                  m_resultCallback(resultCallback)
--                                          {
--
--                                          }
--
--                                          void Process(int i)
--                                          {
--                                                  const impact.d3.Shape* childCollisionShape = m_compoundShape->getChildShape(i);
--                                                  const impact.d3.Transform& childTrans = m_compoundShape->getChildTransform(i);
--                                                  impact.d3.Transform childWorldTrans = m_colObjWorldTransform * childTrans;
--
--                                                  // replace collision shape so that callback can determine the triangle
--                                                  impact.d3.Shape* saveCollisionShape = m_collisionObject->getCollisionShape();
--                                                  m_collisionObject->internalSetTemporaryCollisionShape((impact.d3.Shape*)childCollisionShape);
--
--                                                  LocalInfoAdder2 my_cb(i, &m_resultCallback);
--
--                                                  rayTestSingle(
--                                                          m_rayFromTrans,
--                                                          m_rayToTrans,
--                                                          m_collisionObject,
--                                                          childCollisionShape,
--                                                          childWorldTrans,
--                                                          my_cb);
--
--                                                  // restore
--                                                  m_collisionObject->internalSetTemporaryCollisionShape(saveCollisionShape);
--                                          }
--
--                                          void Process(const impact.d3.collision.bounding_volume_TreeNode* leaf)
--                                          {
--                                                  Process(leaf->dataAsInt);
--                                          }
--                                  };
--
--                                  const impact.d3.Shape.compound* compoundShape = static_cast<const impact.d3.Shape.compound*>(collisionShape);
--                                  const impact.d3.collision.bounding_volume_Tree* dbvt = compoundShape->getDynamicAabbTree();
--
--
--                                  RayTester rayCB(
--                                          collisionObject,
--                                          compoundShape,
--                                          colObjWorldTransform,
--                                          rayFromTrans,
--                                          rayToTrans,
--                                          resultCallback);
--  #ifndef        DISABLE_DBVT_COMPOUNDSHAPE_RAYCAST_ACCELERATION
--                                  if (dbvt)
--                                  {
--                                          impact.d3.Vector localRayFrom = colObjWorldTransform.inverseTimes(rayFromTrans).getOrigin();
--                                          impact.d3.Vector localRayTo = colObjWorldTransform.inverseTimes(rayToTrans).getOrigin();
--                                          impact.d3.collision.bounding_volume_Tree::rayTest(dbvt->m_root, localRayFrom , localRayTo, rayCB);
--                                  }
--                                  else
--  #endif //DISABLE_DBVT_COMPOUNDSHAPE_RAYCAST_ACCELERATION
--                                  {
--                                          for (int i = 0, n = compoundShape->getNumChildShapes(); i < n; ++i)
--                                          {
--                                                  rayCB.Process(i);
--                                          }
--                                  }
--                          }
--                  }
--          }
--  }










   --  objectQuerySingle performs a collision detection query and calls the resultCallback. It is used internally by rayTest.
   --
   procedure objectQuerySingle (castShape                : in     impact.d3.Shape.convex.item'Class;
                                rayFromTrans, rayToTrans : in     math.Transform_3d;
                                collisionObject          : access impact.d3.Object.item'Class;
                                collisionShape           : in     impact.d3.Shape.item'Class;
                                colObjWorldTransform     : in     math.Transform_3d;
                                resultCallback           : access ConvexResultCallback'Class;
                                allowedPenetration       : in     math.Real)
   is
   begin
      raise Program_Error with "tbd";
      return;
   end objectQuerySingle;






   procedure addCollisionObject (Self : in out  Item;   collisionObject      : access impact.d3.Object.item'Class;
                                                        collisionFilterGroup : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.DefaultFilter;
                                                        collisionFilterMask  : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.AllFilter)
   is

      pragma Assert (collisionObject /= null);

      trans : Transform_3d;

      minAabb,
      maxAabb : math.Vector_3;

      the_type : collision.Proxy.BroadphaseNativeTypes;

   begin
      Self.m_collisionObjects.append (collisionObject.all'Access);
--        pragma Assert (Self.m_collisionObjects.find_Index (collisionObject.all'Access) = impact.d3.Object.Vectors.extended_Index (Self.m_collisionObjects.Length));   -- check that the object isn't already added

      --  calculate new AABB
      trans := collisionObject.getWorldTransform.all;

      collisionObject.getCollisionShape.getAabb (trans,  minAabb, maxAabb);

      the_type := collisionObject.getCollisionShape.getShapeType;
      collisionObject.setBroadphaseHandle (Self.getBroadphase.createProxy (minAabb,              maxAabb,
                                                                           the_type,
                                                                           collisionObject,
                                                                           collisionFilterGroup, collisionFilterMask,
                                                                           Self.m_dispatcher1,
                                                                           System.null_Address));
   end addCollisionObject;








   function getCollisionObjectArray (Self : access Item) return access impact.d3.Object.Vector
   is
   begin
      return Self.m_collisionObjects'Unchecked_Access;
   end getCollisionObjectArray;





   procedure removeCollisionObject (Self : in out  Item;   collisionObject      : access impact.d3.Object.item'Class)
   is
      bp : constant access impact.d3.collision.Proxy.item'Class := collisionObject.getBroadphaseHandle;

   begin
      if bp /= null then -- only clear the cached algorithms
         Self.getBroadphase.all.getOverlappingPairCache.cleanProxyFromPairs (bp, Self.m_dispatcher1);
         Self.getBroadphase.destroyProxy                                (bp, Self.m_dispatcher1);

         collisionObject.setBroadphaseHandle (null);
      end if;


      Self.m_collisionObjects.delete (Self.m_collisionObjects.find_Index (collisionObject.all'Access));   -- swapremove
   end removeCollisionObject;








   procedure performDiscreteCollisionDetection (Self : in out  Item)
   is
      dispatchInfo : constant access impact.d3.Dispatcher.DispatcherInfo  := Self.getDispatchInfo;
      dispatcher   : access impact.d3.Dispatcher.item'Class;

   begin
      Self.updateAabbs;

      Self.m_broadphasePairCache.calculateOverlappingPairs (Self.m_dispatcher1);


      dispatcher := Self.getDispatcher;

      if dispatcher /= null then
         dispatcher.dispatchAllCollisionPairs (Self.m_broadphasePairCache.getOverlappingPairCache,
                                               dispatchInfo,
                                               Self.m_dispatcher1);
      end if;

   end performDiscreteCollisionDetection;









   function getDispatchInfo (Self : access Item) return access impact.d3.Dispatcher.DispatcherInfo
   is
   begin
      return Self.m_dispatchInfo'Unchecked_Access;
   end getDispatchInfo;






   function getForceUpdateAllAabbs (Self : in Item) return Boolean
   is
   begin
      return Self.m_forceUpdateAllAabbs;
   end getForceUpdateAllAabbs;






   procedure setForceUpdateAllAabbs (Self : in out  Item;   forceUpdateAllAabbs : in Boolean)
   is
   begin
      Self.m_forceUpdateAllAabbs := forceUpdateAllAabbs;
   end setForceUpdateAllAabbs;





   --- 'Protected' functions
   --

   function get_m_collisionObjects (Self : access Item) return access impact.d3.Object.Vector
   is
   begin
      return Self.m_collisionObjects'Unchecked_Access;
   end get_m_collisionObjects;





end impact.d3.Space;






--  void        impact.d3.Space::objectQuerySingle(const impact.d3.Shape.convex* castShape,const impact.d3.Transform& convexFromTrans,const impact.d3.Transform& convexToTrans,
--                                                                                          impact.d3.Object* collisionObject,
--                                                                                          const impact.d3.Shape* collisionShape,
--                                                                                          const impact.d3.Transform& colObjWorldTransform,
--                                                                                          ConvexResultCallback& resultCallback, impact.d3.Scalar allowedPenetration)
--  {
--          if (collisionShape->isConvex())
--          {
--                  //BT_PROFILE("convexSweepConvex");
--                  impact.d3.collision.convex_Raycast::CastResult castResult;
--                  castResult.m_allowedPenetration = allowedPenetration;
--                  castResult.m_fraction = resultCallback.m_closestHitFraction;//impact.d3.Scalar(1.);//??
--
--                  impact.d3.Shape.convex* convexShape = (impact.d3.Shape.convex*) collisionShape;
--                  impact.d3.collision.simplex_Solver.voronoi        simplexSolver;
--                  impact.d3.collision.convex_penetration_depth_Solver.gjk_epa        gjkEpaPenetrationSolver;
--
--                  impact.d3.collision.convex_Raycast.continuous_convex convexCaster1(castShape,convexShape,&simplexSolver,&gjkEpaPenetrationSolver);
--                  //impact.d3.collision.convex_Raycast.gjk convexCaster2(castShape,convexShape,&simplexSolver);
--                  //impact.d3.collision.convex_Raycast.subsimplex convexCaster3(castShape,convexShape,&simplexSolver);
--
--                  impact.d3.collision.convex_Raycast* castPtr = &convexCaster1;
--
--
--
--                  if (castPtr->calcTimeOfImpact(convexFromTrans,convexToTrans,colObjWorldTransform,colObjWorldTransform,castResult))
--                  {
--                          //add hit
--                          if (castResult.m_normal.length2() > impact.d3.Scalar(0.0001))
--                          {
--                                  if (castResult.m_fraction < resultCallback.m_closestHitFraction)
--                                  {
--                                          castResult.m_normal.normalize();
--                                          impact.d3.Space::LocalConvexResult localConvexResult
--                                                  (
--                                                  collisionObject,
--                                                  0,
--                                                  castResult.m_normal,
--                                                  castResult.m_hitPoint,
--                                                  castResult.m_fraction
--                                                  );
--
--                                          bool normalInWorldSpace = true;
--                                          resultCallback.addSingleResult(localConvexResult, normalInWorldSpace);
--
--                                  }
--                          }
--                  }
--          } else {
--                  if (collisionShape->isConcave())
--                  {
--                          if (collisionShape->getShapeType()==TRIANGLE_MESH_SHAPE_PROXYTYPE)
--                          {
--                                  //BT_PROFILE("convexSweepbtBvhTriangleMesh");
--                                  impact.d3.Shape.concave.triangle_mesh.bvh* triangleMesh = (impact.d3.Shape.concave.triangle_mesh.bvh*)collisionShape;
--                                  impact.d3.Transform worldTocollisionObject = colObjWorldTransform.inverse();
--                                  impact.d3.Vector convexFromLocal = worldTocollisionObject * convexFromTrans.getOrigin();
--                                  impact.d3.Vector convexToLocal = worldTocollisionObject * convexToTrans.getOrigin();
--                                  // rotation of box in local mesh space = MeshRotation^-1 * ConvexToRotation
--                                  impact.d3.Transform rotationXform = impact.d3.Transform(worldTocollisionObject.getBasis() * convexToTrans.getBasis());
--
--                                  //ConvexCast::CastResult
--                                  struct BridgeTriangleConvexcastCallback : public btTriangleConvexcastCallback
--                                  {
--                                          impact.d3.Space::ConvexResultCallback* m_resultCallback;
--                                          impact.d3.Object*        m_collisionObject;
--                                          impact.d3.Shape.concave.triangle_mesh*        m_triangleMesh;
--
--                                          BridgeTriangleConvexcastCallback(const impact.d3.Shape.convex* castShape, const impact.d3.Transform& from,const impact.d3.Transform& to,
--                                                  impact.d3.Space::ConvexResultCallback* resultCallback, impact.d3.Object* collisionObject,impact.d3.Shape.concave.triangle_mesh*        triangleMesh, const impact.d3.Transform& triangleToWorld):
--                                          btTriangleConvexcastCallback(castShape, from,to, triangleToWorld, triangleMesh->getMargin()),
--                                                  m_resultCallback(resultCallback),
--                                                  m_collisionObject(collisionObject),
--                                                  m_triangleMesh(triangleMesh)
--                                          {
--                                          }
--
--
--                                          virtual impact.d3.Scalar reportHit(const impact.d3.Vector& hitNormalLocal, const impact.d3.Vector& hitPointLocal, impact.d3.Scalar hitFraction, int partId, int triangleIndex )
--                                          {
--                                                  impact.d3.Space::LocalShapeInfo        shapeInfo;
--                                                  shapeInfo.m_shapePart = partId;
--                                                  shapeInfo.m_triangleIndex = triangleIndex;
--                                                  if (hitFraction <= m_resultCallback->m_closestHitFraction)
--                                                  {
--
--                                                          impact.d3.Space::LocalConvexResult convexResult
--                                                                  (m_collisionObject,
--                                                                  &shapeInfo,
--                                                                  hitNormalLocal,
--                                                                  hitPointLocal,
--                                                                  hitFraction);
--
--                                                          bool        normalInWorldSpace = true;
--
--
--                                                          return m_resultCallback->addSingleResult(convexResult,normalInWorldSpace);
--                                                  }
--                                                  return hitFraction;
--                                          }
--
--                                  };
--
--                                  BridgeTriangleConvexcastCallback tccb(castShape, convexFromTrans,convexToTrans,&resultCallback,collisionObject,triangleMesh, colObjWorldTransform);
--                                  tccb.m_hitFraction = resultCallback.m_closestHitFraction;
--                                  tccb.m_allowedPenetration = allowedPenetration;
--                                  impact.d3.Vector boxMinLocal, boxMaxLocal;
--                                  castShape->getAabb(rotationXform, boxMinLocal, boxMaxLocal);
--                                  triangleMesh->performConvexcast(&tccb,convexFromLocal,convexToLocal,boxMinLocal, boxMaxLocal);
--                          } else
--                          {
--                                  if (collisionShape->getShapeType()==STATIC_PLANE_PROXYTYPE)
--                                  {
--                                          impact.d3.collision.convex_Raycast::CastResult castResult;
--                                          castResult.m_allowedPenetration = allowedPenetration;
--                                          castResult.m_fraction = resultCallback.m_closestHitFraction;
--                                          impact.d3.Shape.concave.static_plane* planeShape = (impact.d3.Shape.concave.static_plane*) collisionShape;
--                                          impact.d3.collision.convex_Raycast.continuous_convex convexCaster1(castShape,planeShape);
--                                          impact.d3.collision.convex_Raycast* castPtr = &convexCaster1;
--
--                                          if (castPtr->calcTimeOfImpact(convexFromTrans,convexToTrans,colObjWorldTransform,colObjWorldTransform,castResult))
--                                          {
--                                                  //add hit
--                                                  if (castResult.m_normal.length2() > impact.d3.Scalar(0.0001))
--                                                  {
--                                                          if (castResult.m_fraction < resultCallback.m_closestHitFraction)
--                                                          {
--                                                                  castResult.m_normal.normalize();
--                                                                  impact.d3.Space::LocalConvexResult localConvexResult
--                                                                          (
--                                                                          collisionObject,
--                                                                          0,
--                                                                          castResult.m_normal,
--                                                                          castResult.m_hitPoint,
--                                                                          castResult.m_fraction
--                                                                          );
--
--                                                                  bool normalInWorldSpace = true;
--                                                                  resultCallback.addSingleResult(localConvexResult, normalInWorldSpace);
--                                                          }
--                                                  }
--                                          }
--
--                                  } else
--                                  {
--                                          //BT_PROFILE("convexSweepConcave");
--                                          impact.d3.Shape.concave* concaveShape = (impact.d3.Shape.concave*)collisionShape;
--                                          impact.d3.Transform worldTocollisionObject = colObjWorldTransform.inverse();
--                                          impact.d3.Vector convexFromLocal = worldTocollisionObject * convexFromTrans.getOrigin();
--                                          impact.d3.Vector convexToLocal = worldTocollisionObject * convexToTrans.getOrigin();
--                                          // rotation of box in local mesh space = MeshRotation^-1 * ConvexToRotation
--                                          impact.d3.Transform rotationXform = impact.d3.Transform(worldTocollisionObject.getBasis() * convexToTrans.getBasis());
--
--                                          //ConvexCast::CastResult
--                                          struct BridgeTriangleConvexcastCallback : public btTriangleConvexcastCallback
--                                          {
--                                                  impact.d3.Space::ConvexResultCallback* m_resultCallback;
--                                                  impact.d3.Object*        m_collisionObject;
--                                                  impact.d3.Shape.concave*        m_triangleMesh;
--
--                                                  BridgeTriangleConvexcastCallback(const impact.d3.Shape.convex* castShape, const impact.d3.Transform& from,const impact.d3.Transform& to,
--                                                          impact.d3.Space::ConvexResultCallback* resultCallback, impact.d3.Object* collisionObject,impact.d3.Shape.concave*        triangleMesh, const impact.d3.Transform& triangleToWorld):
--                                                  btTriangleConvexcastCallback(castShape, from,to, triangleToWorld, triangleMesh->getMargin()),
--                                                          m_resultCallback(resultCallback),
--                                                          m_collisionObject(collisionObject),
--                                                          m_triangleMesh(triangleMesh)
--                                                  {
--                                                  }
--
--
--                                                  virtual impact.d3.Scalar reportHit(const impact.d3.Vector& hitNormalLocal, const impact.d3.Vector& hitPointLocal, impact.d3.Scalar hitFraction, int partId, int triangleIndex )
--                                                  {
--                                                          impact.d3.Space::LocalShapeInfo        shapeInfo;
--                                                          shapeInfo.m_shapePart = partId;
--                                                          shapeInfo.m_triangleIndex = triangleIndex;
--                                                          if (hitFraction <= m_resultCallback->m_closestHitFraction)
--                                                          {
--
--                                                                  impact.d3.Space::LocalConvexResult convexResult
--                                                                          (m_collisionObject,
--                                                                          &shapeInfo,
--                                                                          hitNormalLocal,
--                                                                          hitPointLocal,
--                                                                          hitFraction);
--
--                                                                  bool        normalInWorldSpace = false;
--
--                                                                  return m_resultCallback->addSingleResult(convexResult,normalInWorldSpace);
--                                                          }
--                                                          return hitFraction;
--                                                  }
--
--                                          };
--
--                                          BridgeTriangleConvexcastCallback tccb(castShape, convexFromTrans,convexToTrans,&resultCallback,collisionObject,concaveShape, colObjWorldTransform);
--                                          tccb.m_hitFraction = resultCallback.m_closestHitFraction;
--                                          tccb.m_allowedPenetration = allowedPenetration;
--                                          impact.d3.Vector boxMinLocal, boxMaxLocal;
--                                          castShape->getAabb(rotationXform, boxMinLocal, boxMaxLocal);
--
--                                          impact.d3.Vector rayAabbMinLocal = convexFromLocal;
--                                          rayAabbMinLocal.setMin(convexToLocal);
--                                          impact.d3.Vector rayAabbMaxLocal = convexFromLocal;
--                                          rayAabbMaxLocal.setMax(convexToLocal);
--                                          rayAabbMinLocal += boxMinLocal;
--                                          rayAabbMaxLocal += boxMaxLocal;
--                                          concaveShape->processAllTriangles(&tccb,rayAabbMinLocal,rayAabbMaxLocal);
--                                  }
--                          }
--                  } else {
--                          ///@todo : use AABB tree or other BVH acceleration structure!
--                          if (collisionShape->isCompound())
--                          {
--                                  BT_PROFILE("convexSweepCompound");
--                                  const impact.d3.Shape.compound* compoundShape = static_cast<const impact.d3.Shape.compound*>(collisionShape);
--                                  int i=0;
--                                  for (i=0;i<compoundShape->getNumChildShapes();i++)
--                                  {
--                                          impact.d3.Transform childTrans = compoundShape->getChildTransform(i);
--                                          const impact.d3.Shape* childCollisionShape = compoundShape->getChildShape(i);
--                                          impact.d3.Transform childWorldTrans = colObjWorldTransform * childTrans;
--                                          // replace collision shape so that callback can determine the triangle
--                                          impact.d3.Shape* saveCollisionShape = collisionObject->getCollisionShape();
--                                          collisionObject->internalSetTemporaryCollisionShape((impact.d3.Shape*)childCollisionShape);
--                      struct        LocalInfoAdder : public ConvexResultCallback {
--                              ConvexResultCallback* m_userCallback;
--                                                          int m_i;
--
--                              LocalInfoAdder (int i, ConvexResultCallback *user)
--                                                                  : m_userCallback(user), m_i(i)
--                                                          {
--                                                                  m_closestHitFraction = m_userCallback->m_closestHitFraction;
--                                                          }
--                                                          virtual bool needsCollision(impact.d3.collision.Proxy* p) const
--                                                          {
--                                                                  return m_userCallback->needsCollision(p);
--                                                          }
--                              virtual impact.d3.Scalar addSingleResult (impact.d3.Space::LocalConvexResult&        r,        bool b)
--                              {
--                                      impact.d3.Space::LocalShapeInfo        shapeInfo;
--                                      shapeInfo.m_shapePart = -1;
--                                      shapeInfo.m_triangleIndex = m_i;
--                                      if (r.m_localShapeInfo == NULL)
--                                          r.m_localShapeInfo = &shapeInfo;
--                                                                          const impact.d3.Scalar result = m_userCallback->addSingleResult(r, b);
--                                                                          m_closestHitFraction = m_userCallback->m_closestHitFraction;
--                                                                          return result;
--
--                              }
--                      };
--
--                      LocalInfoAdder my_cb(i, &resultCallback);
--
--
--                                          objectQuerySingle(castShape, convexFromTrans,convexToTrans,
--                                                  collisionObject,
--                                                  childCollisionShape,
--                                                  childWorldTrans,
--                                                  my_cb, allowedPenetration);
--                                          // restore
--                                          collisionObject->internalSetTemporaryCollisionShape(saveCollisionShape);
--                                  }
--                          }
--                  }
--          }
--  }










--  struct btSingleSweepCallback : public btBroadphaseRayCallback
--  {
--
--          impact.d3.Transform        m_convexFromTrans;
--          impact.d3.Transform        m_convexToTrans;
--          impact.d3.Vector        m_hitNormal;
--          const impact.d3.Space*        m_world;
--          impact.d3.Space::ConvexResultCallback&        m_resultCallback;
--          impact.d3.Scalar        m_allowedCcdPenetration;
--          const impact.d3.Shape.convex* m_castShape;
--
--
--          btSingleSweepCallback(const impact.d3.Shape.convex* castShape, const impact.d3.Transform& convexFromTrans,const impact.d3.Transform& convexToTrans,const impact.d3.Space* world,impact.d3.Space::ConvexResultCallback& resultCallback,impact.d3.Scalar allowedPenetration)
--                  :m_convexFromTrans(convexFromTrans),
--                  m_convexToTrans(convexToTrans),
--                  m_world(world),
--                  m_resultCallback(resultCallback),
--                  m_allowedCcdPenetration(allowedPenetration),
--                  m_castShape(castShape)
--          {
--                  impact.d3.Vector unnormalizedRayDir = (m_convexToTrans.getOrigin()-m_convexFromTrans.getOrigin());
--                  impact.d3.Vector rayDir = unnormalizedRayDir.normalized();
--                  ///what about division by zero? --> just set rayDirection[i] to INF/BT_LARGE_FLOAT
--                  m_rayDirectionInverse[0] = rayDir[0] == impact.d3.Scalar(0.0) ? impact.d3.Scalar(BT_LARGE_FLOAT) : impact.d3.Scalar(1.0) / rayDir[0];
--                  m_rayDirectionInverse[1] = rayDir[1] == impact.d3.Scalar(0.0) ? impact.d3.Scalar(BT_LARGE_FLOAT) : impact.d3.Scalar(1.0) / rayDir[1];
--                  m_rayDirectionInverse[2] = rayDir[2] == impact.d3.Scalar(0.0) ? impact.d3.Scalar(BT_LARGE_FLOAT) : impact.d3.Scalar(1.0) / rayDir[2];
--                  m_signs[0] = m_rayDirectionInverse[0] < 0.0;
--                  m_signs[1] = m_rayDirectionInverse[1] < 0.0;
--                  m_signs[2] = m_rayDirectionInverse[2] < 0.0;
--
--                  m_lambda_max = rayDir.dot(unnormalizedRayDir);
--
--          }
--
--          virtual bool        process(const impact.d3.collision.Proxy* proxy)
--          {
--                  ///terminate further convex sweep tests, once the closestHitFraction reached zero
--                  if (m_resultCallback.m_closestHitFraction == impact.d3.Scalar(0.f))
--                          return false;
--
--                  impact.d3.Object*        collisionObject = (impact.d3.Object*)proxy->m_clientObject;
--
--                  //only perform raycast if filterMask matches
--                  if(m_resultCallback.needsCollision(collisionObject->getBroadphaseHandle())) {
--                          //RigidcollisionObject* collisionObject = ctrl->GetRigidcollisionObject();
--                          m_world->objectQuerySingle(m_castShape, m_convexFromTrans,m_convexToTrans,
--                                  collisionObject,
--                                  collisionObject->getCollisionShape(),
--                                  collisionObject->getWorldTransform(),
--                                  m_resultCallback,
--                                  m_allowedCcdPenetration);
--                  }
--
--                  return true;
--          }
--  };







--  void        impact.d3.Space::convexSweepTest(const impact.d3.Shape.convex* castShape, const impact.d3.Transform& convexFromWorld, const impact.d3.Transform& convexToWorld, ConvexResultCallback& resultCallback, impact.d3.Scalar allowedCcdPenetration) const
--  {
--
--          BT_PROFILE("convexSweepTest");
--          /// use the broadphase to accelerate the search for objects, based on their aabb
--          /// and for each object with ray-aabb overlap, perform an exact ray test
--          /// unfortunately the implementation for rayTest and convexSweepTest duplicated, albeit practically identical
--
--
--
--          impact.d3.Transform        convexFromTrans,convexToTrans;
--          convexFromTrans = convexFromWorld;
--          convexToTrans = convexToWorld;
--          impact.d3.Vector castShapeAabbMin, castShapeAabbMax;
--          /* Compute AABB that encompasses angular movement */
--          {
--                  impact.d3.Vector linVel, angVel;
--                  impact.d3.TransformUtil::calculateVelocity (convexFromTrans, convexToTrans, 1.0, linVel, angVel);
--                  impact.d3.Vector zeroLinVel;
--                  zeroLinVel.setValue(0,0,0);
--                  impact.d3.Transform R;
--                  R.setIdentity ();
--                  R.setRotation (convexFromTrans.getRotation());
--                  castShape->calculateTemporalAabb (R, zeroLinVel, angVel, 1.0, castShapeAabbMin, castShapeAabbMax);
--          }
--
--  #ifndef USE_BRUTEFORCE_RAYBROADPHASE
--
--          btSingleSweepCallback        convexCB(castShape,convexFromWorld,convexToWorld,this,resultCallback,allowedCcdPenetration);
--
--          m_broadphasePairCache->rayTest(convexFromTrans.getOrigin(),convexToTrans.getOrigin(),convexCB,castShapeAabbMin,castShapeAabbMax);
--
--  #else
--          /// go over all objects, and if the ray intersects their aabb + cast shape aabb,
--          // do a ray-shape query using convexCaster (CCD)
--          int i;
--          for (i=0;i<m_collisionObjects.size();i++)
--          {
--                  impact.d3.Object*        collisionObject= m_collisionObjects[i];
--                  //only perform raycast if filterMask matches
--                  if(resultCallback.needsCollision(collisionObject->getBroadphaseHandle())) {
--                          //RigidcollisionObject* collisionObject = ctrl->GetRigidcollisionObject();
--                          impact.d3.Vector collisionObjectAabbMin,collisionObjectAabbMax;
--                          collisionObject->getCollisionShape()->getAabb(collisionObject->getWorldTransform(),collisionObjectAabbMin,collisionObjectAabbMax);
--                          AabbExpand (collisionObjectAabbMin, collisionObjectAabbMax, castShapeAabbMin, castShapeAabbMax);
--                          impact.d3.Scalar hitLambda = impact.d3.Scalar(1.); //could use resultCallback.m_closestHitFraction, but needs testing
--                          impact.d3.Vector hitNormal;
--                          if (btRayAabb(convexFromWorld.getOrigin(),convexToWorld.getOrigin(),collisionObjectAabbMin,collisionObjectAabbMax,hitLambda,hitNormal))
--                          {
--                                  objectQuerySingle(castShape, convexFromTrans,convexToTrans,
--                                          collisionObject,
--                                          collisionObject->getCollisionShape(),
--                                          collisionObject->getWorldTransform(),
--                                          resultCallback,
--                                          allowedCcdPenetration);
--                          }
--                  }
--          }
--  #endif //USE_BRUTEFORCE_RAYBROADPHASE
--  }









--  struct btBridgedManifoldResult : public impact.d3.collision.manifold_Result
--  {
--
--          impact.d3.Space::ContactResultCallback&        m_resultCallback;
--
--          btBridgedManifoldResult( impact.d3.Object* obj0,impact.d3.Object* obj1,impact.d3.Space::ContactResultCallback& resultCallback )
--                  :impact.d3.collision.manifold_Result(obj0,obj1),
--                  m_resultCallback(resultCallback)
--          {
--          }
--
--          virtual void addContactPoint(const impact.d3.Vector& normalOnBInWorld,const impact.d3.Vector& pointInWorld,impact.d3.Scalar depth)
--          {
--                  bool isSwapped = m_manifoldPtr->getBody0() != m_body0;
--                  impact.d3.Vector pointA = pointInWorld + normalOnBInWorld * depth;
--                  impact.d3.Vector localA;
--                  impact.d3.Vector localB;
--                  if (isSwapped)
--                  {
--                          localA = m_rootTransB.invXform(pointA );
--                          localB = m_rootTransA.invXform(pointInWorld);
--                  } else
--                  {
--                          localA = m_rootTransA.invXform(pointA );
--                          localB = m_rootTransB.invXform(pointInWorld);
--                  }
--
--                  impact.d3.manifold_Point newPt(localA,localB,normalOnBInWorld,depth);
--                  newPt.m_positionWorldOnA = pointA;
--                  newPt.m_positionWorldOnB = pointInWorld;
--
--             //BP mod, store contact triangles.
--                  if (isSwapped)
--                  {
--                          newPt.m_partId0 = m_partId1;
--                          newPt.m_partId1 = m_partId0;
--                          newPt.m_index0  = m_index1;
--                          newPt.m_index1  = m_index0;
--                  } else
--                  {
--                          newPt.m_partId0 = m_partId0;
--                          newPt.m_partId1 = m_partId1;
--                          newPt.m_index0  = m_index0;
--                          newPt.m_index1  = m_index1;
--                  }
--
--                  //experimental feature info, for per-triangle material etc.
--                  impact.d3.Object* obj0 = isSwapped? m_body1 : m_body0;
--                  impact.d3.Object* obj1 = isSwapped? m_body0 : m_body1;
--                  m_resultCallback.addSingleResult(newPt,obj0,newPt.m_partId0,newPt.m_index0,obj1,newPt.m_partId1,newPt.m_index1);
--
--          }
--
--  };










--  struct btSingleContactCallback : public btBroadphaseAabbCallback
--  {
--
--          impact.d3.Object* m_collisionObject;
--          impact.d3.Space*        m_world;
--          impact.d3.Space::ContactResultCallback&        m_resultCallback;
--
--
--          btSingleContactCallback(impact.d3.Object* collisionObject, impact.d3.Space* world,impact.d3.Space::ContactResultCallback& resultCallback)
--                  :m_collisionObject(collisionObject),
--                  m_world(world),
--                  m_resultCallback(resultCallback)
--          {
--          }
--
--          virtual bool        process(const impact.d3.collision.Proxy* proxy)
--          {
--                  impact.d3.Object*        collisionObject = (impact.d3.Object*)proxy->m_clientObject;
--                  if (collisionObject == m_collisionObject)
--                          return true;
--
--                  //only perform raycast if filterMask matches
--                  if(m_resultCallback.needsCollision(collisionObject->getBroadphaseHandle()))
--                  {
--                          impact.d3.collision.Algorithm* algorithm = m_world->getDispatcher()->findAlgorithm(m_collisionObject,collisionObject);
--                          if (algorithm)
--                          {
--                                  btBridgedManifoldResult contactPointResult(m_collisionObject,collisionObject, m_resultCallback);
--                                  //discrete collision detection query
--                                  algorithm->processCollision(m_collisionObject,collisionObject, m_world->getDispatchInfo(),&contactPointResult);
--
--                                  algorithm->~impact.d3.collision.Algorithm();
--                                  m_world->getDispatcher()->freeCollisionAlgorithm(algorithm);
--                          }
--                  }
--                  return true;
--          }
--  };







--  ///contactTest performs a discrete collision test against all objects in the impact.d3.Space, and calls the resultCallback.
--  ///it reports one or more contact points for every overlapping object (including the one with deepest penetration)
--
--  void        impact.d3.Space::contactTest( impact.d3.Object* colObj, ContactResultCallback& resultCallback)
--  {
--          impact.d3.Vector aabbMin,aabbMax;
--          colObj->getCollisionShape()->getAabb(colObj->getWorldTransform(),aabbMin,aabbMax);
--          btSingleContactCallback        contactCB(colObj,this,resultCallback);
--
--          m_broadphasePairCache->aabbTest(aabbMin,aabbMax,contactCB);
--  }






--  ///contactTest performs a discrete collision test between two collision objects and calls the resultCallback if overlap if detected.
--  ///it reports one or more contact points (including the one with deepest penetration)
--
--  void        impact.d3.Space::contactPairTest(impact.d3.Object* colObjA, impact.d3.Object* colObjB, ContactResultCallback& resultCallback)
--  {
--          impact.d3.collision.Algorithm* algorithm = getDispatcher()->findAlgorithm(colObjA,colObjB);
--          if (algorithm)
--          {
--                  btBridgedManifoldResult contactPointResult(colObjA,colObjB, resultCallback);
--                  //discrete collision detection query
--                  algorithm->processCollision(colObjA,colObjB, getDispatchInfo(),&contactPointResult);
--
--                  algorithm->~impact.d3.collision.Algorithm();
--                  getDispatcher()->freeCollisionAlgorithm(algorithm);
--          }
--
--  }
