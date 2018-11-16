with impact.d3.Shape.convex;
with impact.d3.Dispatcher;
with impact.d3.collision.Broadphase;
with impact.d3.collision.Configuration;
with impact.d3.Object;
with ada.containers.Vectors;
with impact.d3.collision.overlapped_pair_Callback.cached;
with impact.d3.collision.Proxy;
with impact.d3.manifold_Point;
with impact.d3.Shape;



package impact.d3.Space
--
--   CollisionWorld is interface and container for the collision detection
--
--
--   Bullet Documentation
--
--   Introduction
--   Bullet Collision Detection & Physics SDK
--
--   Bullet is a Collision Detection and Rigid Body Dynamics Library.
--
--   Installation
--
--   Step 1: Download
--   You can download the Bullet Physics Library from the Google Code repository: http://code.google.com/p/bullet/downloads/list
--
--   Step 2: Building
--   Bullet main build system for all platforms is cmake, you can download http://www.cmake.org
--   cmake can autogenerate projectfiles for Microsoft Visual Studio, Apple Xcode, KDevelop and Unix Makefiles.
--   The easiest is to run the CMake cmake-gui graphical user interface and choose the options and generate projectfiles.
--   You can also use cmake in the command-line. Here are some examples for various platforms:
--   cmake . -G "Visual Studio 9 2008"
--   cmake . -G Xcode
--   cmake . -G "Unix Makefiles"
--   Although cmake is recommended, you can also use autotools for UNIX: ./autogen.sh ./configure to create a Makefile and then run make.
--
--   Step 3: Testing demos
--   Try to run and experiment with BasicDemo executable as a starting point.
--   Bullet can be used in several ways, as Full Rigid Body simulation, as Collision Detector Library or Low Level / Snippets like the GJK Closest Point calculation.
--   The Dependencies can be seen in this documentation under Directories
--
--   Step 4: Integrating in your application, full Rigid Body and Soft Body simulation
--   Check out BasicDemo how to create a impact.d3.Space.dynamic, impact.d3.Object.rigid and impact.d3.Shape, Stepping the simulation and synchronizing your graphics object transform.
--   Check out SoftDemo how to use soft body dynamics, using btSoftRigidDynamicsWorld.
--
--   Step 5 : Integrate the Collision Detection Library (without Dynamics and other Extras)
--   Bullet Collision Detection can also be used without the Dynamics/Extras.
--   Check out impact.d3.Space and impact.d3.Object, and the CollisionInterfaceDemo.
--
--   Step 6 : Use Snippets like the GJK Closest Point calculation.
--   Bullet has been designed in a modular way keeping dependencies to a minimum. The ConvexHullDistance demo demonstrates direct use of impact.d3.collision.Detector.discrete.gjk_pair.
--
is
   use Math;



   type Item is tagged private;
   type View is access all Item'Class;


   --- Constants
   --
   DISABLE_DBVT_COMPOUNDSHAPE_RAYCAST_ACCELERATION : constant Boolean := False;



   --- Containers
   --

--     type impact.d3.Object_view is access all impact.d3.Object.item'Class;
--
--     package impact.d3.Object_Vectors is new ada.containers.Vectors (Positive, impact.d3.Object_view);
--     subtype impact.d3.Object_Vector  is     impact.d3.Object_Vectors.Vector;



   package real_Vectors is new ada.containers.Vectors (Positive, math.Real);
   subtype real_Vector  is     real_Vectors.Vector;



   use type math.Vector_3;
   package vector_3_Vectors is new ada.containers.Vectors (Positive, math.Vector_3);
   subtype vector_3_Vector  is     vector_3_Vectors.Vector;





   --- Forge
   --

--     package Forge
--     is
--
--        function to_impact.d3.Space (dispatcher             : access impact.d3.Dispatcher.item'Class;
--                                      broadphasePairCache    : access impact.d3.collision.Broadphase.item'Class;
--                                      collisionConfiguration : access impact.d3.collision.Configuration.item'Class) return Item'Class;
--        --
--        -- This constructor doesn't own the dispatcher and paircache/broadphase
--
--
--        procedure define  (Self : out Item;   dispatcher             : access impact.d3.Dispatcher.item'Class;
--                                              broadphasePairCache    : access impact.d3.collision.Broadphase.item'Class;
--                                              collisionConfiguration : access impact.d3.collision.Configuration.item'Class);
--     end Forge;


   procedure destroy (Self : in out Item);






   --- Attributes
   --

   procedure setBroadphase (Self : in out Item;   pairCache : access impact.d3.collision.Broadphase.item'Class);
   function  getBroadphase (Self : in     Item)        return access impact.d3.collision.Broadphase.item'Class;


--          void        setBroadphase(impact.d3.collision.Broadphase*        pairCache)
--          {
--                  m_broadphasePairCache = pairCache;
--          }


--          impact.d3.collision.Broadphase*        getBroadphase()
--          {
--                  return m_broadphasePairCache;
--          }


   function  getPairCache (Self : in     Item)        return access impact.d3.collision.overlapped_pair_Callback.cached.item'Class;

--          impact.d3.collision.overlapped_pair_Callback.cached*        getPairCache()
--          {
--                  return m_broadphasePairCache->getOverlappingPairCache();
--          }
--



   function  getDispatcher (Self : in     Item)        return access impact.d3.Dispatcher.item'Class;

--          impact.d3.Dispatcher*        getDispatcher()
--          {
--                  return m_dispatcher1;
--          }
--




   procedure updateSingleAabb (Self : in out Item;   colObj : access impact.d3.Object.item'Class);
   procedure updateAabbs      (Self : in out Item);





   --  LocalShapeInfo gives extra information for complex shapes
   --  Currently, only impact.d3.Shape.concave.triangle_mesh is available, so it just contains triangleIndex and subpart
   --
   type LocalShapeInfo is
      record
         m_shapePart,
         m_triangleIndex : Integer;
      end record;





   type LocalRayResult is
      record
         m_collisionObject : access impact.d3.Object.item'Class;
         m_localShapeInfo  : access LocalShapeInfo;
         m_hitNormalLocal  :        math.Vector_3;
         m_hitFraction     :        math.Real;
      end record;


   function to_LocalRayResult (collisionObject : access impact.d3.Object.item'Class;
                               localShapeInfo  : access impact.d3.Space.LocalShapeInfo;
                               hitNormalLocal  : in     math.Vector_3;
                               hitFraction     : in     math.Real) return LocalRayResult;


--          struct        LocalRayResult
--          {
--                  LocalRayResult(impact.d3.Object*        collisionObject,
--                          LocalShapeInfo*        localShapeInfo,
--                          const impact.d3.Vector&                hitNormalLocal,
--                          impact.d3.Scalar hitFraction)
--                  :m_collisionObject(collisionObject),
--                  m_localShapeInfo(localShapeInfo),
--                  m_hitNormalLocal(hitNormalLocal),
--                  m_hitFraction(hitFraction)
--                  {
--                  }
--          };








   --  RayResultCallback is used to report new raycast results
   --
   type RayResultCallback is abstract tagged
      record
         m_closestHitFraction   : math.Real    := 1.0;
         m_collisionObject      : access impact.d3.Object.item'Class;

         m_collisionFilterGroup : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.DefaultFilter;
         m_collisionFilterMask  : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.AllFilter;

         m_flags                : Flags := 0;   -- @BP Mod - Custom flags, currently used to enable backface culling on tri-meshes, see impact.d3.triangle_Callback.raycast
      end record;

   type RayResultCallback_view is access all RayResultCallback'Class;


   procedure destruct (Self : in out RayResultCallback) is null;



   function  hasHit   (Self : in     RayResultCallback) return Boolean;

--                  bool        hasHit() const
--                  {
--                          return (m_collisionObject != 0);
--                  }



   function  needsCollision (Self : in     RayResultCallback;   proxy0 : access impact.d3.collision.Proxy.item'Class) return Boolean;

--                  virtual bool needsCollision(impact.d3.collision.Proxy* proxy0) const
--                  {
--                          bool collides = (proxy0->m_collisionFilterGroup & m_collisionFilterMask) != 0;
--                          collides = collides && (m_collisionFilterGroup & proxy0->m_collisionFilterMask);
--                          return collides;
--                  }




   function  addSingleResult (Self : access RayResultCallback;   rayResult          : access LocalRayResult;
                                                                 normalInWorldSpace : in     Boolean     ) return math.Real
                              is abstract;










   type ClosestRayResultCallback is new RayResultCallback with
      record
         m_rayFromWorld,   -- used to calculate hitPointWorld from hitFraction
         m_rayToWorld,

         m_hitNormalWorld,
         m_hitPointWorld : math.Vector_3;
      end record;



   function to_ClosestRayResultCallback (rayFromWorld,
                                         rayToWorld  : in math.Vector_3) return ClosestRayResultCallback;

--                  ClosestRayResultCallback(const impact.d3.Vector&        rayFromWorld,const impact.d3.Vector&        rayToWorld)
--                  :m_rayFromWorld(rayFromWorld),
--                  m_rayToWorld(rayToWorld)
--                  {
--                  }




   overriding function  addSingleResult (Self : access ClosestRayResultCallback;   rayResult          : access LocalRayResult;
                                                                        normalInWorldSpace : in     Boolean     ) return math.Real;

--                  virtual        impact.d3.Scalar        addSingleResult(LocalRayResult& rayResult,bool normalInWorldSpace)
--                  {
--                          //caller already does the filter on the m_closestHitFraction
--                          btAssert(rayResult.m_hitFraction <= m_closestHitFraction);
--
--                          m_closestHitFraction = rayResult.m_hitFraction;
--                          m_collisionObject = rayResult.m_collisionObject;
--                          if (normalInWorldSpace)
--                          {
--                                  m_hitNormalWorld = rayResult.m_hitNormalLocal;
--                          } else
--                          {
--                                  ///need to transform normal into worldspace
--                                  m_hitNormalWorld = m_collisionObject->getWorldTransform().getBasis()*rayResult.m_hitNormalLocal;
--                          }
--                          m_hitPointWorld.setInterpolate3(m_rayFromWorld,m_rayToWorld,rayResult.m_hitFraction);
--                          return rayResult.m_hitFraction;
--                  }







   type AllHitsRayResultCallback is new RayResultCallback with
      record
         m_collisionObjects : impact.d3.Object.Vector;

         m_rayFromWorld,                                        -- used to calculate hitPointWorld from hitFraction
         m_rayToWorld       : math.Vector_3;

         m_hitNormalWorld   : vector_3_Vector;
         m_hitPointWorld    : vector_3_Vector;
         m_hitFractions     : real_Vector;
      end record;




   function to_AllHitsRayResultCallback (rayFromWorld,
                                         rayToWorld  : in math.Vector_3) return AllHitsRayResultCallback;

--                  AllHitsRayResultCallback(const impact.d3.Vector&        rayFromWorld,const impact.d3.Vector&        rayToWorld)
--                  :m_rayFromWorld(rayFromWorld),
--                  m_rayToWorld(rayToWorld)
--                  {
--                  }



   overriding function  addSingleResult (Self : access     AllHitsRayResultCallback;   rayResult          : access LocalRayResult;
                                                                        normalInWorldSpace : in     Boolean     ) return math.Real;


--                  virtual        impact.d3.Scalar        addSingleResult(LocalRayResult& rayResult,bool normalInWorldSpace)
--                  {
--                          m_collisionObject = rayResult.m_collisionObject;
--                          m_collisionObjects.push_back(rayResult.m_collisionObject);
--                          impact.d3.Vector hitNormalWorld;
--                          if (normalInWorldSpace)
--                          {
--                                  hitNormalWorld = rayResult.m_hitNormalLocal;
--                          } else
--                          {
--                                  ///need to transform normal into worldspace
--                                  hitNormalWorld = m_collisionObject->getWorldTransform().getBasis()*rayResult.m_hitNormalLocal;
--                          }
--                          m_hitNormalWorld.push_back(hitNormalWorld);
--                          impact.d3.Vector hitPointWorld;
--                          hitPointWorld.setInterpolate3(m_rayFromWorld,m_rayToWorld,rayResult.m_hitFraction);
--                          m_hitPointWorld.push_back(hitPointWorld);
--                          m_hitFractions.push_back(rayResult.m_hitFraction);
--                          return m_closestHitFraction;
--                  }










   type LocalConvexResult is
      record
         m_hitCollisionObject : access impact.d3.Object.item;
         m_localShapeInfo     : access LocalShapeInfo;

         m_hitNormalLocal,
         m_hitPointLocal      : math.Vector_3;

         m_hitFraction : math.Real;
      end record;


   function to_LocalConvexResult (hitCollisionObject : access impact.d3.Object.item;
                                  localShapeInfo     : access impact.d3.Space.LocalShapeInfo;

                                  hitNormalLocal,
                                  hitPointLocal      : in     math.Vector_3;

                                  hitFraction        : in     math.Real) return LocalConvexResult;


--                  LocalConvexResult(impact.d3.Object*        hitCollisionObject,
--                          LocalShapeInfo*        localShapeInfo,
--                          const impact.d3.Vector&                hitNormalLocal,
--                          const impact.d3.Vector&                hitPointLocal,
--                          impact.d3.Scalar hitFraction
--                          )
--                  :m_hitCollisionObject(hitCollisionObject),
--                  m_localShapeInfo(localShapeInfo),
--                  m_hitNormalLocal(hitNormalLocal),
--                  m_hitPointLocal(hitPointLocal),
--                  m_hitFraction(hitFraction)
--                  {
--                  }











   --  ConvexResultCallback is used to report new raycast results
   --
   type ConvexResultCallback is abstract tagged
      record
         m_closestHitFraction   : math.Real := 1.0;

         m_collisionFilterGroup : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.DefaultFilter;
         m_collisionFilterMask  : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.AllFilter;
      end record;



   procedure destruct (Self : in out ConvexResultCallback) is null;



   function  hasHit   (Self : in     ConvexResultCallback) return Boolean;

--                  bool        hasHit() const
--                  {
--                          return (m_closestHitFraction < impact.d3.Scalar(1.));
--                  }


   function  needsCollision (Self : in     ConvexResultCallback;   proxy0 : access impact.d3.collision.Proxy.item'Class) return Boolean;

--                  virtual bool needsCollision(impact.d3.collision.Proxy* proxy0) const
--                  {
--                          bool collides = (proxy0->m_collisionFilterGroup & m_collisionFilterMask) != 0;
--                          collides = collides && (m_collisionFilterGroup & proxy0->m_collisionFilterMask);
--                          return collides;
--                  }



   function  addSingleResult (Self : access     ConvexResultCallback;   convexResult       : access LocalConvexResult;
                                                                        normalInWorldSpace : in     Boolean     ) return math.Real
                              is abstract;









   type ClosestConvexResultCallback is new ConvexResultCallback with
      record
         m_convexFromWorld,                        -- used to calculate hitPointWorld from hitFraction
         m_convexToWorld,

         m_hitNormalWorld,
         m_hitPointWorld : math.Vector_3;

         m_hitCollisionObject : access impact.d3.Object.item;
      end record;



   package Forge
   is
      function to_Space (dispatcher             : access impact.d3.Dispatcher.item'Class;
                         broadphasePairCache    : access impact.d3.collision.Broadphase.item'Class;
                         collisionConfiguration : access impact.d3.collision.Configuration.item'Class) return Item'Class;
      --
      --  This constructor doesn't own the dispatcher and paircache/broadphase


      procedure define  (Self : out Item;   dispatcher             : access impact.d3.Dispatcher.item'Class;
                                            broadphasePairCache    : access impact.d3.collision.Broadphase.item'Class;
                                            collisionConfiguration : access impact.d3.collision.Configuration.item'Class);





      function to_ClosestConvexResultCallback (convexFromWorld, convexToWorld : in math.Vector_3) return ClosestConvexResultCallback;

--                  ClosestConvexResultCallback(const impact.d3.Vector&        convexFromWorld,const impact.d3.Vector&        convexToWorld)
--                  :m_convexFromWorld(convexFromWorld),
--                  m_convexToWorld(convexToWorld),
--                  m_hitCollisionObject(0)
--                  {
--                  }

   end Forge;



   overriding function  addSingleResult (Self : access ClosestConvexResultCallback;   convexResult       : access LocalConvexResult;
                                                                           normalInWorldSpace : in     Boolean        ) return math.Real;

--                  virtual        impact.d3.Scalar        addSingleResult(LocalConvexResult& convexResult,bool normalInWorldSpace)
--                  {
--  //caller already does the filter on the m_closestHitFraction
--                          btAssert(convexResult.m_hitFraction <= m_closestHitFraction);
--
--                          m_closestHitFraction = convexResult.m_hitFraction;
--                          m_hitCollisionObject = convexResult.m_hitCollisionObject;
--                          if (normalInWorldSpace)
--                          {
--                                  m_hitNormalWorld = convexResult.m_hitNormalLocal;
--                          } else
--                          {
--                                  ///need to transform normal into worldspace
--                                  m_hitNormalWorld = m_hitCollisionObject->getWorldTransform().getBasis()*convexResult.m_hitNormalLocal;
--                          }
--                          m_hitPointWorld = convexResult.m_hitPointLocal;
--                          return convexResult.m_hitFraction;
--                  }













   --  ContactResultCallback is used to report contact points
   --
   type ContactResultCallback is abstract tagged
      record
         m_collisionFilterGroup : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.DefaultFilter;
         m_collisionFilterMask  : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.AllFilter;
      end record;



   procedure destruct (Self : in out ContactResultCallback) is null;



   function  needsCollision (Self : in     ContactResultCallback;   proxy0 : access impact.d3.collision.Proxy.item) return Boolean;

--                  virtual bool needsCollision(impact.d3.collision.Proxy* proxy0) const
--                  {
--                          bool collides = (proxy0->m_collisionFilterGroup & m_collisionFilterMask) != 0;
--                          collides = collides && (m_collisionFilterGroup & proxy0->m_collisionFilterMask);
--                          return collides;
--                  }


   function  addSingleResult (Self : access ContactResultCallback;   cp      : access impact.d3.manifold_Point.item;

                                                                     colObj0 : in     impact.d3.Object.item'Class;
                                                                     partId0 : in     Integer;
                                                                     index0  : in     Integer;

                                                                     colObj1 : in     impact.d3.Object.item'Class;
                                                                     partId1 : in     Integer;
                                                                     index1  : in     Integer) return math.Real
                              is abstract;






   function  getNumCollisionObjects (Self : in Item) return Natural;


   --          int        getNumCollisionObjects() const
   --          {
   --                  return int(m_collisionObjects.size());
   --          }



   --  rayTest performs a raycast on all objects in the impact.d3.Space, and calls the resultCallback
   --  This allows for several queries: first hit, all hits, any hit, dependent on the value returned by the callback.
   --
   procedure rayTest (Self : access Item;   rayFromWorld, rayToWorld : in     math.Vector_3;
                      resultCallback           : access RayResultCallback'Class);





   --  convexTest performs a swept convex cast on all objects in the impact.d3.Space, and calls the resultCallback
   --  This allows for several queries: first hit, all hits, any hit, dependent on the value return by the callback.
   --
   procedure convexSweepTest (Self : in Item;   castShape             : in     impact.d3.Shape.convex.item'Class;
                                                From, To              : in     math.Transform_3d;
                                                resultCallback        : access ConvexResultCallback'Class;
                                                allowedCcdPenetration : in     math.Real := 0.0);





   --  contactTest performs a discrete collision test between colObj against all objects in the impact.d3.Space, and calls the resultCallback.
   --  it reports one or more contact points for every overlapping object (including the one with deepest penetration)
   --
   procedure contactTest     (Self : in Item;   colObj         : in     impact.d3.Object.item'Class;
                                                resultCallback : access ContactResultCallback 'Class);




   --  contactPairTest performs a discrete collision test between two collision objects and calls the resultCallback if overlap if detected.
   --  it reports one or more contact points (including the one with deepest penetration)
   --
   procedure contactPairTest (Self : in Item;   colObjA, colObjB : in     impact.d3.Object.item'Class;
                                                resultCallback   : access ContactResultCallback 'Class);






   --  rayTestSingle performs a raycast call and calls the resultCallback. It is used internally by rayTest.
   --  In a future implementation, we consider moving the ray test as a virtual method in impact.d3.Shape.
   --  This allows more customization.
   --
   procedure rayTestSingle (rayFromTrans, rayToTrans : access math.Transform_3d;
                            collisionObject          : access impact.d3.Object.item'Class;
                            collisionShape           : access impact.d3.Shape.item'Class;
                            colObjWorldTransform     : access math.Transform_3d;
                            resultCallback           : access RayResultCallback'Class);




   --  objectQuerySingle performs a collision detection query and calls the resultCallback. It is used internally by rayTest.
   --
   procedure objectQuerySingle (castShape                : in     impact.d3.Shape.convex.item'Class;
                                rayFromTrans, rayToTrans : in     math.Transform_3d;
                                collisionObject          : access impact.d3.Object.item'Class;
                                collisionShape           : in     impact.d3.Shape.item'Class;
                                colObjWorldTransform     : in     math.Transform_3d;
                                resultCallback           : access ConvexResultCallback'Class;
                                allowedPenetration       : in     math.Real);






   procedure addCollisionObject (Self : in out  Item;   collisionObject      : access impact.d3.Object.item'Class;
                                                        collisionFilterGroup : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.DefaultFilter;
                                                        collisionFilterMask  : impact.d3.collision.Proxy.CollisionFilterGroups := impact.d3.collision.Proxy.AllFilter);



   function getCollisionObjectArray (Self : access Item) return access impact.d3.Object.Vector;


--          impact.d3.ObjectArray& getCollisionObjectArray()
--          {
--                  return m_collisionObjects;
--          }




   procedure removeCollisionObject (Self : in out  Item;   collisionObject      : access impact.d3.Object.item'Class);


   procedure performDiscreteCollisionDetection (Self : in out  Item);






   function getDispatchInfo (Self : access Item) return access impact.d3.Dispatcher.DispatcherInfo;

--          impact.d3.DispatcherInfo& getDispatchInfo()
--          {
--                  return m_dispatchInfo;
--          }





   function getForceUpdateAllAabbs (Self : in Item) return Boolean;

--          bool        getForceUpdateAllAabbs() const
--          {
--                  return m_forceUpdateAllAabbs;
--          }





   procedure setForceUpdateAllAabbs (Self : in out  Item;   forceUpdateAllAabbs : in Boolean);

--          void setForceUpdateAllAabbs( bool forceUpdateAllAabbs)
--          {
--                  m_forceUpdateAllAabbs = forceUpdateAllAabbs;
--          }





   --- 'Protected' functions
   --

   function get_m_collisionObjects (Self : access Item) return access impact.d3.Object.Vector;





private

   type Item is tagged
      record
         m_collisionObjects    : aliased impact.d3.Object.Vector;

         m_dispatcher1         : access  impact.d3.Dispatcher.item'Class;
         m_dispatchInfo        : aliased impact.d3.Dispatcher.DispatcherInfo;

         --          btStackAlloc*        m_stackAlloc;

         m_broadphasePairCache : access  impact.d3.collision.Broadphase.item'Class;

         --  m_forceUpdateAllAabbs can be set to false as an optimization to only update active object AABBs
         --  it is true by default, because it is error-prone (setting the position of static objects wouldn't update their AABB)
         --
         m_forceUpdateAllAabbs : Boolean;
      end record;

end impact.d3.Space;
