with impact.d3.collision.Proxy,
     impact.d3.collision.overlapped_pair_Callback.cached;
with impact.d3.aabb_Util;
with impact.d3.Dispatcher;
with System;


package impact.d3.collision.Broadphase
--
--  The impact.d3.collision.Broadphase class provides an interface to detect aabb-overlapping object pairs.
--
--  Some implementations for this broadphase interface include btAxisSweep3, bt32BitAxisSweep3 and impact.d3.collision.bounding_volume_TreeBroadphase.
--  The actual overlapping pair management, storage, adding and removing of pairs is dealt by the impact.d3.collision.overlapped_pair_Callback.cached class.
--
--
is
   use Math;

   type Item is limited interface;





   --- btBroadphaseAabbCallback
   --

   type btBroadphaseAabbCallback is abstract tagged null record;


   procedure destruct (Self : in out btBroadphaseAabbCallback)
   is null;


   function  process  (Self : access btBroadphaseAabbCallback;   proxy : in  impact.d3.collision.Proxy.item'Class) return Boolean
                       is abstract;




   --- btBroadphaseRayCallback
   --

--     type Signs is array (1..3) of interfaces.unsigned_32;


   type btBroadphaseRayCallback is abstract new btBroadphaseAabbCallback with
      record
         --  added some cached data to accelerate ray-AABB tests
         --
         m_rayDirectionInverse : math.Vector_3;
         m_signs               : impact.d3.aabb_Util.Signs;
         m_lambda_max          : math.Real;
      end record;


--     function  process  (Self : access btBroadphaseRayCallback;   proxy : in  impact.d3.collision.Proxy.item) return Boolean
--       is abstract;






   --- impact.d3.collision.Broadphase
   --

   procedure destruct                 (Self : in out Item)
   is null;




   function  createProxy              (Self : access Item;   aabbMin, aabbMax     : in     math.Vector_3;
                                                             shapeType            : in     impact.d3.collision.Proxy.BroadphaseNativeTypes;
                                                             userPtr              : access Any'Class;
                                                             collisionFilterGroup : in     impact.d3.collision.Proxy.CollisionFilterGroups;
                                                             collisionFilterMask  : in     impact.d3.collision.Proxy.CollisionFilterGroups;
                                                             dispatcher           : access impact.d3.Dispatcher.item'Class;
                                                             multiSapProxy        : in     System.Address) return access impact.d3.collision.Proxy.item'Class
     is abstract;


   procedure destroyProxy             (Self : access Item;   proxy      : access impact.d3.collision.Proxy.item'Class;
                                                             dispatcher : access impact.d3.Dispatcher.item'Class)
     is abstract;




   procedure setAabb                  (Self : access Item;   absproxy         : access impact.d3.collision.Proxy.item'Class;
                                                             aabbMin, aabbMax : in     math.Vector_3;
                                                             dispatcher       : access impact.d3.Dispatcher.item'Class)
     is abstract;


   procedure getAabb                  (Self : in     Item;   proxy            : access impact.d3.collision.Proxy.item;
                                                             aabbMin, aabbMax :    out math.Vector_3)
     is abstract;




   procedure rayTest                  (Self : access Item;   rayFrom, rayTo   : in     math.Vector_3;
                                                             rayCallback      : in out btBroadphaseRayCallback'Class;
                                                             aabbMin, aabbMax : in     math.Vector_3 := (0.0, 0.0, 0.0))
     is abstract;


   procedure aabbTest                 (Self : access Item;   aabbMin, aabbMax : in     math.Vector_3;
                                                             callback         : in out btBroadphaseAabbCallback'Class)
     is abstract;




   --  calculateOverlappingPairs is optional: incremental algorithms (sweep and prune) might do it during the set aabb
   --
   procedure calculateOverlappingPairs (Self : access Item;   dispatcher : access impact.d3.Dispatcher.item'Class)
     is abstract;


   function getOverlappingPairCache    (Self : in Item) return access impact.d3.collision.overlapped_pair_Callback.cached.item'Class
     is abstract;

   function getOverlappingPairCache    (Self : in Item) return impact.d3.collision.overlapped_pair_Callback.cached.item'Class
     is abstract;




   --  getAabb returns the axis aligned bounding box in the 'global' coordinate frame
   --  will add some transform later
   --
   procedure getBroadphaseAabb         (Self : in Item;   aabbMin, aabbMax : in out math.Vector_3)
     is abstract;


   --  Reset broadphase internal structures, to ensure determinism/reproducability.
   --
   procedure resetPool                 (Self : in out Item;   dispatcher : access impact.d3.Dispatcher.item)
     is null;



end impact.d3.collision.Broadphase;








--  class impact.d3.collision.Broadphase
--  {
--  public:
--          virtual impact.d3.collision.Proxy*        createProxy(  const impact.d3.Vector& aabbMin,  const impact.d3.Vector& aabbMax,int shapeType,void* userPtr, short int collisionFilterGroup,short int collisionFilterMask, impact.d3.Dispatcher* dispatcher,void* multiSapProxy) =0;
--          virtual void        destroyProxy(impact.d3.collision.Proxy* proxy,impact.d3.Dispatcher* dispatcher)=0;
--          virtual void        setAabb(impact.d3.collision.Proxy* proxy,const impact.d3.Vector& aabbMin,const impact.d3.Vector& aabbMax, impact.d3.Dispatcher* dispatcher)=0;
--          virtual void        getAabb(impact.d3.collision.Proxy* proxy,impact.d3.Vector& aabbMin, impact.d3.Vector& aabbMax ) const =0;
--
--          virtual void        rayTest(const impact.d3.Vector& rayFrom,const impact.d3.Vector& rayTo, btBroadphaseRayCallback& rayCallback, const impact.d3.Vector& aabbMin=impact.d3.Vector(0,0,0), const impact.d3.Vector& aabbMax = impact.d3.Vector(0,0,0)) = 0;
--
--          virtual void        aabbTest(const impact.d3.Vector& aabbMin, const impact.d3.Vector& aabbMax, btBroadphaseAabbCallback& callback) = 0;
--
--          ///calculateOverlappingPairs is optional: incremental algorithms (sweep and prune) might do it during the set aabb
--          virtual void        calculateOverlappingPairs(impact.d3.Dispatcher* dispatcher)=0;
--
--          virtual        impact.d3.collision.overlapped_pair_Callback.cached*        getOverlappingPairCache()=0;
--          virtual        const impact.d3.collision.overlapped_pair_Callback.cached*        getOverlappingPairCache() const =0;
--
--          ///getAabb returns the axis aligned bounding box in the 'global' coordinate frame
--          ///will add some transform later
--          virtual void getBroadphaseAabb(impact.d3.Vector& aabbMin,impact.d3.Vector& aabbMax) const =0;
--
--          ///reset broadphase internal structures, to ensure determinism/reproducability
--          virtual void resetPool(impact.d3.Dispatcher* dispatcher) { (void) dispatcher; };
--
--          virtual void        printStats() = 0;
--
--  };

