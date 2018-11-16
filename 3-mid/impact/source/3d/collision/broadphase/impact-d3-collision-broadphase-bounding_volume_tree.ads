with impact.d3.collision.Broadphase,
     impact.d3.collision.Proxy,
     impact.d3.collision.bounding_volume_Tree,
     ada.containers.Vectors;
with impact.d3.Dispatcher;
with System;
with impact.d3.collision.overlapped_pair_Callback.cached;
with Interfaces;



package impact.d3.collision.Broadphase.bounding_volume_Tree
--
--  The impact.d3.collision.bounding_volume_TreeBroadphase implements a broadphase using two dynamic AABB bounding volume hierarchies/trees (see impact.d3.collision.bounding_volume_Tree).
--
--  One tree is used for static/non-moving objects, and another tree is used for dynamic objects. Objects can move from one tree to the other.
--  This is a very fast broadphase, especially for very dynamic worlds where many objects are moving. Its insert/add and remove of objects is generally faster than the sweep and prune broadphases btAxisSweep3 and bt32BitAxisSweep3.
--
--  Original implementation by Nathanael Presson.
--
is


   type Item is limited new impact.d3.collision.Broadphase.item with private;
   type View is access all Item'Class;





   --  Compile time config
   --

   --  #define DBVT_BP_SORTPAIRS                                1

   DBVT_BP_PREVENTFALSEUPDATE : constant Boolean   := False;
   DBVT_BP_ACCURATESLEEPING   : constant Boolean   := False;
   DBVT_BP_ENABLE_BENCHMARK   : constant Boolean   := False;
   DBVT_BP_SORTPAIRS          : constant Boolean   := False;

   DBVT_BP_MARGIN             : constant math.Real := 0.05;



   --  Config
   --
   DYNAMIC_SET : constant := 1;  -- Dynamic set index
   FIXED_SET   : constant := 2;  -- Fixed set index

   STAGECOUNT  : constant := 2;  -- Number of stages









   --  impact.d3.collision.bounding_volume_TreeProxy
   --
   type Proxy is tagged;
   type Link_array  is array (Integer range 1 .. 2) of access Proxy;


   type Proxy is new impact.d3.collision.Proxy.item with
      record
         aabb  : impact.d3.collision.bounding_volume_Tree.AabbMm;
         leaf  : access impact.d3.collision.bounding_volume_Tree.Node'Class;
         links : Link_array;
         stage : Integer;
      end record;

   type Proxy_view is access all Proxy'Class;


   function to_Proxy (aabbMin, aabbMax     : in     math.Vector_3;
                            userPtr              : access Any'Class;
                            collisionFilterGroup : in     impact.d3.collision.Proxy.CollisionFilterGroups;
                            collisionFilterMask  : in     impact.d3.collision.Proxy.CollisionFilterGroups) return bounding_volume_Tree.Proxy;


   package Proxy_Vectors is new ada.containers.Vectors (Positive, Proxy_view);
   subtype ProxyArray    is     Proxy_Vectors.Vector;








   --- impact.d3.collision.bounding_volume_TreeBroadphase
   --

   --  Forge
   --
--     function   to_impact.d3.collision.bounding_volume_TreeBroadphase (paircache : access impact.d3.collision.overlapped_pair_Callback.cached.Item := null) return Item;
   function  new_Broadphase (paircache : access impact.d3.collision.overlapped_pair_Callback.cached.Item := null) return View;


   overriding procedure destruct            (Self      : in out Item);





   --  Operations
   --

   procedure collide  (Self : in out Item;   dispatcher : access impact.d3.Dispatcher.item'Class);
   procedure optimize (Self : in out Item);





   overriding function  createProxy (Self : access Item;    aabbMin, aabbMax     : in     math.Vector_3;
                                                 shapeType            : in     impact.d3.collision.Proxy.BroadphaseNativeTypes;
                                                 userPtr              : access Any'Class;
                                                 collisionFilterGroup : in     impact.d3.collision.Proxy.CollisionFilterGroups;
                                                 collisionFilterMask  : in     impact.d3.collision.Proxy.CollisionFilterGroups;
                                                 dispatcher           : access impact.d3.Dispatcher.item'Class;
                                                 multiSapProxy        : in     System.Address) return access impact.d3.collision.Proxy.item'Class;


   overriding procedure destroyProxy (Self : access Item;   proxy      : access impact.d3.collision.Proxy.item'Class;
                                                 dispatcher : access impact.d3.Dispatcher.item'Class);




   overriding procedure setAabb (Self : access Item;   absproxy         : access impact.d3.collision.Proxy.item'Class;
                                            aabbMin, aabbMax : in     math.Vector_3;
                                            dispatcher       : access impact.d3.Dispatcher.item'Class);


   overriding procedure getAabb                  (Self : in Item;   proxy            : access impact.d3.collision.Proxy.item;
                                                         aabbMin, aabbMax :    out math.Vector_3);




   overriding procedure rayTest (Self : access Item;   rayFrom, rayTo   : in     math.Vector_3;
                                                             rayCallback      : in out impact.d3.collision.Broadphase.btBroadphaseRayCallback'Class;
                      aabbMin, aabbMax : in     math.Vector_3 := (0.0, 0.0, 0.0));


   overriding procedure aabbTest (Self : access Item;   aabbMin, aabbMax : in     math.Vector_3;
                       callback         : in out impact.d3.collision.Broadphase.btBroadphaseAabbCallback'Class);




   --  calculateOverlappingPairs is optional: incremental algorithms (sweep and prune) might do it during the set aabb
   --
   overriding procedure calculateOverlappingPairs (Self : access Item;   dispatcher : access impact.d3.Dispatcher.item'Class);


   overriding function getOverlappingPairCache    (Self : in Item) return access impact.d3.collision.overlapped_pair_Callback.cached.item'Class;

   overriding function getOverlappingPairCache    (Self : in Item) return impact.d3.collision.overlapped_pair_Callback.cached.item'Class;




   --  getAabb returns the axis aligned bounding box in the 'global' coordinate frame
   --  will add some transform later
   --
   overriding procedure getBroadphaseAabb         (Self : in Item;   aabbMin, aabbMax : in out math.Vector_3);


   --  Reset broadphase internal structures, to ensure determinism/reproducability.
   --
   overriding procedure resetPool (Self : in out Item;   dispatcher : access impact.d3.Dispatcher.item);






   procedure performDeferredRemoval    (Self : access Item;   dispatcher : access impact.d3.Dispatcher.item'Class);

   procedure setVelocityPrediction     (Self : in out Item;   prediction : in     math.Real);
   function  getVelocityPrediction     (Self : in     Item)                return math.Real;




   --          This setAabbForceUpdate is similar to setAabb but always forces the aabb update.
   --          It is not part of the impact.d3.collision.Broadphase but specific to impact.d3.collision.bounding_volume_TreeBroadphase.
   --          It bypasses certain optimizations that prevent aabb updates (when the aabb shrinks), see
   --          http://code.google.com/p/bullet/issues/detail?id=223
   --
   procedure setAabbForceUpdate (Self : in out Item'Class;   absproxy         : access impact.d3.collision.Proxy.item'Class;
                                                             aabbMin, aabbMax : in     math.Vector_3;
                                 dispatcher       : access impact.d3.Dispatcher.item);







private

   type Sets_t            is array (1 .. 2) of aliased impact.d3.collision.bounding_volume_Tree.item;
   type Proxy_views is array (Positive range <>) of Proxy_view;

   type cached_overlapped_pair_Callback_view is access all impact.d3.collision.overlapped_pair_Callback.cached.Item'Class;



   type Item is limited new impact.d3.collision.Broadphase.item with
      record
         m_sets            : Sets_t;                                                -- Dbvt sets
         m_stageRoots      : Proxy_views (1 .. STAGECOUNT + 1);             -- Stages list
         m_paircache       : cached_overlapped_pair_Callback_view;                -- Pair cache

         m_prediction      : math.Real;                                                -- Velocity prediction

         m_stageCurrent,                                                        -- Current stage
         m_fupdates,                                                                -- % of fixed updates per frame
         m_dupdates,                                                                -- % of dynamic updates per frame
         m_cupdates,                                                                -- % of cleanup updates per frame
         m_newpairs,                                                                -- Number of pairs created
         m_fixedleft       : Integer;                                                        -- Fixed optimization left

         m_updates_call,                                                        -- Number of updates call
         m_updates_done    : interfaces.Unsigned_32;                                -- Number of updates done
         m_updates_ratio   : math.Real;                                                -- m_updates_done/m_updates_call

         m_pid,                                                                 -- Parse id
         m_cid,                                                                 -- Cleanup index
         m_gid             : Integer;                                           -- Gen id

         m_releasepaircache,                                                        -- Release pair cache on delete
         m_deferedcollide,                                                        -- Defere dynamic/static collision to collide call
         m_needcleanup     : Boolean;                                                -- Need to run cleanup ?
      end record;






   --- Colliders
   --


   --  Tree collider
   --

   type TreeCollider is new impact.d3.collision.bounding_volume_Tree.ICollide with
      record
         pbp   : access impact.d3.collision.broadphase.bounding_volume_Tree.Item'Class;
         proxy : access bounding_volume_Tree.Proxy;
      end record;

   overriding
   procedure Process (Self : in out TreeCollider;   na, nb : access impact.d3.collision.bounding_volume_Tree.Node'Class);

   overriding
   procedure Process (Self : in out TreeCollider;   n      : access impact.d3.collision.bounding_volume_Tree.Node'Class);


end impact.d3.collision.Broadphase.bounding_volume_Tree;
