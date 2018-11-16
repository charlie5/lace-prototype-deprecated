with impact.d3.Dispatcher,
     impact.d3.Object,
     impact.d3.Manifold,
     impact.d3.collision.Algorithm,
     impact.d3.collision.Proxy,
     impact.d3.collision.create_Func,
     impact.d3.collision.manifold_Result,
     impact.d3.collision.overlapped_pair_Callback.cached,
     impact.d3.collision.Configuration;
with System;
with impact.d3.Collision.create_Func;



package impact.d3.Dispatcher.collision
--
--  'impact.d3.Dispatcher.collision' supports algorithms that handle ConvexConvex and ConvexConcave collision pairs.
--  Time of Impact, Closest Points and Penetration Depth.
--
is



   type Item is new  impact.d3.Dispatcher.item with private;








   USE_DISPATCH_REGISTRY_ARRAY : constant Boolean := True;


   --- DispatcherFlags
   --
   CD_STATIC_STATIC_REPORTED                  : constant Flags := 1;
   CD_USE_RELATIVE_CONTACT_BREAKING_THRESHOLD : constant Flags := 2;
   CD_DISABLE_CONTACTPOOL_DYNAMIC_ALLOCATION  : constant Flags := 4;







   --  User can override this nearcallback for collision filtering and more finegrained control over collision detection.
   --
   type btNearCallback is access procedure (collisionPair : access impact.d3.collision.Proxy.btBroadphasePair;
                                            dispatcher    : access impact.d3.Dispatcher.collision.Item'Class;
                                            dispatchInfo  :    out impact.d3.Dispatcher.DispatcherInfo);



--     type impact.d3.Manifold_view is access all impact.d3.Manifold.item'Class;
--     type impact.d3.collision.Algorithm_view is access all impact.d3.collision.Algorithm.item'Class;





   function  to_Dispatcher (collisionConfiguration : access impact.d3.collision.Configuration.Item'Class) return Item;
   overriding procedure destruct (Self : in out Item);




   overriding function  findAlgorithm (Self : access Item;   body0, body1   : access impact.d3.Object.item'Class;
                                                  sharedManifold : access impact.d3.Manifold.Item'Class          := null) return Algorithm_view;


   overriding function  getNewManifold  (Self : access Item;   bod0, bod1 : access Any'Class                    ) return access impact.d3.Manifold.Item'Class;
   overriding procedure releaseManifold (Self : in out Item;   manifold     : access impact.d3.Manifold.Item'Class);
   overriding procedure clearManifold   (Self : in out Item;   manifold     : access impact.d3.Manifold.Item'Class);



   function getDispatcherFlags (Self : in Item) return Flags;


   procedure setDispatcherFlags (Self : in out Item;   To : in Flags);



   --  'registerCollisionCreateFunc' allows registration of custom/alternative collision create functions.
   --
   procedure registerCollisionCreateFunc (Self : in out Item;   proxyType0, proxyType1 : in     impact.d3.collision.Proxy.BroadphaseNativeTypes;
                                          createFunc             : access impact.d3.collision.create_Func.item);





   overriding function  getNumManifolds            (Self : in     Item) return Natural;
--     function  getInternalManifoldPointer (Self : access Item) return access impact.d3.Manifold_view;

   overriding
   function  getInternalManifoldPointer (Self : access Item) return access impact.d3.Manifold.Vector;







--     function  getManifoldByIndexInternal (Self : access Item;   index : in Integer) return impact.d3.Manifold_view;
   overriding
   function  getManifoldByIndexInternal (Self : in     Item;   index : in Integer) return impact.d3.Manifold.view;





--     function  findAlgorithm   (Self : access Item;   body0, body1 : access impact.d3.Object.impact.d3.Object'Class) return access impact.d3.collision.Algorithm.item;
   overriding function  needsCollision  (Self : access Item;   body0, body1 : access impact.d3.Object.item'Class) return Boolean;
   overriding function  needsResponse   (Self : access Item;   body0, body1 : access impact.d3.Object.item'Class) return Boolean;



   overriding procedure dispatchAllCollisionPairs (Self : in out Item;   pairCache    : access impact.d3.collision.overlapped_pair_Callback.cached.item'Class;
                                                              dispatchInfo : access impact.d3.Dispatcher.DispatcherInfo;
                                        dispatcher   : access impact.d3.Dispatcher.item'Class);




   procedure setNearCallback (Self : in out Item;   nearCallback : in btNearCallback);
   function  getNearCallback (Self : in     Item)              return btNearCallback;



   --  By default, Bullet will use this near callback.
   --
   procedure defaultNearCallback (collisionPair : access impact.d3.collision.Proxy.btBroadphasePair;
                                  dispatcher    : access impact.d3.Dispatcher.collision.Item'Class;
                                  dispatchInfo  :    out impact.d3.Dispatcher.DispatcherInfo);




   function  allocateCollisionAlgorithm  (Self : access Item;   size : in     Integer) return system.Address;
   overriding procedure freeCollisionAlgorithm      (Self : in out Item;   ptr  : access impact.d3.collision.Algorithm.item'Class);


   function  getCollisionConfiguration  (Self : access Item) return access impact.d3.collision.Configuration.Item'Class;
   procedure setCollisionConfiguration  (Self : in out Item;   config    : access impact.d3.collision.Configuration.item'Class);









private

   type double_dispatch_Table is array (impact.d3.collision.Proxy.BroadphaseNativeTypes,
                                        impact.d3.collision.Proxy.BroadphaseNativeTypes) of access impact.d3.Collision.Create_Func.item'Class;



--     package impact.d3.Manifold_Vectors is new ada.Containers.Vectors (Positive, impact.d3.Manifold_view);
--     subtype impact.d3.Manifold_Vector  is     impact.d3.Manifold_Vectors.Vector;




   type Item is new  impact.d3.Dispatcher.item with
      record
         m_dispatcherFlags        : Flags;

         m_manifoldsPtr           : aliased impact.d3.Manifold.Vector;
         m_defaultManifoldResult  : impact.d3.collision.manifold_Result.item;

         m_nearCallback           : btNearCallback;
         m_doubleDispatch         : double_dispatch_Table;
         m_collisionConfiguration : access impact.d3.collision.Configuration.item'Class;

         --           m_collisionAlgorithmPoolAllocator : access btPoolAllocator;
         --           m_persistentManifoldPoolAllocator : access btPoolAllocator;
      end record;




end impact.d3.Dispatcher.collision;
