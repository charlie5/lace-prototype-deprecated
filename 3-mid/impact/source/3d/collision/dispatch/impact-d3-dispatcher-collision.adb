with ada.Unchecked_Deallocation;
with impact.d3.Containers;
with impact.d3.collision.Proxy;


package body impact.d3.Dispatcher.collision
is


   --- Globals
   --

   gNumManifold : Integer := 0;



   type Object_view is access all impact.d3.Object.item'Class;




   --- Forge
   --

   function  to_Dispatcher (collisionConfiguration : access impact.d3.collision.Configuration.Item'Class) return Item
   is
      use impact.d3.collision.Proxy;
      Self : Item;
   begin
      Self.m_dispatcherFlags        := (impact.d3.Dispatcher.collision.CD_USE_RELATIVE_CONTACT_BREAKING_THRESHOLD);
      Self.m_collisionConfiguration := collisionConfiguration;


      Self.setNearCallback (defaultNearCallback'Access);

      --             m_collisionAlgorithmPoolAllocator = collisionConfiguration->getCollisionAlgorithmPool();
      --             m_persistentManifoldPoolAllocator = collisionConfiguration->getPersistentManifoldPool();

      for i in BroadphaseNativeTypes
      loop

         for j in BroadphaseNativeTypes
         loop
            Self.m_doubleDispatch (i, j) := Self.m_collisionConfiguration.getCollisionAlgorithmCreateFunc (i, j);

            pragma Assert (Self.m_doubleDispatch (i, j) /= null);
         end loop;

      end loop;


      return Self;
   end to_Dispatcher;







   overriding procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;











   --- Attributes
   --


   overriding function  findAlgorithm (Self : access Item;   body0, body1   : access impact.d3.Object.item'Class;
                            sharedManifold : access impact.d3.Manifold.Item'Class          := null) return Algorithm_view
   is
      ci   : impact.d3.collision.Algorithm.AlgorithmConstructionInfo;
      algo : impact.d3.Dispatcher.Algorithm_view;

   begin
      ci.m_dispatcher1 := Self;
      ci.m_manifold    := sharedManifold;
      algo             := Self.m_doubleDispatch (body0.getCollisionShape.getShapeType,
                                                 body1.getCollisionShape.getShapeType).CreateCollisionAlgorithm (ci,  body0, body1);

      return algo;
   end findAlgorithm;





   overriding function  getNewManifold (Self : access Item;   bod0, bod1 : access Any'Class                    ) return access impact.d3.Manifold.Item'Class
   is
      use impact.d3.Manifold;

      body0 : constant impact.d3.Object.view := impact.d3.Object.view (bod0);
      body1 : constant impact.d3.Object.view := impact.d3.Object.view (bod1);

      contactBreakingThreshold,
      contactProcessingThreshold : math.Real;
   begin
      gNumManifold := gNumManifold + 1;

      pragma Assert (gNumManifold < 65535);



      --  optional relative contact breaking threshold, turned on by default (use setDispatcherFlags to switch off feature for improved performance).
      --
      if (Self.m_dispatcherFlags and impact.d3.Dispatcher.collision.CD_USE_RELATIVE_CONTACT_BREAKING_THRESHOLD) /= 0 then
         contactBreakingThreshold := math.Real'Min (body0.getCollisionShape.getContactBreakingThreshold (gContactBreakingThreshold),
                                                    body1.getCollisionShape.getContactBreakingThreshold (gContactBreakingThreshold));
      else
         contactBreakingThreshold := gContactBreakingThreshold;
      end if;



      contactProcessingThreshold := math.Real'Min (body0.getContactProcessingThreshold,
                                                   body1.getContactProcessingThreshold);

      declare
         manifold : constant impact.d3.Manifold.view := new impact.d3.Manifold.item'(to_Manifold (Containers.Any_view (body0), Containers.Any_view (body1),
                                                                                                          0,
                                                                                                          contactBreakingThreshold,
                                                                                                          contactProcessingThreshold));
      begin
         Self.m_manifoldsPtr.append (manifold);
         manifold.m_index1a := Integer (Self.m_manifoldsPtr.Length);

         return manifold;
      end;
   end getNewManifold;






   overriding procedure releaseManifold (Self : in out Item;   manifold     : access impact.d3.Manifold.Item'Class)
   is
      findIndex : Integer;
   begin
      gNumManifold := gNumManifold - 1;

      Self.clearManifold (manifold);

      findIndex := manifold.m_index1a;     pragma Assert (findIndex <= Integer (Self.m_manifoldsPtr.Length));

      Self.m_manifoldsPtr.swap (findIndex,  Integer (Self.m_manifoldsPtr.Length) - 0);
      Self.m_manifoldsPtr.Element (findIndex).m_index1a := findIndex;
      Self.m_manifoldsPtr.delete_Last;

      manifold.destruct;

      declare
         procedure free is new ada.Unchecked_Deallocation (impact.d3.Manifold.item'Class,  impact.d3.Manifold.view);
         the_Manifold : impact.d3.Manifold.view := manifold.all'Access;
      begin
         free (the_Manifold);
      end;
   end releaseManifold;








   overriding procedure clearManifold   (Self : in out Item;   manifold     : access impact.d3.Manifold.Item'Class)
   is
      pragma Unreferenced (Self);
   begin
      manifold.clearManifold;
   end clearManifold;





   function getDispatcherFlags (Self : in Item) return Flags
   is
   begin
      return Self.m_dispatcherFlags;
   end getDispatcherFlags;




   procedure setDispatcherFlags (Self : in out Item;   To : in Flags)
   is
   begin
      Self.m_dispatcherFlags := To;
   end setDispatcherFlags;



   --  'registerCollisionCreateFunc' allows registration of custom/alternative collision create functions.
   --
   procedure registerCollisionCreateFunc (Self : in out Item;   proxyType0, proxyType1 : in     impact.d3.collision.Proxy.BroadphaseNativeTypes;
                                          createFunc             : access impact.d3.collision.create_Func.item)
   is
   begin
      Self.m_doubleDispatch (proxyType0, proxyType1) := createFunc;
   end registerCollisionCreateFunc;





   overriding function getNumManifolds (Self : in     Item) return Natural
   is
   begin
      return Natural (Self.m_manifoldsPtr.Length);
   end getNumManifolds;




   overriding function  getInternalManifoldPointer (Self : access Item) return access impact.d3.Manifold.Vector
--     function getInternalManifoldPointer (Self : access Item) return access impact.d3.Manifold_view
   is
   begin
      return Self.m_manifoldsPtr'Access;
   end getInternalManifoldPointer;





   overriding function  getManifoldByIndexInternal (Self : in     Item;   index : in Integer) return impact.d3.Manifold.view
--     function getManifoldByIndexInternal (Self : access Item;   index : in Integer) return impact.d3.Manifold_view
   is
   begin
      return Self.m_manifoldsPtr.Element (index);
   end getManifoldByIndexInternal;













   overriding function  needsCollision (Self : access Item;   body0, body1 : access impact.d3.Object.item'Class) return Boolean
   is
      pragma Assert (body0 /= null);
      pragma Assert (body1 /= null);

      Result : Boolean := True;

   begin
      if         (not body0.isActive)
        and then (not body1.isActive)
      then
         Result := False;

      elsif not body0.checkCollideWith (body1) then
         Result := False;
      end if;


      return Result;
   end needsCollision;






   overriding function  needsResponse (Self : access Item;   body0, body1 : access impact.d3.Object.item'Class) return Boolean
   is
      pragma Unreferenced (Self);
      hasResponse : Boolean := body0.hasContactResponse and then body1.hasContactResponse;   -- Here you can do filtering.
   begin
      --  no response between two static/kinematic bodies:
      --
      hasResponse :=          hasResponse
                     and then (      (not body0.isStaticOrKinematicObject)
                               or else (not body1.isStaticOrKinematicObject));

      return hasResponse;
   end needsResponse;








   procedure setNearCallback (Self : in out Item;   nearCallback : in btNearCallback)
   is
   begin
      Self.m_nearCallback := nearCallback;
   end setNearCallback;





   function  getNearCallback (Self : in     Item)              return btNearCallback
   is
   begin
      return Self.m_nearCallback;
   end getNearCallback;






   --  By default, Bullet will use this near callback.
   --
   procedure defaultNearCallback (collisionPair : access impact.d3.collision.Proxy.btBroadphasePair;
                                  dispatcher    : access impact.d3.Dispatcher.collision.Item'Class;
                                  dispatchInfo  :    out impact.d3.Dispatcher.DispatcherInfo)
   is


      colObj0 : constant impact.d3.Object.view := impact.d3.Object.view (collisionPair.m_pProxy0.m_clientObject);
      colObj1 : constant impact.d3.Object.view := impact.d3.Object.view (collisionPair.m_pProxy1.m_clientObject);

      contactPointResult : aliased impact.d3.collision.manifold_Result.item;
      toi                :         math.Real;
   begin

      if dispatcher.needsCollision (colObj0, colObj1) then
         --  Dispatcher will keep algorithms persistent in the collision pair.
         --
         if collisionPair.m_algorithm = null then
            collisionPair.m_algorithm := dispatcher.findAlgorithm (colObj0, colObj1);
         end if;

         if collisionPair.m_algorithm /= null then
            contactPointResult := impact.d3.collision.manifold_Result.Forge.to_manifold_Result (colObj0, colObj1);

            if dispatchInfo.m_dispatchFunc = DISPATCH_DISCRETE then     -- Discrete collision detection query.
               collisionPair.m_algorithm.processCollision (colObj0, colObj1,
                                                           dispatchInfo,
                                                           contactPointResult);
            else                                                                         -- Continuous collision detection query, time of impact (toi).
               toi := collisionPair.m_algorithm.calculateTimeOfImpact (colObj0, colObj1,
                                                                       dispatchInfo,
                                                                       contactPointResult'Access);
               if dispatchInfo.m_timeOfImpact > toi then
                  dispatchInfo.m_timeOfImpact := toi;
               end if;
            end if;

         end if;
      end if;

   end defaultNearCallback;






   function  allocateCollisionAlgorithm  (Self : access Item;   size : in Integer) return system.Address
   is
      pragma Unreferenced (Self, size);
   begin
      return system.Null_Address;
   end allocateCollisionAlgorithm;





   overriding procedure freeCollisionAlgorithm (Self : in out Item;   ptr  : access impact.d3.collision.Algorithm.item'Class)
   is
      pragma Unreferenced (Self, ptr);
   begin
      return;
   end freeCollisionAlgorithm;




   function  getCollisionConfiguration  (Self : access Item) return access impact.d3.collision.Configuration.Item'Class
   is
   begin
      return Self.m_collisionConfiguration;
   end getCollisionConfiguration;



   procedure setCollisionConfiguration  (Self : in out Item;   config    : access impact.d3.collision.Configuration.item'Class)
   is
   begin
      Self.m_collisionConfiguration := config;
   end setCollisionConfiguration;





   --  Interface for iterating all overlapping collision pairs, no matter how those pairs are stored (array, set, map etc).
   --  This is useful for the collision dispatcher.
   --
   type btCollisionPairCallback (m_dispatchInfo : access impact.d3.Dispatcher.DispatcherInfo)
     is new impact.d3.collision.overlapped_pair_Callback.cached.btOverlapCallback with
      record
         m_dispatcher : access impact.d3.Dispatcher.collision.item'Class;
      end record;


   overriding function processOverlap (Self : in     btCollisionPairCallback;   pair : access impact.d3.collision.Proxy.btBroadphasePair) return Boolean;




   function to_btCollisionPairCallback (dispatchInfo : access impact.d3.Dispatcher.DispatcherInfo;
                                        dispatcher   : access impact.d3.Dispatcher.collision.item'Class) return btCollisionPairCallback
   is
      Self : btCollisionPairCallback (dispatchInfo);
   begin
      Self.m_dispatcher := dispatcher;

      return Self;
   end to_btCollisionPairCallback;




   overriding function processOverlap (Self : in     btCollisionPairCallback;   pair : access impact.d3.collision.Proxy.btBroadphasePair) return Boolean
   is
   begin
      Self.m_dispatcher.getNearCallback.all (pair,
                                             Self.m_dispatcher,
                                             Self.m_dispatchInfo.all);
      return False;
   end processOverlap;








   overriding procedure dispatchAllCollisionPairs (Self : in out Item;   pairCache    : access impact.d3.collision.overlapped_pair_Callback.cached.item'Class;
                                                              dispatchInfo : access impact.d3.Dispatcher.DispatcherInfo;
                                                              dispatcher   : access impact.d3.Dispatcher.item'Class)
   is
   begin
      --  m_blockedForChanges = true;

      declare
         collisionCallback : aliased btCollisionPairCallback := to_btCollisionPairCallback (dispatchInfo, Self'Unchecked_Access);
      begin
         pairCache.processAllOverlappingPairs (collisionCallback'Access,  dispatcher);
      end;

      --  m_blockedForChanges = false;
   end dispatchAllCollisionPairs;




end impact.d3.Dispatcher.collision;
