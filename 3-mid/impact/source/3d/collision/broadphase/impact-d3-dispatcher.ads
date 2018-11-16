with impact.d3.Object,
     impact.d3.Manifold;

limited
with impact.d3.collision.Algorithm,
     impact.d3.collision.overlapped_pair_Callback.cached;





package impact.d3.Dispatcher
--
--  The impact.d3.Dispatcher interface class can be used in combination with broadphase to dispatch calculations for overlapping pairs.
--
--  For example, for pairwise collision detection, calculating contact points stored in impact.d3.Manifold or user callbacks (game logic).
--
is
   use Math;



   --- DispatchFunc
   --

   type DispatchFunc is  (DISPATCH_DISCRETE,        DISPATCH_CONTINUOUS);
   for  DispatchFunc use (DISPATCH_DISCRETE => 1,   DISPATCH_CONTINUOUS => 2);




   --- DispatcherInfo
   --

   type DispatcherInfo is
      record
         m_timeStep        : Real       := 0.0;
         m_stepCount       : Integer      := 0;
         m_dispatchFunc    : DispatchFunc := DISPATCH_DISCRETE;
         m_timeOfImpact    : Real       := 1.0;
         m_useContinuous   : Boolean      := True;
         m_enableSatConvex : Boolean      := False;
         m_enableSPU       : Boolean      := True;
         m_useEpa          : Boolean      := True;

         m_allowedCcdPenetration               : Real := 0.04;
         m_useConvexConservativeDistanceUtil   : Boolean := False;
         m_convexConservativeDistanceThreshold : Real  := 0.0;
      end record;



   type Algorithm_view is access all impact.d3.collision.Algorithm.item'Class;




   --- impact.d3.Dispatcher
   --

   type Item is abstract tagged null record;


   procedure destruct (Self : in out Item)   is abstract;






   function  findAlgorithm (Self : access Item;   body0, body1   : access impact.d3.Object.item'Class;
                                                          sharedManifold : access impact.d3.Manifold.Item'Class := null) return Algorithm_view
                           is abstract;



   function  getNewManifold  (Self : access    Item;   bod0, bod1 : access Any'Class) return access impact.d3.Manifold.Item'Class
                             is abstract;



   procedure releaseManifold (Self : in out Item;   manifold : access impact.d3.Manifold.Item'Class)   is abstract;


   procedure clearManifold   (Self : in out Item;   manifold : access impact.d3.Manifold.Item'Class)   is abstract;



   procedure freeCollisionAlgorithm (Self : in out Item;   ptr : access impact.d3.collision.Algorithm.item'Class)   is abstract;

--          virtual        void freeCollisionAlgorithm(void* ptr) = 0;




   function  needsCollision (Self : access Item;   body0, body1 : access impact.d3.Object.item'Class) return Boolean     is abstract;

   function  needsResponse  (Self : access Item;   body0, body1 : access impact.d3.Object.item'Class) return Boolean     is abstract;



   procedure dispatchAllCollisionPairs (Self : in out Item;    pairCache    : access impact.d3.collision.overlapped_pair_Callback.cached.item'Class;
                                                                       dispatchInfo : access DispatcherInfo;
                                                                       dispatcher   : access d3.Dispatcher.item'Class)
                                        is abstract;

--          virtual void        dispatchAllCollisionPairs(impact.d3.collision.overlapped_pair_Callback.cached* pairCache,const impact.d3.DispatcherInfo& dispatchInfo,impact.d3.Dispatcher* dispatcher)  =0;







   function  getNumManifolds  (Self : in    Item) return Natural   is abstract;



   function  getManifoldByIndexInternal  (Self : in    Item;   index : in Integer) return impact.d3.Manifold.view   is abstract;



   function  getInternalManifoldPointer  (Self : access Item) return access impact.d3.Manifold.Vector   is abstract;

--          virtual        impact.d3.Manifold**        getInternalManifoldPointer() = 0;




--          virtual        btPoolAllocator*        getInternalManifoldPool()             = 0;
--          virtual        void*                   allocateCollisionAlgorithm(int size)  = 0;



end impact.d3.Dispatcher;
