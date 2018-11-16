with impact.d3.Containers;

with ada.Containers.Vectors;
with impact.d3.collision.Proxy;
with Interfaces;
with impact.d3.Dispatcher;



package impact.d3.collision.overlapped_pair_Callback.cached
--
--  The 'impact.d3.collision.overlapped_pair_Callback.cached' provides an interface for overlapping pair management (add, remove, storage), used
--  by the impact.d3.collision.Broadphase broadphases.
--
--  The 'btHashedOverlappingPairCache' and 'btSortedOverlappingPairCache' classes are two implementations.
--
is


   type Item is abstract new impact.d3.collision.overlapped_pair_Callback.item with null record;




   --- Globals
   --

--     gRemovePairs,
--     gAddedPairs,
--     gFindPairs   : Integer;
--

   BT_NULL_PAIR : constant interfaces.Unsigned_32 := 16#ffff_ffff#;





   --- btBroadphasePairArray
   --
   use type impact.d3.collision.Proxy.btBroadphasePair;


   --  type btBroadphasePair_view is access all impact.d3.collision.Proxy.btBroadphasePair;

   use type impact.d3.collision.Proxy.btBroadphasePair_view;
   package btBroadphasePair_Vectors is new ada.Containers.Vectors (Positive, impact.d3.collision.Proxy.btBroadphasePair_view);
   subtype btBroadphasePairArray    is     btBroadphasePair_Vectors.Vector;







   --- btOverlapCallback
   --
   type btOverlapCallback is abstract tagged null record;


   procedure destruct       (Self : in out btOverlapCallback)
                             is null;

   function  processOverlap (Self : in     btOverlapCallback;   pair : access impact.d3.collision.Proxy.btBroadphasePair) return Boolean
                             is abstract;
   --
   --  Returns 'True' for deletion of the pair.







   --- btOverlapFilterCallback
   --
   type btOverlapFilterCallback is abstract tagged null record;


   procedure destruct                (Self : in out btOverlapFilterCallback)
                                      is null;

   function  needBroadphaseCollision (Self : in     btOverlapFilterCallback;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return Boolean
                                      is abstract;
   --
   --  Returns 'True' when pairs need collision








   --- 'impact.d3.collision.overlapped_pair_Callback.cached'
   --


   --- Forge
   --

   overriding procedure destruct (Self : in out Item)     is null;    -- This is needed so we can get to the derived class destructor.



   --- Attributes
   --

   function getOverlappingPairArrayPtr (Self : in     Item) return btBroadphasePair_Vectors.Cursor
                                        is abstract;


   function getOverlappingPairArray    (Self : access Item) return access btBroadphasePairArray
                                        is abstract;


   procedure cleanOverlappingPair      (Self : in out Item;   pair       : access impact.d3.collision.Proxy.btBroadphasePair;
                                                              dispatcher : access impact.d3.Dispatcher.item'Class)
   is abstract;



   function getNumOverlappingPairs     (Self : in Item) return Integer
                                        is abstract;


   procedure cleanProxyFromPairs       (Self : access Item;   callback : access impact.d3.collision.Proxy.item'Class;
                                                              dispatcher : access impact.d3.Dispatcher.item'Class)
   is abstract;


   procedure setOverlapFilterCallback  (Self : in out Item;   proxy      : access btOverlapFilterCallback'Class)
   is abstract;


   procedure processAllOverlappingPairs (Self : in out Item;   callback   : access btOverlapCallback'Class;
                                                               dispatcher : access impact.d3.Dispatcher.item'Class)
   is abstract;



   function findPair           (Self : in Item;   proxy0, proxy1 : access impact.d3.collision.Proxy.item) return access impact.d3.collision.Proxy.btBroadphasePair
                                is abstract;


   function hasDeferredRemoval (Self : in Item) return Boolean
                                is abstract;




   procedure setInternalGhostPairCallback (Self : in out Item;   ghostPairCallback   : access impact.d3.collision.overlapped_pair_Callback.item'Class)
   is abstract;



   procedure sortOverlappingPairs (Self : in out Item;   dispatcher : access impact.d3.Dispatcher.item'Class)
   is abstract;








   --------------------------------
   --- btHashedOverlappingPairCache
   --
   --  Hash-space based Pair Cache. Thanks to Erin Catto (Box2D, http://www.box2d.org) and Pierre Terdiman (Codercorner, http://codercorner.com).
   --

   type btHashedOverlappingPairCache      is new        impact.d3.collision.overlapped_pair_Callback.cached.item with private;
   type btHashedOverlappingPairCache_view is access all btHashedOverlappingPairCache'Class;




   --- Forge
   --


--     function  to_btHashedOverlappingPairCache return btHashedOverlappingPairCache;
   function new_btHashedOverlappingPairCache return btHashedOverlappingPairCache_view; -- access btHashedOverlappingPairCache'Class;

   overriding procedure destruct (Self : in out btHashedOverlappingPairCache);




   --- Attributes
   --

   overriding function getOverlappingPairArrayPtr (Self : in     btHashedOverlappingPairCache) return btBroadphasePair_Vectors.Cursor;


   overriding function getOverlappingPairArray    (Self : access btHashedOverlappingPairCache) return access btBroadphasePairArray;


   overriding procedure cleanOverlappingPair      (Self : in out btHashedOverlappingPairCache;   pair       : access impact.d3.collision.Proxy.btBroadphasePair;
                                                                                      dispatcher : access impact.d3.Dispatcher.item'Class);



   overriding function getNumOverlappingPairs     (Self : in btHashedOverlappingPairCache) return Integer;


   overriding procedure cleanProxyFromPairs (Self : access btHashedOverlappingPairCache;   proxy : access impact.d3.collision.Proxy.item'Class;
                                  dispatcher : access impact.d3.Dispatcher.item'Class);


   overriding procedure setOverlapFilterCallback (Self : in out btHashedOverlappingPairCache;   callback      : access btOverlapFilterCallback'Class);
   function  getOverlapFilterCallback  (Self : access btHashedOverlappingPairCache)         return access btOverlapFilterCallback'Class;


   overriding procedure processAllOverlappingPairs (Self : in out btHashedOverlappingPairCache;   callback   : access btOverlapCallback'Class;
                                                                                       dispatcher : access impact.d3.Dispatcher.item'Class);



   overriding function findPair           (Self : in btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item) return access impact.d3.collision.Proxy.btBroadphasePair;


   overriding function hasDeferredRemoval (Self : in btHashedOverlappingPairCache) return Boolean;




   overriding procedure setInternalGhostPairCallback (Self : in out btHashedOverlappingPairCache;   ghostPairCallback   : access impact.d3.collision.overlapped_pair_Callback.item'Class);



   overriding procedure sortOverlappingPairs (Self : in out btHashedOverlappingPairCache;   dispatcher : access impact.d3.Dispatcher.item'Class);





   overriding function  addOverlappingPair    (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return access impact.d3.collision.Proxy.btBroadphasePair;

   overriding function  removeOverlappingPair (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class;
                                                                                  dispatcher     : access impact.d3.Dispatcher.item'Class) return access Any'Class;

   overriding procedure removeOverlappingPairsContainingProxy (Self : access btHashedOverlappingPairCache;   proxy0     : access impact.d3.collision.Proxy.item'Class;
                                                                                                  dispatcher : access impact.d3.Dispatcher.item'Class);



   function needsBroadphaseCollision (Self : in btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return Boolean;


   function GetCount                 (Self : in btHashedOverlappingPairCache) return Integer;












   --------------------------------
   --- btSortedOverlappingPairCache
   --
   --  Maintains the objects with overlapping AABB.
   --  Typically managed by the Broadphase, Axis3Sweep or btSimpleBroadphase
   --
   type btSortedOverlappingPairCache is new impact.d3.collision.overlapped_pair_Callback.cached.Item with private;



   --- Forge
   --

   function  to_btSortedOverlappingPairCache return btSortedOverlappingPairCache;
   overriding procedure destruct                (Self : in out btSortedOverlappingPairCache);





   --- Attributes
   --

   overriding function getOverlappingPairArrayPtr (Self : in     btSortedOverlappingPairCache) return btBroadphasePair_Vectors.Cursor;


   overriding function getOverlappingPairArray    (Self : access btSortedOverlappingPairCache) return access btBroadphasePairArray;


   overriding procedure cleanOverlappingPair      (Self : in out btSortedOverlappingPairCache;   pair       : access impact.d3.collision.Proxy.btBroadphasePair;
                                                                                      dispatcher : access impact.d3.Dispatcher.item'Class);



   overriding function getNumOverlappingPairs     (Self : in btSortedOverlappingPairCache) return Integer;


   overriding procedure cleanProxyFromPairs (Self : access btSortedOverlappingPairCache;   proxy      : access impact.d3.collision.Proxy.item'Class;
                                                                                dispatcher : access impact.d3.Dispatcher.item'Class);


   overriding procedure processAllOverlappingPairs (Self : in out btSortedOverlappingPairCache;   callback   : access btOverlapCallback'Class;
                                                                                       dispatcher : access impact.d3.Dispatcher.item'Class);



   overriding function findPair           (Self : in btSortedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item) return access impact.d3.collision.Proxy.btBroadphasePair;


   overriding function hasDeferredRemoval (Self : in btSortedOverlappingPairCache) return Boolean;




   overriding procedure setInternalGhostPairCallback (Self : in out btSortedOverlappingPairCache;   ghostPairCallback   : access impact.d3.collision.overlapped_pair_Callback.item'Class);



   overriding procedure sortOverlappingPairs (Self : in out btSortedOverlappingPairCache;   dispatcher : access impact.d3.Dispatcher.item'Class);



   function needsBroadphaseCollision (Self : in btSortedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return Boolean;






   overriding function  addOverlappingPair (Self : access btSortedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return access impact.d3.collision.Proxy.btBroadphasePair;

   overriding function  removeOverlappingPair (Self : access btSortedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class;
                                                                                  dispatcher     : access impact.d3.Dispatcher.item'Class) return access Any'Class;

   overriding procedure removeOverlappingPairsContainingProxy (Self : access btSortedOverlappingPairCache;   proxy     : access impact.d3.collision.Proxy.item'Class;
                                                                                                  dispatcher : access impact.d3.Dispatcher.item'Class);



   function  getOverlapFilterCallback (Self : access btSortedOverlappingPairCache) return access btOverlapFilterCallback'Class;
   overriding procedure setOverlapFilterCallback (Self : in out btSortedOverlappingPairCache;   callback : access btOverlapFilterCallback'Class);




private

--     use type interfaces.Unsigned_32;
--     package unsigned_32_Vectors is new ada.Containers.Vectors (Positive, interfaces.Unsigned_32);
--     subtype unsigned_32_Vector  is     unsigned_32_Vectors.Vector;



   type btHashedOverlappingPairCache is new impact.d3.collision.overlapped_pair_Callback.cached.item with
      record
         m_overlappingPairArray  : aliased btBroadphasePairArray;
         m_overlapFilterCallback : access  btOverlapFilterCallback'Class;
         m_blockedForChanges     :         Boolean;

         m_hashTable             :        impact.d3.containers.unsigned_32_Vector;
         m_next                  :        impact.d3.containers.unsigned_32_Vector;
         m_ghostPairCallback     : access impact.d3.collision.overlapped_pair_Callback.item'Class;
      end record;



   function internalAddPair (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return access impact.d3.collision.Proxy.btBroadphasePair;
   procedure growTables     (Self : in out btHashedOverlappingPairCache);

   function equalsPair      (Self : in     btHashedOverlappingPairCache;   pair               : in impact.d3.collision.Proxy.btBroadphasePair;
                                                                           proxyId1, proxyId2 : in Integer) return Boolean;


   function internalFindPair (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class;
                                                                            hash           : in     Integer) return access impact.d3.collision.Proxy.btBroadphasePair;

   function internalFindPairIndex (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class;
                                                                                 hash           : in     Integer) return Integer;






   function getHash (proxyId1, proxyId2 : in Integer) return Interfaces.Unsigned_32;








   type btSortedOverlappingPairCache is new impact.d3.collision.overlapped_pair_Callback.cached.item with
      record
         m_overlappingPairArray  : aliased btBroadphasePairArray;           -- avoid brute-force finding all the time
         m_blockedForChanges     :         Boolean;                         -- during the dispatch, check that user doesn't destroy/create proxy
         m_hasDeferredRemoval    :         Boolean;                         -- by default, do the removal during the pair traversal
         m_overlapFilterCallback : access btOverlapFilterCallback'Class;    -- if set, use the callback instead of the built in filter in needBroadphaseCollision
         m_ghostPairCallback     : access impact.d3.collision.overlapped_pair_Callback.Item'Class;
      end record;


end impact.d3.collision.overlapped_pair_Callback.cached;





--  ///btNullPairCache skips add/removal of overlapping pairs. Userful for benchmarking and unit testing.
--
--  class btNullPairCache : public impact.d3.collision.overlapped_pair_Callback.cached
--  {
--
--          btBroadphasePairArray        m_overlappingPairArray;
--
--  public:
--
--          virtual btBroadphasePair*        getOverlappingPairArrayPtr()
--          {
--                  return &m_overlappingPairArray[0];
--          }
--          const btBroadphasePair*        getOverlappingPairArrayPtr() const
--          {
--                  return &m_overlappingPairArray[0];
--          }
--          btBroadphasePairArray&        getOverlappingPairArray()
--          {
--                  return m_overlappingPairArray;
--          }
--
--          virtual        void        cleanOverlappingPair(btBroadphasePair& /*pair*/,impact.d3.Dispatcher* /*dispatcher*/)
--          {
--
--          }
--
--          virtual int getNumOverlappingPairs() const
--          {
--                  return 0;
--          }
--
--          virtual void        cleanProxyFromPairs(impact.d3.collision.Proxy* /*proxy*/,impact.d3.Dispatcher* /*dispatcher*/)
--          {
--
--          }
--
--          virtual        void setOverlapFilterCallback(btOverlapFilterCallback* /*callback*/)
--          {
--          }
--
--          virtual void        processAllOverlappingPairs(btOverlapCallback*,impact.d3.Dispatcher* /*dispatcher*/)
--          {
--          }
--
--          virtual btBroadphasePair* findPair(impact.d3.collision.Proxy* /*proxy0*/, impact.d3.collision.Proxy* /*proxy1*/)
--          {
--                  return 0;
--          }
--
--          virtual bool        hasDeferredRemoval()
--          {
--                  return true;
--          }
--
--          virtual        void        setInternalGhostPairCallback(impact.d3.collision.overlapped_pair_Callback* /* ghostPairCallback */)
--          {
--
--          }
--
--          virtual btBroadphasePair*        addOverlappingPair(impact.d3.collision.Proxy* /*proxy0*/,impact.d3.collision.Proxy* /*proxy1*/)
--          {
--                  return 0;
--          }
--
--          virtual void*        removeOverlappingPair(impact.d3.collision.Proxy* /*proxy0*/,impact.d3.collision.Proxy* /*proxy1*/,impact.d3.Dispatcher* /*dispatcher*/)
--          {
--                  return 0;
--          }
--
--          virtual void        removeOverlappingPairsContainingProxy(impact.d3.collision.Proxy* /*proxy0*/,impact.d3.Dispatcher* /*dispatcher*/)
--          {
--          }
--
--          virtual void        sortOverlappingPairs(impact.d3.Dispatcher* dispatcher)
--          {
--          (void) dispatcher;
--          }
--
--
--  };


