with Ada.Containers;
with Ada.Unchecked_Conversion;
with impact.d3.collision.Proxy,
     impact.d3.Collision.Algorithm;


package body impact.d3.collision.overlapped_pair_Callback.cached
--
--
--
is

   -----------
   --- Globals
   --

   gOverlappingPairs : Integer := 0;

   gRemovePairs      : Integer := 0;
   gAddedPairs       : Integer := 0;
   gFindPairs        : Integer := 0;



   --------------------------------
   --- btHashedOverlappingPairCache
   --


   --- Forge
   --

   function to_btHashedOverlappingPairCache return btHashedOverlappingPairCache
   is
      Self : btHashedOverlappingPairCache;
   begin
      Self.m_blockedForChanges := False;

      Self.m_overlappingPairArray.reserve_Capacity (2);
      Self.growTables;

      return Self;
   end to_btHashedOverlappingPairCache;




   function new_btHashedOverlappingPairCache return btHashedOverlappingPairCache_view
   is
      Self : constant btHashedOverlappingPairCache_view := new btHashedOverlappingPairCache;
   begin
      Self.m_blockedForChanges := False;

      Self.m_overlappingPairArray.reserve_Capacity (2);
      Self.growTables;


      return Self;
--        return new btHashedOverlappingPairCache' (to_btHashedOverlappingPairCache);
   end new_btHashedOverlappingPairCache;






   overriding procedure destruct (Self : in out btHashedOverlappingPairCache)
   is
   begin
      null;
   end destruct;





   --- Attributes
   --

   overriding function getOverlappingPairArrayPtr (Self : in     btHashedOverlappingPairCache) return btBroadphasePair_Vectors.Cursor
   is
   begin
      return Self.m_overlappingPairArray.First;
   end getOverlappingPairArrayPtr;






   overriding function getOverlappingPairArray    (Self : access btHashedOverlappingPairCache) return access btBroadphasePairArray
   is
   begin
      return Self.m_overlappingPairArray'Access;
   end getOverlappingPairArray;




   overriding procedure cleanOverlappingPair (Self : in out btHashedOverlappingPairCache;   pair       : access impact.d3.collision.Proxy.btBroadphasePair;
                                                                                            dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      pragma Unreferenced (Self);
   begin
      if pair.m_algorithm /= null then
         pair.m_algorithm.destruct;
         dispatcher.freeCollisionAlgorithm (pair.m_algorithm);
         pair.m_algorithm := null;
      end if;
   end cleanOverlappingPair;




   overriding function getNumOverlappingPairs     (Self : in btHashedOverlappingPairCache) return Integer
   is
   begin
      return Integer (Self.m_overlappingPairArray.Length);
   end getNumOverlappingPairs;






   --- 'cleanProxyFromPairs'
   --

   type CleanPairCallback is new btOverlapCallback with
      record
         m_cleanProxy : access impact.d3.collision.Proxy.item'Class;
         m_pairCache  : access impact.d3.collision.overlapped_pair_Callback.cached.Item'Class;
         m_dispatcher : access impact.d3.Dispatcher.item'Class;
      end record;



   overriding function  processOverlap (Self : in     CleanPairCallback;   pair : access impact.d3.collision.Proxy.btBroadphasePair) return Boolean
   is
   begin
      if        pair.m_pProxy0 = Self.m_cleanProxy
        or else pair.m_pProxy1 = Self.m_cleanProxy
      then
         Self.m_pairCache.cleanOverlappingPair (pair, Self.m_dispatcher);
      end if;

      return False;
   end processOverlap;



   function to_CleanPairCallback (cleanProxy : access impact.d3.collision.Proxy.item'Class;
                                  pairCache  : access impact.d3.collision.overlapped_pair_Callback.cached.Item'Class;
                                  dispatcher : access impact.d3.Dispatcher.item'Class) return CleanPairCallback
   is
      Self : CleanPairCallback;
   begin
      Self.m_cleanProxy := cleanProxy;
      Self.m_pairCache  := pairCache;
      Self.m_dispatcher := dispatcher;

      return Self;
   end to_CleanPairCallback;




   overriding procedure cleanProxyFromPairs       (Self : access btHashedOverlappingPairCache;   proxy : access impact.d3.collision.Proxy.item'Class;
                                                                                      dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      cleanPairs : aliased CleanPairCallback := to_CleanPairCallback (proxy, Self, dispatcher);
   begin
      Self.processAllOverlappingPairs (cleanPairs'Access, dispatcher);
   end cleanProxyFromPairs;









   overriding procedure setOverlapFilterCallback  (Self : in out btHashedOverlappingPairCache;   callback      : access btOverlapFilterCallback'Class)
   is
   begin
      Self.m_overlapFilterCallback := callback;
   end setOverlapFilterCallback;







   function  getOverlapFilterCallback  (Self : access btHashedOverlappingPairCache)         return access btOverlapFilterCallback'Class
   is
   begin
      return Self.m_overlapFilterCallback;
   end getOverlapFilterCallback;





   function needsBroadphaseCollision (Self : in btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return Boolean
   is
      use type impact.d3.collision.Proxy.CollisionFilterGroups;

      collides : Boolean;
   begin
      if Self.m_overlapFilterCallback /= null then
         return Self.m_overlapFilterCallback.needBroadphaseCollision (proxy0, proxy1);
      end if;

      collides := (proxy0.m_collisionFilterGroup and proxy1.m_collisionFilterMask) /= 0;
      collides := collides and then (proxy1.m_collisionFilterGroup and proxy0.m_collisionFilterMask) /= 0;

      return collides;
   end needsBroadphaseCollision;








   function internalAddPair (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return access impact.d3.collision.Proxy.btBroadphasePair
   is
      use Interfaces;
      use type Ada.Containers.Count_type;

      the_proxy0 : access impact.d3.collision.Proxy.item'Class := proxy0;
      the_proxy1 : access impact.d3.collision.Proxy.item'Class := proxy1;

      proxyId1,
      proxyId2,
      hash   : Integer;

      pair : access impact.d3.collision.Proxy.btBroadphasePair;

      count,
      oldCapacity,
      newCapacity : Integer;

      unused : access impact.d3.collision.Proxy.btBroadphasePair;
      pragma Unreferenced (unused);


      function to_Integer is new ada.unchecked_Conversion (Unsigned_32, Integer);

   begin
      if proxy0.m_uniqueId > proxy1.m_uniqueId then
         declare
            Pad : constant access impact.d3.collision.Proxy.item := the_proxy0;
         begin
            the_proxy0 := the_proxy1;
            the_proxy1 := Pad;           -- btSwap (proxy0, proxy1);
         end;
      end if;

      proxyId1 := the_proxy0.getUid;
      proxyId2 := the_proxy1.getUid;

      declare
         kkk : Unsigned_32 := getHash (proxyId1, proxyId2);
         ppp : Integer := Integer (Self.m_overlappingPairArray.capacity - 1);
         jjj : Unsigned_32 := Unsigned_32 (Self.m_overlappingPairArray.capacity - 1);
      begin
         null;
      end;

      hash := to_Integer (getHash (proxyId1, proxyId2) and Unsigned_32 (Self.m_overlappingPairArray.capacity - 1));   -- New hash value with new mask.
      pair := Self.internalFindPair (the_proxy0, the_proxy1,  hash);

      if pair /= null then
         return pair;
      end if;

      count       := Integer (Self.m_overlappingPairArray.Length);
      oldCapacity := Integer (Self.m_overlappingPairArray.capacity);

--          void* mem = &m_overlappingPairArray.expandNonInitializing();

--        pair = new (mem) btBroadphasePair(*proxy0,*proxy1);
      pair                              := new impact.d3.collision.Proxy.btBroadphasePair'(impact.d3.collision.Proxy.to_btBroadphasePair (the_proxy0, the_proxy1));
      pair.m_algorithm                  := null;
      pair.internals.m_internalTmpValue := 0;

      Self.m_overlappingPairArray.append (pair);

      --  this is where we add an actual pair, so also call the 'ghost'
      --
      if Self.m_ghostPairCallback /= null then
         unused := Self.m_ghostPairCallback.addOverlappingPair (the_proxy0, the_proxy1);
      end if;

      newCapacity := Integer (Self.m_overlappingPairArray.capacity);

      if oldCapacity < newCapacity then
         Self.growTables;
         hash := Integer (getHash (proxyId1, proxyId2) and Unsigned_32 (Self.m_overlappingPairArray.capacity - 1));   -- hash with new capacity
      end if;


      Self.m_next     .replace_Element (count + 1, Unsigned_32'(Self.m_hashTable.Element (hash + 1)));
      Self.m_hashTable.replace_Element (hash + 1,  Unsigned_32 (count + 1));

      return pair;
   end internalAddPair;












   procedure growTables     (Self : in out btHashedOverlappingPairCache)
   is
      use type ada.containers.Count_type;

      newCapacity      : constant ada.containers.Count_type := Self.m_overlappingPairArray.capacity;
      curHashtableSize : Integer;

   begin

      if Self.m_hashTable.Length < newCapacity then                  -- grow 'hashtable' and 'next' table
         curHashtableSize := Integer (Self.m_hashTable.Length);

         Self.m_hashTable.set_Length (newCapacity);
         Self.m_next     .set_Length (newCapacity);

         for i in 1 .. Integer (newCapacity)
         loop
            Self.m_hashTable.replace_Element (i, BT_NULL_PAIR);
         end loop;


         for i in 1 .. Integer (newCapacity)
         loop
            Self.m_next.replace_Element (i, BT_NULL_PAIR);
         end loop;


         for i in 1 .. Integer (curHashtableSize)
         loop
            declare
               use Interfaces;

               pair      : constant access impact.d3.collision.Proxy.btBroadphasePair := Self.m_overlappingPairArray.Element (i);
               proxyId1  : constant Integer     := pair.m_pProxy0.getUid;
               proxyId2  : constant Integer     := pair.m_pProxy1.getUid;
               hashValue : constant Unsigned_32 := (getHash (proxyId1, proxyId2) and Unsigned_32 (Self.m_overlappingPairArray.capacity - 1))  +  1;        -- New hash value with new mask
            begin
               Self.m_next     .replace_Element (i,                    Unsigned_32'(Self.m_hashTable.Element (Integer (hashValue))));
               Self.m_hashTable.replace_Element (Integer (hashValue),  Unsigned_32 (i));
            end;
         end loop;

      end if;
   end growTables;






   function equalsPair      (Self : in     btHashedOverlappingPairCache;   pair               : in impact.d3.collision.Proxy.btBroadphasePair;
                                                                           proxyId1, proxyId2 : in Integer) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return     pair.m_pProxy0.getUid = proxyId1
        and then pair.m_pProxy1.getUid = proxyId2;
   end equalsPair;





   overriding procedure processAllOverlappingPairs (Self : in out btHashedOverlappingPairCache;   callback   : access btOverlapCallback'Class;
                                                                                       dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      pair   : access impact.d3.collision.Proxy.btBroadphasePair;
      i      :        Integer := 1;
      unused : access Any'Class;
      pragma Unreferenced (unused);

   begin
      while i <= Integer (Self.m_overlappingPairArray.Length)
      loop
         pair := Self.m_overlappingPairArray.Element (i);

         if callback.processOverlap (pair) then
            unused            := Self.removeOverlappingPair (pair.m_pProxy0, pair.m_pProxy1, dispatcher);
            gOverlappingPairs := gOverlappingPairs - 1;
         else
            i := i + 1;
         end if;
      end loop;
   end processAllOverlappingPairs;





   overriding function findPair           (Self : in btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item) return access impact.d3.collision.Proxy.btBroadphasePair
   is
      use Interfaces;

      the_proxy0 : access impact.d3.collision.Proxy.item := proxy0;
      the_proxy1 : access impact.d3.collision.Proxy.item := proxy1;

      index,
      hash     : Integer;

      proxyId1,
      proxyId2 : Integer;

   begin
      gFindPairs := gFindPairs + 1;

      if the_proxy0.m_uniqueId > the_proxy1.m_uniqueId then
         declare
            Pad : constant access impact.d3.collision.Proxy.item := the_proxy0;
         begin
            the_proxy0 := the_proxy1;
            the_proxy1 := Pad;           -- btSwap (proxy0, proxy1);
         end;
      end if;

      proxyId1 := the_proxy0.getUid;
      proxyId2 := the_proxy1.getUid;

      hash     := Integer (getHash (proxyId1, proxyId2) and (Unsigned_32 (Self.m_overlappingPairArray.capacity) - 1));

      if hash >= Integer (Self.m_hashTable.Length) then
         return null;
      end if;


      index := Integer (Unsigned_32'(Self.m_hashTable.Element (hash)));

      while      Unsigned_32 (index) /= BT_NULL_PAIR
        and then not Self.equalsPair (Self.m_overlappingPairArray.Element (index).all,  proxyId1, proxyId2)
      loop
         index := Integer (Unsigned_32'(Self.m_next.Element (index)));
      end loop;

      if Unsigned_32 (index) = BT_NULL_PAIR then
         return null;
      end if;

      pragma Assert (index <= Integer (Self.m_overlappingPairArray.Length));


      return Self.m_overlappingPairArray.Element (index);
   end findPair;








   overriding function hasDeferredRemoval (Self : in btHashedOverlappingPairCache) return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return False;
   end hasDeferredRemoval;




   overriding procedure setInternalGhostPairCallback (Self : in out btHashedOverlappingPairCache;   ghostPairCallback   : access impact.d3.collision.overlapped_pair_Callback.item'Class)
   is
   begin
      Self.m_ghostPairCallback := ghostPairCallback;
   end setInternalGhostPairCallback;




   overriding procedure sortOverlappingPairs (Self : in out btHashedOverlappingPairCache;   dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      --  need to keep hashmap in sync with pair address, so rebuild all
      tmpPairs : btBroadphasePairArray;

      function "<" (L, R : in impact.d3.collision.Proxy.btBroadphasePair_view) return Boolean
      is
      begin
         return impact.d3.collision.Proxy.btBroadphasePairSortPredicate (L.all, R.all);
      end;

      package Sorter is new btBroadphasePair_Vectors.generic_Sorting ("<");

      unused      : access Any'Class;
      pragma Unreferenced (unused);
      unused_pair : access impact.d3.collision.Proxy.btBroadphasePair;
      pragma Unreferenced (unused_pair);

   begin
      for i in 1 .. Integer (Self.m_overlappingPairArray.Length)
      loop
         tmpPairs.append (Self.m_overlappingPairArray.Element (i));
      end loop;


      for i in 1 .. Integer (tmpPairs.Length)
      loop
         unused := Self.removeOverlappingPair (tmpPairs.Element (i).m_pProxy0, tmpPairs.Element (i).m_pProxy1, dispatcher);
      end loop;


      for i in 1 .. Integer (Self.m_next.Length)
      loop
         Self.m_next.replace_Element (i, BT_NULL_PAIR);
      end loop;


      Sorter.sort (tmpPairs);       -- tmpPairs.quickSort (btBroadphasePairSortPredicate());

      for i in 1 .. Integer (tmpPairs.Length)
      loop
         unused_pair := Self.addOverlappingPair (tmpPairs.Element (i).m_pProxy0,
                                                 tmpPairs.Element (i).m_pProxy1);
      end loop;
   end sortOverlappingPairs;







   --  Add a pair and return the new pair. If the pair already exists,
   --  no new pair is created and the old one is returned.
   --
   overriding function  addOverlappingPair    (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return access impact.d3.collision.Proxy.btBroadphasePair
   is
   begin
      gAddedPairs := gAddedPairs + 1;

      if not Self.needsBroadphaseCollision (proxy0, proxy1) then
         return null;
      end if;

      return Self.internalAddPair (proxy0, proxy1);
   end addOverlappingPair;








   overriding function  removeOverlappingPair (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class;
                                                                                  dispatcher     : access impact.d3.Dispatcher.item'Class) return access Any'Class
   is
      use Interfaces;
      use type Ada.Containers.count_type;
      the_proxy0 : access impact.d3.collision.Proxy.item := proxy0;
      the_proxy1 : access impact.d3.collision.Proxy.item := proxy1;

      proxyId1,
      proxyId2 : Integer;

      hash : Integer;
      pair : access impact.d3.collision.Proxy.btBroadphasePair;

      userData : access Any'Class;

      pairIndex,
      lastPairIndex : Integer;
      index,
      previous      : Unsigned_32;


      last     : access impact.d3.collision.Proxy.btBroadphasePair;
      lastHash : Integer;

      unused : access Any'Class;
      pragma Unreferenced (unused);

   begin
      gRemovePairs := gRemovePairs + 1;

      if the_proxy0.m_uniqueId > the_proxy1.m_uniqueId then
         declare
            Pad : constant access impact.d3.collision.Proxy.item := the_proxy0;
         begin
            the_proxy0 := the_proxy1;
            the_proxy1 := Pad;           -- btSwap (proxy0, proxy1);
         end;
      end if;

      proxyId1 := the_proxy0.getUid;
      proxyId2 := the_proxy1.getUid;


      hash := Integer (getHash (proxyId1, proxyId2) and Unsigned_32 (Self.m_overlappingPairArray.capacity - 1));
      pair := Self.internalFindPair (the_proxy0, the_proxy1, hash);

      if pair = null then
         return null;
      end if;

      Self.cleanOverlappingPair (pair, dispatcher);

      userData := pair.internals.m_internalInfo1;

      pragma Assert (pair.m_pProxy0.getUid = proxyId1);
      pragma Assert (pair.m_pProxy1.getUid = proxyId2);

      pairIndex := Self.internalFindPairIndex (the_proxy0, the_proxy1, hash);   -- Integer (pair - Self.m_overlappingPairArray (1));

      pragma Assert (pairIndex <= Integer (Self.m_overlappingPairArray.Length));

      --  Remove the pair from the hash table.
      index := Self.m_hashTable.Element (hash + 1);
      pragma Assert (Unsigned_32 (index) /= BT_NULL_PAIR);

      previous := BT_NULL_PAIR;

      while index /= Unsigned_32 (pairIndex)
      loop
         previous := index;
         index    := Self.m_next.Element (Integer (index));
      end loop;

      if previous /= BT_NULL_PAIR then
         pragma Assert (Self.m_next.Element (Integer (previous)) = Unsigned_32 (pairIndex));
         Self.m_next.replace_Element (Integer (previous),   Unsigned_32'(Self.m_next.Element (pairIndex)));
      else
         Self.m_hashTable.replace_Element (hash + 1,  Unsigned_32'(Self.m_next.Element (pairIndex)));
      end if;

      --  We now move the last pair into spot of the
      --  pair being removed. We need to fix the hash
      --  table indices to support the move.

      lastPairIndex := Integer (Self.m_overlappingPairArray.Length - 0);

      if Self.m_ghostPairCallback /= null then
         unused := Self.m_ghostPairCallback.removeOverlappingPair (the_proxy0, the_proxy1, dispatcher);
      end if;

      --  If the removed pair is the last pair, we are done.
      --
      if lastPairIndex = pairIndex then
         Self.m_overlappingPairArray.delete_Last;
         return userData;
      end if;

      --  Remove the last pair from the hash table.
      last := Self.m_overlappingPairArray.Element (lastPairIndex);
      --  missing swap here too, Nat.
      lastHash := Integer (getHash (last.m_pProxy0.getUid, last.m_pProxy1.getUid) and Unsigned_32 (Self.m_overlappingPairArray.capacity - 1));

      index := Self.m_hashTable.Element (lastHash + 1);
      pragma Assert (index /= BT_NULL_PAIR);

      previous := BT_NULL_PAIR;

      while index /= Unsigned_32 (lastPairIndex)
      loop
         previous := index;
         index    := Self.m_next.Element (Integer (index));
      end loop;

      if previous /= BT_NULL_PAIR then
         pragma Assert (Unsigned_32'(Self.m_next.Element (Integer (previous))) = Unsigned_32 (lastPairIndex));
         Self.m_next.replace_Element (Integer (previous),  Unsigned_32'(Self.m_next.Element (lastPairIndex)));
      else
         Self.m_hashTable.replace_Element (lastHash + 1,  Unsigned_32'(Self.m_next.Element (lastPairIndex)));
      end if;

      --  Copy the last pair into the remove pair's spot.
      Self.m_overlappingPairArray.replace_Element (pairIndex,  impact.d3.collision.Proxy.btBroadphasePair_view'(Self.m_overlappingPairArray.Element (lastPairIndex)));

      --  Insert the last pair into the hash table
      Self.m_next.replace_Element (pairIndex,  Unsigned_32'(Self.m_hashTable.Element (lastHash + 1)));
      Self.m_hashTable.replace_Element (lastHash + 1,   Unsigned_32 (pairIndex));

      Self.m_overlappingPairArray.delete_Last;


      return userData;
   end removeOverlappingPair;







   --- 'removeOverlappingPairsContainingProxy'
   --

   type RemovePairCallback is new btOverlapCallback with
      record
         m_obsoleteProxy : access impact.d3.collision.Proxy.item'Class;
      end record;


   overriding function  processOverlap (Self : in     RemovePairCallback;   pair : access impact.d3.collision.Proxy.btBroadphasePair) return Boolean
   is
   begin
      return    pair.m_pProxy0 = Self.m_obsoleteProxy
        or else pair.m_pProxy1 = Self.m_obsoleteProxy;
   end processOverlap;


   function to_RemovePairCallback (obsoleteProxy : access impact.d3.collision.Proxy.item'Class) return RemovePairCallback
   is
      Self : RemovePairCallback;
   begin
      Self.m_obsoleteProxy := obsoleteProxy;
      return Self;
   end to_RemovePairCallback;




   overriding procedure removeOverlappingPairsContainingProxy (Self : access btHashedOverlappingPairCache;   proxy0     : access impact.d3.collision.Proxy.item'Class;
                                                                                                  dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      removeCallback : aliased RemovePairCallback := to_RemovePairCallback (proxy0);
   begin
      Self.processAllOverlappingPairs (removeCallback'Access, dispatcher);
   end removeOverlappingPairsContainingProxy;





   function GetCount (Self : in btHashedOverlappingPairCache) return Integer
   is
   begin
      return Integer (Self.m_overlappingPairArray.Length);
   end GetCount;





   function internalFindPairIndex (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class;
                                                                                 hash           : in     Integer) return Integer
   is
      use Interfaces;

      proxyId1 : constant Integer := proxy0.getUid;
      proxyId2 : constant Integer := proxy1.getUid;

      index    : Unsigned_32 := Self.m_hashTable.Element (hash + 1);
   begin
      while      Interfaces.unsigned_32 (index) /= BT_NULL_PAIR
        and then not Self.equalsPair (Self.m_overlappingPairArray.Element (Integer (index)).all,  proxyId1, proxyId2)
      loop
         index := Self.m_next.Element (Integer (index));
      end loop;

      if Interfaces.unsigned_32 (index) = BT_NULL_PAIR then
         return 0;
      end if;

      pragma Assert (index <= Unsigned_32 (Self.m_overlappingPairArray.Length));

      return Integer (index);
   end internalFindPairIndex;





   function internalFindPair (Self : access btHashedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class;
                                                                            hash           : in     Integer) return access collision.Proxy.btBroadphasePair
   is
      index : constant Integer := Self.internalFindPairIndex (proxy0, proxy1, hash);
   begin
      if index = 0 then
         return null;
      end if;

      return Self.m_overlappingPairArray.Element (index);
   end internalFindPair;






   function getHash (proxyId1, proxyId2 : in Integer) return Interfaces.Unsigned_32
   is
      use Interfaces;

      key : Unsigned_32 :=    Unsigned_32 (proxyId1)
                           or shift_Left (Unsigned_32 (proxyId2), 16);
   begin
      --  Thomas Wang's hash
      --
      key := key   +   not shift_Left  (key, 15);
      key := key   xor     shift_Right (key, 10);
      key := key   +       shift_Left  (key, 3);
      key := key   xor     shift_Right (key, 6);
      key := key   +   not shift_Left  (key, 11);
      key := key   xor     shift_Right (key, 16);

      return key;
   end getHash;


















   --------------------------------
   --- btSortedOverlappingPairCache
   --



   --- Forge
   --

   function  to_btSortedOverlappingPairCache return btSortedOverlappingPairCache
   is
      Self : btSortedOverlappingPairCache;
   begin
      Self.m_blockedForChanges  := False;
      Self.m_hasDeferredRemoval := True;
      Self.m_overlappingPairArray.reserve_Capacity (2);

      return Self;
   end to_btSortedOverlappingPairCache;






   overriding procedure destruct                (Self : in out btSortedOverlappingPairCache)
   is
   begin
      null;
   end destruct;





   --- Attributes
   --

   overriding function getOverlappingPairArrayPtr (Self : in     btSortedOverlappingPairCache) return btBroadphasePair_Vectors.Cursor
   is
   begin
      return Self.m_overlappingPairArray.First;
   end getOverlappingPairArrayPtr;




   overriding function getOverlappingPairArray    (Self : access btSortedOverlappingPairCache) return access btBroadphasePairArray
   is
   begin
      return Self.m_overlappingPairArray'Access;
   end getOverlappingPairArray;





   overriding procedure cleanOverlappingPair (Self : in out btSortedOverlappingPairCache;   pair       : access impact.d3.collision.Proxy.btBroadphasePair;
                                                                                 dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      pragma Unreferenced (Self);
   begin
      if pair.m_algorithm /= null then
         pair.m_algorithm.destruct;
         dispatcher.freeCollisionAlgorithm (pair.m_algorithm);
         pair.m_algorithm := null;

         gRemovePairs     := gRemovePairs - 1;
      end if;
   end cleanOverlappingPair;







   overriding function getNumOverlappingPairs     (Self : in btSortedOverlappingPairCache) return Integer
   is
   begin
      return Integer (Self.m_overlappingPairArray.Length);
   end getNumOverlappingPairs;






   overriding procedure cleanProxyFromPairs (Self : access btSortedOverlappingPairCache;   proxy      : access impact.d3.collision.Proxy.item'Class;
                                                                                dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      type CleanPairCallback is new btOverlapCallback with
         record
            m_cleanProxy : access impact.d3.collision.Proxy.item;
            m_pairCache  : access impact.d3.collision.overlapped_pair_Callback.cached.Item'Class;
            m_dispatcher : access impact.d3.Dispatcher.item;
         end record;

      overriding function  processOverlap (Self : in CleanPairCallback;   pair : access impact.d3.collision.Proxy.btBroadphasePair) return Boolean;



      function to_CleanPairCallback (cleanProxy : access impact.d3.collision.Proxy.item'Class;
                                     pairCache  : access impact.d3.collision.overlapped_pair_Callback.cached.item'Class;
                                     dispatcher : access impact.d3.Dispatcher.item'Class) return CleanPairCallback
      is
         Self : CleanPairCallback;
      begin
         Self.m_cleanProxy := cleanProxy;
         Self.m_pairCache  := pairCache;
         Self.m_dispatcher := dispatcher;

         return Self;
      end to_CleanPairCallback;



      overriding function  processOverlap (Self : in CleanPairCallback;   pair : access impact.d3.collision.Proxy.btBroadphasePair) return Boolean
      is
      begin
         if        pair.m_pProxy0 = Self.m_cleanProxy
           or else pair.m_pProxy1 = Self.m_cleanProxy
         then
            Self.m_pairCache.cleanOverlappingPair (pair, Self.m_dispatcher);
         end if;

         return False;
      end processOverlap;



      cleanPairs : aliased CleanPairCallback := to_CleanPairCallback (proxy, Self, dispatcher);

   begin
      Self.processAllOverlappingPairs (cleanPairs'Access, dispatcher);
   end cleanProxyFromPairs;








   overriding procedure processAllOverlappingPairs (Self : in out btSortedOverlappingPairCache;   callback   : access btOverlapCallback'Class;
                                                                                       dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      i    : Integer              := 1;
      pair : impact.d3.collision.Proxy.btBroadphasePair_view;
   begin
      while i <= Integer (Self.m_overlappingPairArray.Length)
      loop
         pair := Self.m_overlappingPairArray.Element (i);

         if callback.processOverlap (pair) then
            Self.cleanOverlappingPair (pair, dispatcher);

            pair.m_pProxy0 := null;
            pair.m_pProxy1 := null;

            Self.m_overlappingPairArray.swap (i, Integer (Self.m_overlappingPairArray.Length) - 0);
            Self.m_overlappingPairArray.delete_Last;

            gOverlappingPairs := gOverlappingPairs - 1;
         else
            i := i + 1;
         end if;
      end loop;
   end processAllOverlappingPairs;







   --  This findPair becomes really slow. Either sort the list to speedup the query, or
   --  use a different solution. It is mainly used for Removing overlapping pairs. Removal could be delayed.
   --  we could keep a linked list in each proxy, and store pair in one of the proxies (with lowest memory address)
   --  Also we can use a 2D bitmap, which can be useful for a future GPU implementation
   --
   overriding function findPair           (Self : in btSortedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item) return access impact.d3.collision.Proxy.btBroadphasePair
   is
   begin
      if not Self.needsBroadphaseCollision (proxy0, proxy1) then
         return null;
      end if;


      declare
         use btBroadphasePair_Vectors,  impact.d3.collision.Proxy;

         tmpPair : constant btBroadphasePair                := to_btBroadphasePair (proxy0, proxy1);
         Cursor  : btBroadphasePair_Vectors.Cursor := Self.m_overlappingPairArray.First;
      begin
         while has_Element (Cursor)
         loop
            if Element (Cursor).all = tmpPair then
               return Element (Cursor);
            end if;

            next (Cursor);
         end loop;
      end;


      return null;
   end findPair;







   overriding function hasDeferredRemoval (Self : in btSortedOverlappingPairCache) return Boolean
   is
   begin
      return Self.m_hasDeferredRemoval;
   end hasDeferredRemoval;






   overriding procedure setInternalGhostPairCallback (Self : in out btSortedOverlappingPairCache;   ghostPairCallback   : access impact.d3.collision.overlapped_pair_Callback.item'Class)
   is
   begin
      Self.m_ghostPairCallback := ghostPairCallback;
   end setInternalGhostPairCallback;






   overriding procedure sortOverlappingPairs (Self : in out btSortedOverlappingPairCache;   dispatcher : access impact.d3.Dispatcher.item'Class)
   is
   begin
      null;   -- Should already be sorted.
   end sortOverlappingPairs;





   function needsBroadphaseCollision (Self : in btSortedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return Boolean
   is
      use type impact.d3.collision.Proxy.CollisionFilterGroups;

      collides : Boolean;
   begin
      if Self.m_overlapFilterCallback /= null then
         return Self.m_overlapFilterCallback.needBroadphaseCollision (proxy0, proxy1);
      end if;

      collides :=                   (proxy0.m_collisionFilterGroup and proxy1.m_collisionFilterMask) /= 0;
      collides := collides and then (proxy1.m_collisionFilterGroup and proxy0.m_collisionFilterMask) /= 0;

      return collides;
   end needsBroadphaseCollision;






   overriding function  addOverlappingPair    (Self : access btSortedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class) return access impact.d3.collision.Proxy.btBroadphasePair
   is
   begin
      pragma Assert (proxy0 /= proxy1);    -- don't add overlap with own


      if not Self.needsBroadphaseCollision (proxy0, proxy1) then
         return null;
      end if;

      declare
         use impact.d3.collision.Proxy;

         pair   : constant btBroadphasePair_view := new btBroadphasePair'(to_btBroadphasePair (proxy0, proxy1));
         unused : access btBroadphasePair;
         pragma Unreferenced (unused);
      begin
         gOverlappingPairs := gOverlappingPairs + 1;
         gAddedPairs       := gAddedPairs       + 1;

         if Self.m_ghostPairCallback /= null then
            unused := Self.m_ghostPairCallback.addOverlappingPair (proxy0, proxy1);
        end if;

         Self.m_overlappingPairArray.Append (pair);

         return pair;
      end;
   end addOverlappingPair;







   type Any_View is access all Any'Class;


   overriding
   function  removeOverlappingPair (Self : access btSortedOverlappingPairCache;   proxy0, proxy1 : access impact.d3.collision.Proxy.item'Class;
                                                                                  dispatcher     : access impact.d3.Dispatcher.item'Class) return access Any'Class
   is
      use impact.d3.collision.Proxy;

      findPair  : btBroadphasePair_view;
      findIndex : Integer;

      userData  : Any_view;

   begin
      if not Self.hasDeferredRemoval then
         findPair  := new btBroadphasePair'(to_btBroadphasePair (proxy0, proxy1));
         findIndex := Self.m_overlappingPairArray.find_Index (findPair);

         if findIndex <= Integer (Self.m_overlappingPairArray.Length) then
            gOverlappingPairs := gOverlappingPairs - 1;

            declare
               pair     : constant btBroadphasePair_view := Self.m_overlappingPairArray.Element (findIndex);
               unused   : access  Any'Class;
               pragma Unreferenced (unused);
            begin
               userData := pair.internals.m_internalInfo1.all'Access;

               Self.cleanOverlappingPair (pair, dispatcher);

               if Self.m_ghostPairCallback /= null then
                  unused := Self.m_ghostPairCallback.removeOverlappingPair (proxy0, proxy1,  dispatcher);
               end if;

               Self.m_overlappingPairArray.replace_Element (findIndex, pair);
            end;
         end if;

         Self.m_overlappingPairArray.swap (findIndex,  Integer (Self.m_overlappingPairArray.capacity) - 0);
         Self.m_overlappingPairArray.delete_Last;

         return Any_view (userData);
      end if;


      return null;
   end removeOverlappingPair;










   overriding procedure removeOverlappingPairsContainingProxy (Self : access btSortedOverlappingPairCache;   proxy      : access impact.d3.collision.Proxy.item'Class;
                                                                                                  dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      type RemovePairCallback is new btOverlapCallback with
         record
            m_obsoleteProxy : access impact.d3.collision.Proxy.item;
         end record;

      overriding function  processOverlap (Self : in RemovePairCallback;   pair : access impact.d3.collision.Proxy.btBroadphasePair) return Boolean;



      function to_RemovePairCallback (obsoleteProxy : access impact.d3.collision.Proxy.item'Class) return RemovePairCallback
      is
         Self : RemovePairCallback;
      begin
         Self.m_obsoleteProxy := obsoleteProxy;

         return Self;
      end to_RemovePairCallback;



      overriding function  processOverlap (Self : in RemovePairCallback;   pair : access impact.d3.collision.Proxy.btBroadphasePair) return Boolean
      is
      begin
         return    pair.m_pProxy0 = Self.m_obsoleteProxy
           or else pair.m_pProxy1 = Self.m_obsoleteProxy;
      end processOverlap;



      removeCallback : aliased RemovePairCallback := to_RemovePairCallback (proxy);

   begin
      Self.processAllOverlappingPairs (removeCallback'Access, dispatcher);
   end removeOverlappingPairsContainingProxy;










   function  getOverlapFilterCallback (Self : access btSortedOverlappingPairCache) return access btOverlapFilterCallback'Class
   is
   begin
      return Self.m_overlapFilterCallback;
   end getOverlapFilterCallback;





   overriding procedure setOverlapFilterCallback (Self : in out btSortedOverlappingPairCache;   callback : access btOverlapFilterCallback'Class)
   is
   begin
      Self.m_overlapFilterCallback := callback;
   end setOverlapFilterCallback;



end impact.d3.collision.overlapped_pair_Callback.cached;
