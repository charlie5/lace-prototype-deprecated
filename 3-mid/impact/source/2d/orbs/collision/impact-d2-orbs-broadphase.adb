with ada.unchecked_Deallocation,
     ada.Containers.generic_array_Sort;



package body impact.d2.orbs.Broadphase
is
   use type int32;


   procedure free is new ada.Unchecked_Deallocation (int32_array, int32_array_view);
   procedure free is new ada.Unchecked_Deallocation (b2Pairs,     b2Pairs_view);





   function  to_b2BroadPhase return b2BroadPhase
   is
      Self : b2BroadPhase;
   begin
      Self.m_proxyCount   := 0;

      Self.m_pairCapacity := 16;
      Self.m_pairCount    := 0;
      Self.m_pairBuffer   := new b2Pairs (1 .. Self.m_pairCapacity);

      Self.m_moveCapacity := 16;
      Self.m_moveCount    := 0;
      Self.m_moveBuffer   := new int32_array (1 .. Self.m_moveCapacity);

      return Self;
   end to_b2BroadPhase;





   procedure destruct     (Self : in out b2BroadPhase)
   is
   begin
      free (Self.m_moveBuffer);
      free (Self.m_pairBuffer);
   end destruct;






   function  CreateProxy  (Self : access b2BroadPhase;   aabb     : in     collision.b2AABB;
                                                         userData : access Any'Class) return int32
   is
      proxyId : constant int32 := Self.m_tree.createProxy (aabb, userData);
   begin
      Self.m_proxyCount := Self.m_proxyCount + 1;
      Self.BufferMove (proxyId);
      return proxyId;
   end CreateProxy;




   procedure DestroyProxy (Self : in out b2BroadPhase;   proxyId  : in     int32)
   is
   begin
      Self.UnBufferMove (proxyId);
      Self.m_proxyCount := Self.m_proxyCount - 1;
      Self.m_tree.destroyProxy (proxyId);
   end DestroyProxy;



   procedure MoveProxy (Self : in out b2BroadPhase;   proxyId      : in     int32;
                                                      aabb         : in     collision.b2AABB;
                                                      displacement : in     b2Vec2)
   is
      buffer : constant Boolean := Self.m_tree.moveProxy (proxyId, aabb, displacement);
   begin
      if buffer then
         Self.BufferMove (proxyId);
      end if;
   end MoveProxy;





   function  GetFatAABB (Self : in    b2BroadPhase;   proxyId      : in     int32) return collision.b2AABB
   is
   begin
      return Self.m_tree.getFatAABB (proxyId);
   end GetFatAABB;




   function  GetUserData (Self : in    b2BroadPhase;   proxyId      : in     int32) return access Any'Class
   is
   begin
      return Self.m_tree.getUserData (proxyId);
   end GetUserData;



   function  TestOverlap (Self : in    b2BroadPhase;   proxyIdA, proxyIdB : in     int32) return Boolean
   is
      aabbA : constant collision.b2AABB := Self.m_tree.getFatAABB (proxyIdA);
      aabbB : constant collision.b2AABB := Self.m_tree.getFatAABB (proxyIdB);
   begin
      return collision.b2TestOverlap (aabbA, aabbB);
   end TestOverlap;




   function  GetProxyCount (Self : in    b2BroadPhase) return int32
   is
   begin
      return Self.m_proxyCount;
   end GetProxyCount;







   function ComputeHeight (Self : in b2BroadPhase) return int32
   is
   begin
      return Self.m_tree.ComputeHeight;
   end ComputeHeight;







   procedure   BufferMove  (Self : in out b2BroadPhase;   proxyId : in int32)
   is
      oldBuffer : int32_array_view;
   begin
      if Self.m_moveCount = Self.m_moveCapacity then
         oldBuffer           := Self.m_moveBuffer;
         Self.m_moveCapacity := Self.m_moveCapacity * 2;
         Self.m_moveBuffer   := new int32_array (1 .. Self.m_moveCapacity);

         Self.m_moveBuffer (oldBuffer'Range) := oldBuffer.all;

         free (oldBuffer);
      end if;

      Self.m_moveCount                     := Self.m_moveCount + 1;
      Self.m_moveBuffer (Self.m_moveCount) := proxyId;
   end BufferMove;




   procedure unBufferMove  (Self : in out b2BroadPhase;   proxyId : in int32)
   is
   begin
      for i in 1 .. Self.m_moveCount loop
         if Self.m_moveBuffer (i) = proxyId then
            Self.m_moveBuffer (i) := e_nullProxy;
            return;
         end if;
      end loop;
   end unBufferMove;








   --  This is called from b2DynamicTree::Query when we are gathering pairs.
   --
   function  QueryCallback (Self : access     b2BroadPhase;   proxyId : in int32) return Boolean
   is
      oldBuffer : b2Pairs_view;
   begin
      --  A proxy cannot form a pair with itself.
      if proxyId = Self.m_queryProxyId then

         return True;
      end if;

      --  Grow the pair buffer as needed.
      if Self.m_pairCount = Self.m_pairCapacity then
         oldBuffer           := Self.m_pairBuffer;
         Self.m_pairCapacity := Self.m_pairCapacity * 2;
         Self.m_pairBuffer   := new b2Pairs (1 .. Self.m_pairCapacity);

         Self.m_pairBuffer (oldBuffer'Range) := oldBuffer.all;

         free (oldBuffer);
      end if;

      Self.m_pairCount := Self.m_pairCount + 1;

      Self.m_pairBuffer (Self.m_pairCount).proxyIdA := int32'Min (proxyId, Self.m_queryProxyId);
      Self.m_pairBuffer (Self.m_pairCount).proxyIdB := int32'Max (proxyId, Self.m_queryProxyId);


      return True;
   end QueryCallback;






   --  This is used to sort pairs.
   --
   function b2PairLessThan (pair1, pair2 : in b2Pair) return Boolean
   is
   begin
      if pair1.proxyIdA < pair2.proxyIdA then
         return True;
      end if;

      if pair1.proxyIdA = pair2.proxyIdA then
         return pair1.proxyIdB < pair2.proxyIdB;
      end if;

      return False;
   end b2PairLessThan;





   procedure UpdatePairs (Self : in out b2BroadPhase;   the_Callback : access callback_t)
   is
      userDataA,
      userDataB : access Any'Class;

      procedure sort is new ada.Containers.Generic_Array_Sort (int32, b2Pair,
                                                               b2Pairs,
                                                               b2PairLessThan);
      i : int32;
   begin
      --  Reset pair buffer
      Self.m_pairCount := 0;

      --  Perform tree queries for all moving proxies.
      for i in 1 .. Self.m_moveCount loop
         Self.m_queryProxyId := Self.m_moveBuffer (i);

         if Self.m_queryProxyId /= e_nullProxy then
            declare
               --  We have to query the tree with the fat AABB so that
               --  we don't fail to create a pair that may touch later.
               fatAABB : collision.b2AABB renames Self.m_tree.GetFatAABB (Self.m_queryProxyId);

               procedure Query is new dynamic_tree.Query (b2BroadPhase, QueryCallback);
            begin
               --  Query tree, create pairs and add them pair buffer.
               Query (Self.m_tree,  Self'Access, fatAABB);
            end;
         end if;
      end loop;

      --  Reset move buffer
      Self.m_moveCount := 0;

      --  Sort the pair buffer to expose duplicates.
      sort (Self.m_pairBuffer (1 .. Self.m_pairCount));

      --  Send the pairs back to the client.
      i := 1;

      while i <= Self.m_pairCount loop
         declare
            primaryPair : constant access b2Pair := Self.m_pairBuffer (i)'Access;
            pair        : access b2Pair;
         begin
            userDataA := Self.m_tree.GetUserData (primaryPair.proxyIdA);
            userDataB := Self.m_tree.GetUserData (primaryPair.proxyIdB);

            addPair (the_Callback, userDataA, userDataB);
            i := i + 1;

            --  Skip any duplicate pairs.
            while i <= Self.m_pairCount loop
               pair := Self.m_pairBuffer (i)'Access;

               exit when pair.proxyIdA /= primaryPair.proxyIdA
                 or else pair.proxyIdB /= primaryPair.proxyIdB;

               i := i + 1;
            end loop;
         end;
      end loop;

      --  Try to keep the tree balanced.
      Self.m_tree.Rebalance (4);

   end UpdatePairs;






   procedure Query (Self : in b2BroadPhase;   the_Callback : access callback_t;
                                              aabb         : in     collision.b2AABB)
   is
      procedure Query is new dynamic_tree.Query (callback_t, QueryCallback);
   begin
      Query (Self.m_tree, the_Callback, aabb);
   end Query;





   procedure RayCast (Self : in b2BroadPhase;   the_Callback : access callback_t;
                                                input        : in     collision.b2RayCastInput)
   is
      procedure RayCast is new dynamic_tree.Raycast (callback_t, RayCastCallback);
   begin
      RayCast (Self.m_tree,  the_Callback, input);
   end RayCast;


end impact.d2.orbs.Broadphase;
