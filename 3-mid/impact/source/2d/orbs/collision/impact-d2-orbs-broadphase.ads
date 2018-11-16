with impact.d2.orbs.Collision,
     impact.d2.Math;

private with impact.d2.orbs.dynamic_Tree;



package impact.d2.orbs.Broadphase
--
--  The broad-phase is used for computing pairs and performing volume queries and ray casts.
--  This broad-phase does not persist pairs. Instead, this reports potentially new pairs.
--  It is up to the client to consume the new pairs and to track subsequent overlap.
--
is
   use impact.d2.Math;




   type b2Pair is
      record
         proxyIdA,
         proxyIdB,
         next    : int32;
      end record;

   type b2Pairs is array (int32 range <>) of aliased b2Pair;



   e_nullProxy : constant := -1;




   type b2BroadPhase is tagged private;


   function  to_b2BroadPhase return b2BroadPhase;

   procedure destruct     (Self : in out b2BroadPhase);


   --          Create a proxy with an initial AABB. Pairs are not reported until
   --          UpdatePairs is called.
   --
   function  CreateProxy  (Self : access b2BroadPhase;   aabb     : in     collision.b2AABB;
                                                         userData : access Any'Class) return int32;

   --  Destroy a proxy. It is up to the client to remove any pairs.
   --
   procedure DestroyProxy (Self : in out b2BroadPhase;   proxyId  : in     int32);


   --  Call MoveProxy as many times as you like, then when you are done
   --  call UpdatePairs to finalized the proxy pairs (for your time step).
   --
   procedure MoveProxy (Self : in out b2BroadPhase;   proxyId      : in     int32;
                                                      aabb         : in     collision.b2AABB;
                                                      displacement : in b2Vec2);

   --  Get the fat AABB for a proxy.
   --
   function  GetFatAABB (Self : in    b2BroadPhase;   proxyId      : in     int32) return collision.b2AABB;


   --  Get user data from a proxy. Returns NULL if the id is invalid.
   --
   function  GetUserData (Self : in    b2BroadPhase;   proxyId      : in     int32) return access Any'Class;




   --  Test overlap of fat AABBs.
   --
   function  TestOverlap (Self : in    b2BroadPhase;   proxyIdA, proxyIdB : in     int32) return Boolean;



   --  Get the number of proxies.
   --
   function  GetProxyCount (Self : in    b2BroadPhase) return int32;



   --  Update the pairs. This results in pair callbacks. This can only add pairs.
   --

   generic
      type callback_t is private;

      with procedure addPair (the_Callback : access callback_t;
                              userDataA,
                              userDataB    : access Any'Class);

   procedure UpdatePairs (Self : in out b2BroadPhase;   the_Callback : access callback_t);

--          template <typename T>
--          void UpdatePairs(T* callback);

--                 addPair (callback, userDataA, userDataB);


   --  Query an AABB for overlapping proxies. The callback class
   --  is called for each proxy that overlaps the supplied AABB.
   --
   generic
      type callback_t is private;
      with function QueryCallback (the_Callback : access callback_t  ;
                                   nodeId       : in     int32       ) return Boolean;

   procedure Query (Self : in b2BroadPhase;   the_Callback : access callback_t;
                                              aabb         : in     collision.b2AABB);

--          template <typename T>
--          void Query(T* callback, const b2AABB& aabb) const;




   --  Ray-cast against the proxies in the tree. This relies on the callback
   --  to perform a exact ray-cast in the case were the proxy contains a shape.
   --  The callback also performs the any collision filtering. This has performance
   --  roughly equal to k * log(n), where k is the number of collisions and n is the
   --  number of proxies in the tree.
   --  'input'    the ray-cast input data. The ray extends from p1 to p1 + maxFraction * (p2 - p1).
   --  'callback' a callback class that is called for each proxy that is hit by the ray.
   --
   generic
      type callback_t is private;

      with function RayCastCallback (the_Callback : access callback_t;
                                     Input        : in     collision.b2RayCastInput;
                                     nodeId       : in     int32       ) return float32;

   procedure RayCast (Self : in b2BroadPhase;   the_Callback : access callback_t;
                                                 input        : in     collision.b2RayCastInput);

--          template <typename T>
--          void RayCast(T* callback, const b2RayCastInput& input) const;



   --  Compute the height of the embedded tree.
   --
   function ComputeHeight (Self : in b2BroadPhase) return int32;






private

   type b2Pairs_view     is access all b2Pairs;
   type int32_array_view is access all int32_array;


   type b2BroadPhase is tagged
      record
         m_tree         : aliased dynamic_tree.b2DynamicTree := dynamic_tree.to_b2DynamicTree;

         m_proxyCount   : int32;

         m_moveBuffer   : int32_array_view;
         m_moveCapacity : int32;
         m_moveCount    : int32;

         m_pairBuffer   : b2Pairs_view;
         m_pairCapacity : int32;
         m_pairCount    : int32;

         m_queryProxyId : int32;
      end record;


   procedure   BufferMove  (Self : in out b2BroadPhase;   proxyId : in int32);
   procedure unBufferMove  (Self : in out b2BroadPhase;   proxyId : in int32);

   function  QueryCallback (Self : access b2BroadPhase;   proxyId : in int32) return Boolean;

end impact.d2.orbs.Broadphase;
