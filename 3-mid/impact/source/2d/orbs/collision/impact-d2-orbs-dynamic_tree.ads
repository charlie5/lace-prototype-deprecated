with impact.d2.orbs.Collision,
     impact.d2.Math;



package  impact.d2.orbs.dynamic_Tree
--
--  A dynamic AABB tree broad-phase, inspired by Nathanael Presson's btDbvt.
--
is
   use impact.d2.Math;


   b2_nullNode : constant := 0;


   --  A node in the dynamic tree. The client does not interact with this directly.
   --
   type b2DynamicTreeNode_Kind is (with_Parent_index, with_Next_index);

   type b2DynamicTreeNode (Kind : b2DynamicTreeNode_Kind := with_parent_index) is
      record
         aabb     :         collision.b2AABB;    -- This is the fattened AABB.
         userData : access  Any'Class;

         child1   : aliased int32;
         child2   :         int32;

         case Kind is
            when with_Parent_index => parent : int32;
            when with_Next_index   => next   : int32;
         end case;

      end record;
   pragma Unchecked_Union (b2DynamicTreeNode);

   type b2DynamicTreeNodes is array (int32 range <>) of b2DynamicTreeNode;



   function isLeaf (Self : in b2DynamicTreeNode) return Boolean;






   --   A dynamic tree arranges data in a binary tree to accelerate
   --   queries such as volume queries and ray casts. Leafs are proxies
   --   with an AABB. In the tree we expand the proxy AABB by b2_fatAABBFactor
   --   so that the proxy AABB is bigger than the client object. This allows the client
   --   object to move by small amounts without triggering a tree update.
   --
   --   Nodes are pooled and relocatable, so we use node indices rather than pointers.
   --
   type b2DynamicTree is tagged private;



   function to_b2DynamicTree return b2DynamicTree;     -- Constructing the tree initializes the node pool.

   procedure destruct     (Self : in out b2DynamicTree);    -- Destroy the tree, freeing the node pool.



   --           Create a proxy. Provide a tight fitting AABB and a userData pointer.
   --
   function  createProxy  (Self : access b2DynamicTree;   aabb     : in     collision.b2AABB;
                                                          userData : access Any'Class) return int32;

   --  Destroy a proxy. This asserts if the id is invalid.
   --
   procedure destroyProxy  (Self : in out b2DynamicTree;   proxyId : int32);


   --           Move a proxy with a swepted AABB. If the proxy has moved outside of its fattened AABB,
   --           then the proxy is removed from the tree and re-inserted. Otherwise
   --           the function returns immediately.
   --           Return True if the proxy was re-inserted.
   --
   function  MoveProxy     (Self : access b2DynamicTree;   proxyId      : in int32;
                                                           aabb         : in collision.b2AABB;
                                                           displacement : in b2Vec2) return Boolean;

   --           Perform some iterations to re-balance the tree.
   --
   procedure Rebalance     (Self : in out b2DynamicTree;   iterations : in int32);


   --           Get proxy user data.
   --           Return the proxy user data or 0 if the id is invalid.
   --
   function  getUserData   (Self : in     b2DynamicTree;   proxyId : in int32) return access Any'Class;


   --           Get the fat AABB for a proxy.
   --
   function  GetFatAABB    (Self : in     b2DynamicTree;   proxyId : int32) return collision.b2AABB;


   --           Compute the height of the tree.
   --
   function  ComputeHeight (Self : in     b2DynamicTree) return int32;


   --           Query an AABB for overlapping proxies. The callback class
   --           is called for each proxy that overlaps the supplied AABB.
   --
   generic
      type callback_t is private;
      with function QueryCallback (the_Callback : access callback_t  ;
                                   nodeId       : in     int32       ) return Boolean;

   procedure Query (Self : in b2DynamicTree;   the_Callback : access callback_t;
                                               aabb         : in     collision.b2AABB);


   --          template <typename T>
   --          void Query(T* callback, const b2AABB& aabb) const;



   --           Ray-cast against the proxies in the tree. This relies on the callback
   --           to perform a exact ray-cast in the case were the proxy contains a shape.
   --           The callback also performs the any collision filtering. This has performance
   --           roughly equal to k * log(n), where k is the number of collisions and n is the
   --           number of proxies in the tree.
   --
   --           'input'    the ray-cast input data. The ray extends from p1 to p1 + maxFraction * (p2 - p1).
   --           'callback' a callback class that is called for each proxy that is hit by the ray.
   --
   generic
      type callback_t is private;
      with function RayCastCallback (the_Callback : access callback_t;
                                     Input        : in     collision.b2RayCastInput;
                                     nodeId       : in     int32       ) return float32;

   procedure Raycast (Self : in b2DynamicTree;   the_Callback : access callback_t;
                                                 input        : in     collision.b2RayCastInput);

--          template <typename T>
--          void RayCast(T* callback, const b2RayCastInput& input) const;
--



--     float32 value = callback->RayCastCallback(subInput, nodeId);


private

   type b2DynamicTreeNodes_view is access all b2DynamicTreeNodes;


   type b2DynamicTree is tagged
      record
         m_root           : int32;

         m_nodes          : b2DynamicTreeNodes_view;
         m_nodeCount      : int32;
         m_nodeCapacity   : int32;

         m_freeList       : int32;

         m_path           : uint32;        -- This is used incrementally traverse the tree for re-balancing.

         m_insertionCount : int32;
      end record;



   function  AllocateNode (Self : access b2DynamicTree) return int32;
   procedure FreeNode     (Self : in out b2DynamicTree;    nodeId : in int32);

   procedure InsertLeaf   (Self : in out b2DynamicTree;    leafId : in int32);
   procedure RemoveLeaf   (Self : in out b2DynamicTree;    leafId : in int32);

   function  ComputeHeight (Self : in     b2DynamicTree;   nodeId : in int32) return int32;


end impact.d2.orbs.dynamic_Tree;
