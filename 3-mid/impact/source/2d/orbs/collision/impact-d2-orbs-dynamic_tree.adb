with ada.Unchecked_Deallocation,
     interfaces.c.Pointers;

--  for debug

package body impact.d2.orbs.dynamic_Tree
is
   use type int32;



   procedure free is new ada.Unchecked_Deallocation (b2DynamicTreeNodes, b2DynamicTreeNodes_view);





   function isLeaf (Self : in b2DynamicTreeNode) return Boolean
   is
   begin
      return Self.child1 = b2_nullNode;
   end isLeaf;





   function to_b2DynamicTree return b2DynamicTree
   is
      Self : b2DynamicTree;
   begin
      Self.m_root := b2_nullNode;

      Self.m_nodeCapacity := 16;
      Self.m_nodeCount := 0;

      Self.m_nodes := new b2DynamicTreeNodes (1 .. Self.m_nodeCapacity);

      --  Build a linked list for the free list.
      for i in 1 .. Self.m_nodeCapacity - 1 loop
         Self.m_nodes (i).next := i + 1;
      end loop;

      Self.m_nodes (Self.m_nodeCapacity - 1).next := b2_nullNode;
      Self.m_freeList                           := 1; -- 0;
      Self.m_path                               := 0;
      Self.m_insertionCount                     := 0;

      return Self;
   end to_b2DynamicTree;




   procedure destruct     (Self : in out b2DynamicTree)
   is
   begin
      free (Self.m_nodes);    -- This frees the entire tree in one shot.
   end destruct;





   --  Create a proxy in the tree as a leaf node. We return the index
   --  of the node instead of a pointer so that we can grow
   --  the node pool.
   --
   function  createProxy  (Self : access b2DynamicTree;   aabb     : in     collision.b2AABB;
                                                          userData : access Any'Class     ) return int32
   is
      proxyId : constant int32  := Self.AllocateNode;
      r       : constant b2Vec2 := (b2_aabbExtension, b2_aabbExtension);

      iterationCount : int32;
      tryCount       : int32;
      height         : int32;
   begin
      --  Fatten the aabb.
      Self.m_nodes (proxyId).aabb.lowerBound := aabb.lowerBound - r;
      Self.m_nodes (proxyId).aabb.upperBound := aabb.upperBound + r;
      Self.m_nodes (proxyId).userData        := userData;

      Self.InsertLeaf (proxyId);

      --  Rebalance if necessary.
      iterationCount := int32 (interfaces.shift_Right (uint32 (Self.m_nodeCount), 4));
      tryCount       := 0;
      height         := Self.ComputeHeight;

      while height > 64 and then tryCount < 10 loop
         Self.Rebalance (iterationCount);
         height   := Self.ComputeHeight;
         tryCount := tryCount + 1;
      end loop;

      return proxyId;
   end createProxy;










   procedure destroyProxy  (Self : in out b2DynamicTree;   proxyId : int32)
   is
   begin
      pragma Assert (1 <= proxyId and then proxyId <= Self.m_nodeCapacity);
      pragma Assert (isLeaf (Self.m_nodes (proxyId)));

      Self.removeLeaf (proxyId);
      Self.freeNode   (proxyId);
   end destroyProxy;





   function  MoveProxy     (Self : access b2DynamicTree;   proxyId      : in int32;
                            aabb         : in collision.b2AABB;
                            displacement : in b2Vec2) return Boolean
   is
      use impact.d2.orbs.Collision;
      b    : collision.b2AABB;
      d, r : b2Vec2;
   begin
      pragma Assert (1 <= proxyId and then proxyId <= Self.m_nodeCapacity);
      pragma Assert (isLeaf (Self.m_nodes (proxyId)));

      if Contains (Self.m_nodes (proxyId).aabb, aabb) then
         return False;
      end if;

      Self.RemoveLeaf (proxyId);

      --  Extend AABB.
      b            := aabb;
      r            := (b2_aabbExtension, b2_aabbExtension);
      b.lowerBound := b.lowerBound - r;
      b.upperBound := b.upperBound + r;

      --  Predict AABB displacement.
      d := b2_aabbMultiplier * displacement;

      if d.x < 0.0 then
         b.lowerBound.x := b.lowerBound.x + d.x;
      else
         b.upperBound.x := b.upperBound.x + d.x;
      end if;

      if d.y < 0.0 then
         b.lowerBound.y := b.lowerBound.y + d.y;
      else
         b.upperBound.y := b.upperBound.y + d.y;
      end if;

      Self.m_nodes (proxyId).aabb := b;

      Self.insertLeaf (proxyId);
      return True;
   end MoveProxy;





   type int32_array is array (uint32 range <>) of aliased int32;

   package int32_pointers is new interfaces.c.Pointers (uint32, int32, int32_array, int32'Last);
   subtype int32_view is int32_pointers.Pointer;


   procedure Rebalance     (Self : in out b2DynamicTree;   iterations : in int32)
   is
      use type uint32;

      node : int32;
      bit  : uint32;
   begin
      if Self.m_root = b2_nullNode then
         return;
      end if;

      for i in 1 .. iterations loop
         node := Self.m_root;

         bit := 0;

         while not isLeaf (Self.m_nodes (node)) loop
            declare
               use Interfaces, int32_pointers;

               children : int32_array renames Value (Self.m_nodes (node).child1'Access,
                                                     C.ptrdiff_t'(3));
            begin
--                 put_Line (uint32'Image (Children'first));
--                 put_Line (int32'image (int32 (shift_Right (Self.m_path, Integer (bit)) and 1) + 0));

               node := children (uint32 (shift_Right (Self.m_path, Integer (bit)) and 1) + 0);
               bit  := (bit + 1) and (uint32'Size - 1);
            end;
         end loop;

         Self.m_path := Self.m_path + 1;

         Self.RemoveLeaf (node);
         Self.InsertLeaf (node);
      end loop;

   end Rebalance;







   function  getUserData   (Self : in     b2DynamicTree;   proxyId : in int32) return access Any'Class
   is
   begin
      pragma Assert (1 <= proxyId and then proxyId <= Self.m_nodeCapacity);

      return Self.m_nodes (proxyId).userData;
   end getUserData;




   function  GetFatAABB    (Self : in     b2DynamicTree;   proxyId : int32) return collision.b2AABB
   is
   begin
      pragma Assert (1 <= proxyId and then proxyId <= Self.m_nodeCapacity);

      return Self.m_nodes (proxyId).aabb;
   end GetFatAABB;



   function  ComputeHeight (Self : in     b2DynamicTree) return int32
   is
   begin
      return Self.ComputeHeight (Self.m_root);
   end ComputeHeight;







   procedure Query (Self : in b2DynamicTree;   the_Callback : access callback_t;
                                               aabb         : in     collision.b2AABB)
   is
      k_stackSize : constant := 128;

      stack       : array (int32 range 1 .. k_stackSize) of int32;
      count       : int32 := 0;
      nodeId      : int32;
   begin
      count         := count + 1;
      stack (count) := Self.m_root;


      while count > 0 loop
         nodeId := stack (count);
         count  := count - 1;

         if nodeId /= b2_nullNode then
            declare
               node    : b2DynamicTreeNode renames Self.m_nodes (nodeId);
               proceed : Boolean;
            begin
               if collision.b2TestOverlap (node.aabb, aabb) then

                  if isLeaf (node) then
                     proceed := QueryCallback (the_Callback, nodeId);
                     if not proceed then
                        return;
                     end if;
                  else
                     if count < k_stackSize then
                        count         := count + 1;
                        stack (count) := node.child1;
                     end if;

                     if count < k_stackSize then
                        count         := count + 1;
                        stack (count) := node.child2;
                     end if;
                  end if;

               end if;
            end;
         end if;

      end loop;

   end Query;






   procedure Raycast (Self : in b2DynamicTree;   the_Callback : access callback_t;
                                                 input        : in     collision.b2RayCastInput)
   is
      p1    : constant b2Vec2 := input.p1;
      p2    : constant b2Vec2 := input.p2;
      r_pad : constant b2Vec2 := p2 - p1;   pragma Assert (LengthSquared (r_pad) > 0.0);
      r     : constant b2Vec2 := Normalize (r_pad);

      --  v is perpendicular to the segment.
      v     : constant b2Vec2 := b2Cross (1.0, r);
      abs_v : constant b2Vec2 := b2Abs (v);

      --  Separating axis for segment (Gino, p80).
      --  |dot(v, p1 - c)| > dot(|v|, h)
      maxFraction : float32 := input.maxFraction;

      segmentAABB : collision.b2AABB;
      t           : b2Vec2;

      k_stackSize : constant := 128;
      stack       : array (int32 range 1 .. k_stackSize) of int32;

      count       : int32  := 0;
      nodeId      : int32;
   begin
      --  Build a bounding box for the segment.
      t                      := p1 + maxFraction * (p2 - p1);
      segmentAABB.lowerBound := b2Min (p1, t);
      segmentAABB.upperBound := b2Max (p1, t);

      count         := count + 1;
      stack (count) := Self.m_root;

      while count > 0 loop
         nodeId := stack (count);
         count  := count - 1;

         if nodeId /= b2_nullNode then
            declare
               use impact.d2.orbs.Collision;
               node       : b2DynamicTreeNode renames Self.m_nodes (nodeId);

               c, h       : b2Vec2;
               separation : float32;
               subInput   : collision.b2RayCastInput;
               value      : float32;
            begin
               if collision.b2TestOverlap (node.aabb, segmentAABB) then

                  --  Separating axis for segment (Gino, p80).
                  --  |dot(v, p1 - c)| > dot(|v|, h)
                  c          := GetCenter  (node.aabb);
                  h          := GetExtents (node.aabb);
                  separation := abs (b2Dot (v, p1 - c))  -  b2Dot (abs_v, h);

                  if separation <= 0.0 then

                     if isLeaf (node) then

                        subInput.p1          := input.p1;
                        subInput.p2          := input.p2;
                        subInput.maxFraction := maxFraction;

                        value := RayCastCallback (the_Callback, subInput, nodeId);

                        if value = 0.0 then
                           return;   -- The client has terminated the ray cast.
                        end if;

                        if value > 0.0 then
                           --  Update segment bounding box.
                           maxFraction            := value;
                           t                      := p1 + maxFraction * (p2 - p1);
                           segmentAABB.lowerBound := b2Min (p1, t);
                           segmentAABB.upperBound := b2Max (p1, t);
                        end if;

                     else

                        if count < k_stackSize then
                           count         := count + 1;
                           stack (count) := node.child1;
                        end if;

                        if count < k_stackSize then
                           count         := count + 1;
                           stack (count) := node.child2;
                        end if;
                     end if;
                  end if;
               end if;
            end;
         end if;
      end loop;

   end Raycast;






   --  Allocate a node from the pool. Grow the pool if necessary.
   --
   function  AllocateNode (Self : access     b2DynamicTree) return int32
   is
      oldNodes : b2DynamicTreeNodes_view;
      nodeId   : int32;
   begin
      --  Expand the node pool as needed.
      if Self.m_freeList = b2_nullNode then
         pragma Assert (Self.m_nodeCount = Self.m_nodeCapacity);

         --  The free list is empty. Rebuild a bigger pool.
         oldNodes                      := Self.m_nodes;
         Self.m_nodeCapacity           := Self.m_nodeCapacity * 2;
         Self.m_nodes                  := new b2DynamicTreeNodes (1 .. Self.m_nodeCapacity);
         Self.m_nodes (oldNodes'Range) := oldNodes.all;
         free (oldNodes);

         --  Build a linked list for the free list. The parent pointer becomes the "next" pointer.
         for i in Self.m_nodeCount + 1 .. Self.m_nodeCapacity - 1 loop
            Self.m_nodes (i).next := i + 1;
         end loop;

         Self.m_nodes (Self.m_nodeCapacity).next := b2_nullNode;
         Self.m_freeList := Self.m_nodeCount + 1;
      end if;

      --  Peel a node off the free list.
      nodeId                       := Self.m_freeList;
      Self.m_freeList              := Self.m_nodes (nodeId).next;
      Self.m_nodes (nodeId).parent := b2_nullNode;
      Self.m_nodes (nodeId).child1 := b2_nullNode;
      Self.m_nodes (nodeId).child2 := b2_nullNode;

      Self.m_nodeCount := Self.m_nodeCount + 1;

      return nodeId;
   end AllocateNode;




   --  Return a node to the pool.
   --
   procedure FreeNode     (Self : in out b2DynamicTree;    nodeId : in int32)
   is
   begin
      pragma Assert (1 <= nodeId and then nodeId <= Self.m_nodeCapacity);
      pragma Assert (1 < Self.m_nodeCount);

      Self.m_nodes (nodeId).next := Self.m_freeList;
      Self.m_freeList            := nodeId;
      Self.m_nodeCount           := Self.m_nodeCount - 1;
   end FreeNode;





   procedure InsertLeaf   (Self : in out b2DynamicTree;    leafId : in int32)
   is
      use impact.d2.orbs.Collision;

      center  : b2Vec2;
      sibling : int32;

      node1,
      node2   : int32;
   begin
      Self.m_insertionCount := Self.m_insertionCount + 1;

      if Self.m_root = b2_nullNode then
         Self.m_root := leafId;
         Self.m_nodes (Self.m_root).parent := b2_nullNode;
         return;
      end if;

      --  Find the best sibling for this node.
      center  := GetCenter (Self.m_nodes (leafId).aabb);
      sibling := Self.m_root;

      if not isLeaf (Self.m_nodes (sibling)) then
         loop
            declare
               child1 : constant int32   := Self.m_nodes (sibling).child1;
               child2 : constant int32   := Self.m_nodes (sibling).child2;

               delta1 : b2Vec2  := b2Abs (GetCenter (Self.m_nodes (child1).aabb)  -  center);
               delta2 : b2Vec2  := b2Abs (GetCenter (Self.m_nodes (child2).aabb)  -  center);

               norm1  : constant float32 := delta1.x + delta1.y;
               norm2  : constant float32 := delta2.x + delta2.y;
            begin

               if norm1 < norm2 then
                  sibling := child1;
               else
                  sibling := child2;
               end if;
            end;

            exit when isLeaf (Self.m_nodes (sibling));
         end loop;
      end if;

      --  Create a parent for the siblings.
      node1                         := Self.m_nodes (sibling).parent;
      node2                         := Self.AllocateNode;
      Self.m_nodes (node2).parent   := node1;
      Self.m_nodes (node2).userData := null;
      combine (Self.m_nodes (node2).aabb,  Self.m_nodes (leafId).aabb,
                                           Self.m_nodes (sibling).aabb);

      if node1 /= b2_nullNode then
         if Self.m_nodes (Self.m_nodes (sibling).parent).child1 = sibling then
            Self.m_nodes (node1).child1 := node2;
         else
            Self.m_nodes (node1).child2 := node2;
         end if;

         Self.m_nodes (node2).child1   := sibling;
         Self.m_nodes (node2).child2   := leafId;
         Self.m_nodes (sibling).parent := node2;
         Self.m_nodes (leafId).parent  := node2;

         loop
            if Contains (Self.m_nodes (node1).aabb,  Self.m_nodes (node2).aabb) then
               exit;
            end if;

            Combine (Self.m_nodes (node1).aabb,  Self.m_nodes (Self.m_nodes (node1).child1).aabb,
                                                Self.m_nodes (Self.m_nodes (node1).child2).aabb);
            node2 := node1;
            node1 := Self.m_nodes (node1).parent;

            exit when node1 = b2_nullNode;
         end loop;

      else
         Self.m_nodes (node2).child1  := sibling;
         Self.m_nodes (node2).child2   := leafId;
         Self.m_nodes (sibling).parent := node2;
         Self.m_nodes (leafId).parent  := node2;
         Self.m_root                  := node2;
      end if;

   end InsertLeaf;






   procedure RemoveLeaf   (Self : in out b2DynamicTree;    leafId : in int32)
   is
      use impact.d2.orbs.Collision;

      node2   : int32;
      node1   : int32;
      sibling : int32;

      oldAABB : b2AABB;
   begin
      if leafId = Self.m_root then
         Self.m_root := b2_nullNode;
         return;
      end if;

      node2 := Self.m_nodes (leafId).parent;
      node1 := Self.m_nodes (node2).parent;

      if Self.m_nodes (node2).child1 = leafId then
         sibling := Self.m_nodes (node2).child2;
      else
         sibling := Self.m_nodes (node2).child1;
      end if;

      if node1 /= b2_nullNode then
         --  Destroy node2 and connect node1 to sibling.
         if Self.m_nodes (node1).child1 = node2 then
            Self.m_nodes (node1).child1 := sibling;
         else
            Self.m_nodes (node1).child2 := sibling;
         end if;

         Self.m_nodes (sibling).parent := node1;
         Self.FreeNode (node2);

         --  Adjust ancestor bounds.
         while node1 /= b2_nullNode loop
            oldAABB := Self.m_nodes (node1).aabb;
            Combine (Self.m_nodes (node1).aabb,  Self.m_nodes (Self.m_nodes (node1).child1).aabb,
                                                 Self.m_nodes (Self.m_nodes (node1).child2).aabb);

            if Contains (oldAABB,  Self.m_nodes (node1).aabb) then
               exit;
            end if;

            node1 := Self.m_nodes (node1).parent;
         end loop;
      else
         Self.m_root                   := sibling;
         Self.m_nodes (sibling).parent := b2_nullNode;
         Self.FreeNode (node2);
      end if;

   end RemoveLeaf;




   --  Compute the height of a sub-tree.
   --
   function  ComputeHeight (Self : in     b2DynamicTree;   nodeId : in int32) return int32
   is
   begin
      if nodeId = b2_nullNode then
         return 0;
      end if;

      pragma Assert (1 <= nodeId and then nodeId <= Self.m_nodeCapacity);

      declare
         node : b2DynamicTreeNode renames Self.m_nodes (nodeId);

         height1 : constant int32 := Self.computeHeight (node.child1);
         height2 : constant int32 := Self.computeHeight (node.child2);
      begin
         return 1 + int32'Max (height1, height2);
      end;
   end ComputeHeight;




end impact.d2.orbs.dynamic_Tree;
