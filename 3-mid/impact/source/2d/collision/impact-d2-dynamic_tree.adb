with
     ada.Containers.Vectors,
     ada.Unchecked_Deallocation,
     interfaces.c.Pointers;

--  for debug

package body impact.d2.dynamic_Tree
is
   use type int32;


   -------
   -- Node
   --
   procedure free is new ada.Unchecked_Deallocation (b2DynamicTreeNodes,
                                                     b2DynamicTreeNodes_view);

   function isLeaf (Self : in b2DynamicTreeNode) return Boolean
   is
   begin
      return Self.child1 = b2_nullNode;
   end isLeaf;



   -------
   -- Tree
   --

   function to_b2DynamicTree return b2DynamicTree
   is
      Self : b2DynamicTree;
   begin
      Self.m_root := b2_nullNode;

      Self.m_nodeCapacity := 16;
      Self.m_nodeCount    := 0;

      Self.m_nodes := new b2DynamicTreeNodes (0 .. Self.m_nodeCapacity - 1);

      --  Build a linked list for the free list.
      for i in 0 .. Self.m_nodeCapacity - 2
      loop
         Self.m_nodes (i).next   := i + 1;
         Self.m_nodes (i).height := -1;
      end loop;

      Self.m_nodes (Self.m_nodeCapacity - 1).next   := b2_nullNode;
      Self.m_nodes (Self.m_nodeCapacity - 1).height := -1;

      Self.m_freeList       := 0;
      Self.m_path           := 0;
      Self.m_insertionCount := 0;

      return Self;
   end to_b2DynamicTree;



   procedure destruct (Self : in out b2DynamicTree)
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
   begin
      --  Fatten the aabb.
      Self.m_nodes (proxyId).aabb.lowerBound := aabb.lowerBound - r;
      Self.m_nodes (proxyId).aabb.upperBound := aabb.upperBound + r;
      Self.m_nodes (proxyId).userData        := userData;
      Self.m_nodes (proxyId).height          := 0;

      Self.InsertLeaf (proxyId);

      return proxyId;
   end createProxy;


--  int32 b2DynamicTree::CreateProxy(const b2AABB& aabb, void* userData)
--  {
--      int32 proxyId = AllocateNode();
--
--      // Fatten the aabb.
--      b2Vec2 r(b2_aabbExtension, b2_aabbExtension);
--      m_nodes[proxyId].aabb.lowerBound = aabb.lowerBound - r;
--      m_nodes[proxyId].aabb.upperBound = aabb.upperBound + r;
--      m_nodes[proxyId].userData = userData;
--      m_nodes[proxyId].height = 0;
--
--      InsertLeaf(proxyId);
--
--      return proxyId;
--  }



   procedure destroyProxy  (Self : in out b2DynamicTree;   proxyId : int32)
   is
   begin
      pragma Assert (0 <= proxyId and then proxyId < Self.m_nodeCapacity);
      pragma Assert (isLeaf (Self.m_nodes (proxyId)));

      Self.removeLeaf (proxyId);
      Self.freeNode   (proxyId);
   end destroyProxy;


--  void b2DynamicTree::DestroyProxy(int32 proxyId)
--  {
--      b2Assert(0 <= proxyId && proxyId < m_nodeCapacity);
--      b2Assert(m_nodes[proxyId].IsLeaf());
--
--      RemoveLeaf(proxyId);
--      FreeNode(proxyId);
--  }



   function  MoveProxy     (Self : access b2DynamicTree;   proxyId      : in int32;
                            aabb         : in collision.b2AABB;
                            displacement : in b2Vec2) return Boolean
   is
      use impact.d2.Collision;
      b    : collision.b2AABB;
      d, r : b2Vec2;
   begin
      pragma Assert (0 <= proxyId and then proxyId < Self.m_nodeCapacity);
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


--  bool b2DynamicTree::MoveProxy(int32 proxyId, const b2AABB& aabb, const b2Vec2& displacement)
--  {
--      b2Assert(0 <= proxyId && proxyId < m_nodeCapacity);
--
--      b2Assert(m_nodes[proxyId].IsLeaf());
--
--      if (m_nodes[proxyId].aabb.Contains(aabb))
--      {
--              return false;
--      }
--
--      RemoveLeaf(proxyId);
--
--      // Extend AABB.
--      b2AABB b = aabb;
--      b2Vec2 r(b2_aabbExtension, b2_aabbExtension);
--      b.lowerBound = b.lowerBound - r;
--      b.upperBound = b.upperBound + r;
--
--      // Predict AABB displacement.
--      b2Vec2 d = b2_aabbMultiplier * displacement;
--
--      if (d.x < 0.0f)
--      {
--              b.lowerBound.x += d.x;
--      }
--      else
--      {
--              b.upperBound.x += d.x;
--      }
--
--      if (d.y < 0.0f)
--      {
--              b.lowerBound.y += d.y;
--      }
--      else
--      {
--              b.upperBound.y += d.y;
--      }
--
--      m_nodes[proxyId].aabb = b;
--
--      InsertLeaf(proxyId);
--      return true;
--  }




   -- Perform a left or right rotation if node A is imbalanced.
   -- Returns the new root index.

   function Balance (Self : in out b2DynamicTree;   iA : in int32) return int32
   is
      pragma assert (iA /= b2_nullNode);

      A      : b2DynamicTreeNode renames Self.m_nodes (iA);
      iB, iC : int32;

   begin
      if   IsLeaf (A)
        or A.height < 2
      then
         return iA;
      end if;

      iB := A.child1;   pragma assert (0 <= iB and iB < Self.m_nodeCapacity);
      iC := A.child2;   pragma assert (0 <= iC and iC < Self.m_nodeCapacity);

      declare
         B       : b2DynamicTreeNode renames Self.m_nodes (iB);
         C       : b2DynamicTreeNode renames Self.m_nodes (iC);

         balance : constant int32 := C.height - B.height;
      begin

         -- Rotate C up
         if balance > 1
         then
            declare
               i_F : constant int32 := C.child1;   pragma assert (0 <= i_F and i_F < Self.m_nodeCapacity);
               iG  : constant int32 := C.child2;   pragma assert (0 <= iG  and iG  < Self.m_nodeCapacity);

               F   : b2DynamicTreeNode renames Self.m_nodes (i_F);
               G   : b2DynamicTreeNode renames Self.m_nodes (iG);

            begin
               -- Swap A and C
               C.child1 := iA;
               C.parent := A.parent;
               A.parent := iC;

               -- A's old parent should point to C
               if C.parent /= b2_nullNode
               then
                  if Self.m_nodes (C.parent).child1 = iA then
                     Self.m_nodes (C.parent).child1 := iC;
                  else
                     pragma assert (Self.m_nodes (C.parent).child2 = iA);
                     Self.m_nodes (C.parent).child2 := iC;
                  end if;
               else
                  Self.m_root := iC;
               end if;

               -- Rotate
               if F.height > G.height
               then
                  C.child2 := i_F;
                  A.child2 := iG;
                  G.parent := iA;
                  A.aabb.Combine (B.aabb, G.aabb);
                  C.aabb.Combine (A.aabb, F.aabb);

                  A.height := 1 + int32'Max (B.height, G.height);
                  C.height := 1 + int32'Max (A.height, F.height);
               else
                  C.child2 := iG;
                  A.child2 := i_F;
                  F.parent := iA;
                  A.aabb.Combine(B.aabb, F.aabb);
                  C.aabb.Combine(A.aabb, G.aabb);

                  A.height := 1 + int32'Max (B.height, F.height);
                  C.height := 1 + int32'Max (A.height, G.height);
               end if;

               return iC;
            end;
         end if;

         -- Rotate B up
         if balance < -1
         then
            declare
               iD : constant int32 := B.child1;
               iE : constant int32 := B.child2;
               D  : b2DynamicTreeNode renames Self.m_nodes (iD);
               E  : b2DynamicTreeNode renames Self.m_nodes (iE);
               pragma assert (0 <= iD and iD < Self.m_nodeCapacity);
               pragma assert (0 <= iE and iE < Self.m_nodeCapacity);
            begin
               -- Swap A and B
               B.child1 := iA;
               B.parent := A.parent;
               A.parent := iB;

               -- A's old parent should point to B
               if B.parent /= b2_nullNode
               then
                  if Self.m_nodes (B.parent).child1 = iA then
                     Self.m_nodes (B.parent).child1 := iB;
                  else
                     pragma assert (Self.m_nodes (B.parent).child2 = iA);
                     Self.m_nodes (B.parent).child2 := iB;
                  end if;
               else
                  Self.m_root := iB;
               end if;

               -- Rotate
               if D.height > E.height
               then
                  B.child2 := iD;
                  A.child1 := iE;
                  E.parent := iA;
                  A.aabb.Combine (C.aabb, E.aabb);
                  B.aabb.Combine (A.aabb, D.aabb);

                  A.height := 1 + int32'Max (C.height, E.height);
                  B.height := 1 + int32'Max (A.height, D.height);
               else
                  B.child2 := iE;
                  A.child1 := iD;
                  D.parent := iA;
                  A.aabb.Combine (C.aabb, D.aabb);
                  B.aabb.Combine (A.aabb, E.aabb);

                  A.height := 1 + int32'Max (C.height, D.height);
                  B.height := 1 + int32'Max (A.height, E.height);
               end if;

               return iB;
            end;
         end if;
      end;

      return iA;
   end Balance;



--  // Perform a left or right rotation if node A is imbalanced.
--  // Returns the new root index.
--  int32 b2DynamicTree::Balance(int32 iA)
--  {
--      b2Assert(iA != b2_nullNode);
--
--      b2TreeNode* A = m_nodes + iA;
--      if (A->IsLeaf() || A->height < 2)
--      {
--              return iA;
--      }
--
--      int32 iB = A->child1;
--      int32 iC = A->child2;
--      b2Assert(0 <= iB && iB < m_nodeCapacity);
--      b2Assert(0 <= iC && iC < m_nodeCapacity);
--
--      b2TreeNode* B = m_nodes + iB;
--      b2TreeNode* C = m_nodes + iC;
--
--      int32 balance = C->height - B->height;
--
--      // Rotate C up
--      if (balance > 1)
--      {
--              int32 iF = C->child1;
--              int32 iG = C->child2;
--              b2TreeNode* F = m_nodes + iF;
--              b2TreeNode* G = m_nodes + iG;
--              b2Assert(0 <= iF && iF < m_nodeCapacity);
--              b2Assert(0 <= iG && iG < m_nodeCapacity);
--
--              // Swap A and C
--              C->child1 = iA;
--              C->parent = A->parent;
--              A->parent = iC;
--
--              // A's old parent should point to C
--              if (C->parent != b2_nullNode)
--              {
--                      if (m_nodes[C->parent].child1 == iA)
--                      {
--                              m_nodes[C->parent].child1 = iC;
--                      }
--                      else
--                      {
--                              b2Assert(m_nodes[C->parent].child2 == iA);
--                              m_nodes[C->parent].child2 = iC;
--                      }
--              }
--              else
--              {
--                      m_root = iC;
--              }
--
--              // Rotate
--              if (F->height > G->height)
--              {
--                      C->child2 = iF;
--                      A->child2 = iG;
--                      G->parent = iA;
--                      A->aabb.Combine(B->aabb, G->aabb);
--                      C->aabb.Combine(A->aabb, F->aabb);
--
--                      A->height = 1 + b2Max(B->height, G->height);
--                      C->height = 1 + b2Max(A->height, F->height);
--              }
--              else
--              {
--                      C->child2 = iG;
--                      A->child2 = iF;
--                      F->parent = iA;
--                      A->aabb.Combine(B->aabb, F->aabb);
--                      C->aabb.Combine(A->aabb, G->aabb);
--
--                      A->height = 1 + b2Max(B->height, F->height);
--                      C->height = 1 + b2Max(A->height, G->height);
--              }
--
--              return iC;
--      }
--
--      // Rotate B up
--      if (balance < -1)
--      {
--              int32 iD = B->child1;
--              int32 iE = B->child2;
--              b2TreeNode* D = m_nodes + iD;
--              b2TreeNode* E = m_nodes + iE;
--              b2Assert(0 <= iD && iD < m_nodeCapacity);
--              b2Assert(0 <= iE && iE < m_nodeCapacity);
--
--              // Swap A and B
--              B->child1 = iA;
--              B->parent = A->parent;
--              A->parent = iB;
--
--              // A's old parent should point to B
--              if (B->parent != b2_nullNode)
--              {
--                      if (m_nodes[B->parent].child1 == iA)
--                      {
--                              m_nodes[B->parent].child1 = iB;
--                      }
--                      else
--                      {
--                              b2Assert(m_nodes[B->parent].child2 == iA);
--                              m_nodes[B->parent].child2 = iB;
--                      }
--              }
--              else
--              {
--                      m_root = iB;
--              }
--
--              // Rotate
--              if (D->height > E->height)
--              {
--                      B->child2 = iD;
--                      A->child1 = iE;
--                      E->parent = iA;
--                      A->aabb.Combine(C->aabb, E->aabb);
--                      B->aabb.Combine(A->aabb, D->aabb);
--
--                      A->height = 1 + b2Max(C->height, E->height);
--                      B->height = 1 + b2Max(A->height, D->height);
--              }
--              else
--              {
--                      B->child2 = iE;
--                      A->child1 = iD;
--                      D->parent = iA;
--                      A->aabb.Combine(C->aabb, D->aabb);
--                      B->aabb.Combine(A->aabb, E->aabb);
--
--                      A->height = 1 + b2Max(C->height, D->height);
--                      B->height = 1 + b2Max(A->height, E->height);
--              }
--
--              return iB;
--      }
--
--      return iA;
--  }






   function  getUserData   (Self : in     b2DynamicTree;   proxyId : in int32) return access Any'Class
   is
      pragma Assert (0 <= proxyId and then proxyId < Self.m_nodeCapacity);
   begin
      return Self.m_nodes (proxyId).userData;
   end getUserData;




   function  GetFatAABB    (Self : in     b2DynamicTree;   proxyId : int32) return collision.b2AABB
   is
      pragma Assert (0 <= proxyId and then proxyId < Self.m_nodeCapacity);
   begin
      return Self.m_nodes (proxyId).aabb;
   end GetFatAABB;



   package int32_Vectors is new ada.Containers.Vectors (Positive, int32);
   subtype int32_Stack   is int32_Vectors.Vector;



   procedure Query (Self : in b2DynamicTree;   the_Callback : access callback_t;
                                               aabb         : in     collision.b2AABB)
   is
      use ada.Containers,
          int32_Vectors;

      Stack  : int32_Stack;
      nodeId : int32;

   begin
      Stack.reserve_Capacity (256);
      Stack.append (Self.m_root);

      while Stack.Length > 0
      loop
         nodeId := Stack.Last_Element;
         Stack.delete_Last;

         if nodeId /= b2_nullNode then
            declare
               node    : b2DynamicTreeNode renames Self.m_nodes (nodeId);
               proceed : Boolean;
            begin
               if collision.b2TestOverlap (node.aabb, aabb)
               then
                  if isLeaf (node) then
                     proceed := QueryCallback (the_Callback, nodeId);
                     if not proceed then
                        return;
                     end if;
                  else
                     Stack.append (node.child1);
                     Stack.append (node.child2);
                  end if;
               end if;
            end;
         end if;

      end loop;
   end Query;




   procedure Raycast (Self : in b2DynamicTree;   the_Callback : access callback_t;
                                                 input        : in     collision.b2RayCastInput)
   is
      use ada.Containers,
          int32_Vectors;

      p1    : constant b2Vec2 := input.p1;
      p2    : constant b2Vec2 := input.p2;
      r_pad : constant b2Vec2 := p2 - p1;   pragma Assert (LengthSquared (r_pad) > 0.0);
      r     : constant b2Vec2 := Normalize (r_pad);

      --  v is perpendicular to the segment.
      --
      v     : constant b2Vec2 := b2Cross (1.0, r);
      abs_v : constant b2Vec2 := b2Abs (v);

      --  Separating axis for segment (Gino, p80).
      --  |dot(v, p1 - c)| > dot(|v|, h)
      --
      maxFraction : float32 := input.maxFraction;

      segmentAABB : collision.b2AABB;
      t           : b2Vec2;

      Stack       : int32_Stack;

      count       : int32  := 0;
      nodeId      : int32;
   begin
      --  Build a bounding box for the segment.
      --
      t                      := p1 + maxFraction * (p2 - p1);
      segmentAABB.lowerBound := b2Min (p1, t);
      segmentAABB.upperBound := b2Max (p1, t);

      Stack.reserve_Capacity (256);
      Stack.append (Self.m_root);

      while Stack.Length > 0
      loop
         nodeId := Stack.last_Element;
         Stack.delete_Last;

         if nodeId /= b2_nullNode then
            declare
               use impact.d2.Collision;
               node       : b2DynamicTreeNode renames Self.m_nodes (nodeId);

               c, h       : b2Vec2;
               separation : float32;
               subInput   : collision.b2RayCastInput;
               value      : float32;
            begin
               if collision.b2TestOverlap (node.aabb, segmentAABB) then

                  --  Separating axis for segment (Gino, p80).
                  --  |dot(v, p1 - c)| > dot(|v|, h)
                  --
                  c          := GetCenter  (node.aabb);
                  h          := GetExtents (node.aabb);
                  separation := abs (b2Dot (v, p1 - c))  -  b2Dot (abs_v, h);

                  if separation <= 0.0 then

                     if isLeaf (node)
                     then
                        subInput.p1          := input.p1;
                        subInput.p2          := input.p2;
                        subInput.maxFraction := maxFraction;

                        value := RayCastCallback (the_Callback, subInput, nodeId);

                        if value = 0.0 then
                           return;   -- The client has terminated the ray cast.
                        end if;

                        if value > 0.0 then
                           --  Update segment bounding box.
                           --
                           maxFraction            := value;
                           t                      := p1 + maxFraction * (p2 - p1);
                           segmentAABB.lowerBound := b2Min (p1, t);
                           segmentAABB.upperBound := b2Max (p1, t);
                        end if;

                     else
                        Stack.append (node.child1);
                        Stack.append (node.child2);
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
      if Self.m_freeList = b2_nullNode
      then
         pragma Assert (Self.m_nodeCount = Self.m_nodeCapacity);

         --  The free list is empty. Rebuild a bigger pool.
         oldNodes                      := Self.m_nodes;
         Self.m_nodeCapacity           := Self.m_nodeCapacity * 2;
         Self.m_nodes                  := new b2DynamicTreeNodes (0 .. Self.m_nodeCapacity - 1);
         Self.m_nodes (oldNodes'Range) := oldNodes.all;
         free (oldNodes);

         --  Build a linked list for the free list. The parent pointer becomes the "next" pointer.
         for i in Self.m_nodeCount .. Self.m_nodeCapacity - 2
         loop
            Self.m_nodes (i).next   := i + 1;
            Self.m_nodes (i).height := -1;
         end loop;

         Self.m_nodes (Self.m_nodeCapacity - 1).next   := b2_nullNode;
         Self.m_nodes (Self.m_nodeCapacity - 1).height := -1;
         Self.m_freeList                               := Self.m_nodeCount;
      end if;

      --  Peel a node off the free list.
      nodeId                         := Self.m_freeList;
      Self.m_freeList                := Self.m_nodes (nodeId).next;
      Self.m_nodes (nodeId).parent   := b2_nullNode;
      Self.m_nodes (nodeId).child1   := b2_nullNode;
      Self.m_nodes (nodeId).child2   := b2_nullNode;
      Self.m_nodes (nodeId).height   := 0;
      Self.m_nodes (nodeId).userData := null;

      Self.m_nodeCount := Self.m_nodeCount + 1;

      return nodeId;
   end AllocateNode;


--  // Allocate a node from the pool. Grow the pool if necessary.
--  int32 b2DynamicTree::AllocateNode()
--  {
--      // Expand the node pool as needed.
--      if (m_freeList == b2_nullNode)
--      {
--              b2Assert(m_nodeCount == m_nodeCapacity);
--
--              // The free list is empty. Rebuild a bigger pool.
--              b2TreeNode* oldNodes = m_nodes;
--              m_nodeCapacity *= 2;
--              m_nodes = (b2TreeNode*)b2Alloc(m_nodeCapacity * sizeof(b2TreeNode));
--              memcpy(m_nodes, oldNodes, m_nodeCount * sizeof(b2TreeNode));
--              b2Free(oldNodes);
--
--              // Build a linked list for the free list. The parent
--              // pointer becomes the "next" pointer.
--              for (int32 i = m_nodeCount; i < m_nodeCapacity - 1; ++i)
--              {
--                      m_nodes[i].next = i + 1;
--                      m_nodes[i].height = -1;
--              }
--              m_nodes[m_nodeCapacity-1].next = b2_nullNode;
--              m_nodes[m_nodeCapacity-1].height = -1;
--              m_freeList = m_nodeCount;
--      }
--
--      // Peel a node off the free list.
--      int32 nodeId = m_freeList;
--      m_freeList = m_nodes[nodeId].next;
--      m_nodes[nodeId].parent = b2_nullNode;
--      m_nodes[nodeId].child1 = b2_nullNode;
--      m_nodes[nodeId].child2 = b2_nullNode;
--      m_nodes[nodeId].height = 0;
--      m_nodes[nodeId].userData = NULL;
--      ++m_nodeCount;
--      return nodeId;
--  }




   --  Return a node to the pool.
   --
   procedure FreeNode     (Self : in out b2DynamicTree;    nodeId : in int32)
   is
   begin
      pragma Assert (0 <= nodeId and then nodeId < Self.m_nodeCapacity);
      pragma Assert (0 < Self.m_nodeCount);

      Self.m_nodes (nodeId).next   := Self.m_freeList;
      Self.m_nodes (nodeId).height := -1;
      Self.m_freeList              := nodeId;
      Self.m_nodeCount             := Self.m_nodeCount - 1;
   end FreeNode;


--  void b2DynamicTree::FreeNode(int32 nodeId)
--  {
--      b2Assert(0 <= nodeId && nodeId < m_nodeCapacity);
--      b2Assert(0 < m_nodeCount);
--      m_nodes[nodeId].next = m_freeList;
--      m_nodes[nodeId].height = -1;
--      m_freeList = nodeId;
--      --m_nodeCount;
--  }




   procedure InsertLeaf   (Self : in out b2DynamicTree;    leafId : in int32)
   is
      use impact.d2.Collision;

      center  : b2Vec2;
      sibling : int32;

      node1,
      node2   : int32;

      leafAABB : b2AABB;
      index    : int32;

   begin
      Self.m_insertionCount := Self.m_insertionCount + 1;

      if Self.m_root = b2_nullNode then
         Self.m_root := leafId;
         Self.m_nodes (Self.m_root).parent := b2_nullNode;
         return;
      end if;

      --  Find the best sibling for this node.
      leafAABB := Self.m_nodes (leafId).aabb;
      index    := Self.m_root;


      while not IsLeaf (Self.m_nodes (index))
      loop
         declare
            child1 : constant int32 := Self.m_nodes (index).child1;
            child2 : constant int32 := Self.m_nodes (index).child2;

            area : constant float32 := GetPerimeter (Self.m_nodes (index).aabb);

            combinedAABB : b2AABB;
            combinedArea : float32;

            cost,
            cost1,
            cost2,
            inheritanceCost : float32;

         begin
            Combine (combinedAABB,  Self.m_nodes (index).aabb,
                                    leafAABB);
            combinedArea := GetPerimeter (combinedAABB);

            -- Cost of creating a new parent for this node and the new leaf
            cost := 2.0 * combinedArea;

            -- Minimum cost of pushing the leaf further down the tree
            inheritanceCost := 2.0 * (combinedArea - area);

            -- Cost of descending into child1
            if IsLeaf (Self.m_nodes (child1))
            then
               declare
                  aabb : b2AABB;
               begin
                  Combine (aabb,  leafAABB,
                                  Self.m_nodes (child1).aabb);
                  cost1 := GetPerimeter (aabb) + inheritanceCost;
               end;
            else
               declare
                  aabb    : b2AABB;
                  oldArea,
                  newArea : float32;
               begin
                  aabb.Combine (leafAABB, Self.m_nodes (child1).aabb);
                  oldArea := Self.m_nodes (child1).aabb.GetPerimeter;
                  newArea := aabb.GetPerimeter;
                  cost1   := (newArea - oldArea) + inheritanceCost;
               end;
            end if;

            -- Cost of descending into child2

            if IsLeaf (Self.m_nodes (child2))
            then
               declare
                  aabb : b2AABB;
               begin
                  aabb.Combine (leafAABB, Self.m_nodes (child2).aabb);
                  cost2 := aabb.GetPerimeter + inheritanceCost;
               end;
            else
               declare
                  aabb    : b2AABB;
                  oldArea,
                  newArea : float32;
               begin
                  aabb.Combine (leafAABB, Self.m_nodes (child2).aabb);
                  oldArea := Self.m_nodes (child2).aabb.GetPerimeter;
                  newArea := aabb.GetPerimeter;
                  cost2   := newArea - oldArea + inheritanceCost;
               end;
            end if;

            -- Descend according to the minimum cost.
            if         cost < cost1
              and then cost < cost2
            then
               exit;
            end if;

            -- Descend
            if cost1 < cost2 then
               index := child1;
            else
               index := child2;
            end if;
         end;
      end loop;

      sibling := index;

      -- Create a new parent.
      --
      declare
         oldParent : constant int32 := Self.m_nodes (sibling).parent;
         newParent : constant int32 := Self.AllocateNode;
      begin
         Self.m_nodes (newParent).parent   := oldParent;
         Self.m_nodes (newParent).userData := null;
         Self.m_nodes (newParent).aabb.Combine(leafAABB,
                                               Self.m_nodes (sibling).aabb);
         Self.m_nodes (newParent).height   := Self.m_nodes (sibling).height + 1;

         if oldParent /= b2_nullNode
         then
            -- The sibling was not the root.
            if Self.m_nodes (oldParent).child1 = sibling
            then
               Self.m_nodes (oldParent).child1 := newParent;

            else

               Self.m_nodes (oldParent).child2 := newParent;
            end if;

            Self.m_nodes (newParent).child1 := sibling;
            Self.m_nodes (newParent).child2 := leafId;
            Self.m_nodes (sibling)  .parent := newParent;
            Self.m_nodes (leafId)     .parent := newParent;

         else

            -- The sibling was the root.
            Self.m_nodes (newParent).child1 := sibling;
            Self.m_nodes (newParent).child2 := leafId;
            Self.m_nodes (sibling)  .parent := newParent;
            Self.m_nodes (leafId)     .parent := newParent;
            Self.m_root                     := newParent;
         end if;
      end;

      -- Walk back up the tree fixing heights and AABBs
      index := Self.m_nodes (leafId).parent;

      while index /= b2_nullNode
      loop
         declare
            child1,
            child2 : int32;
         begin
            index := Self.Balance (index);

            child1 := Self.m_nodes (index).child1;
            child2 := Self.m_nodes (index).child2;

            pragma assert (child1 /= b2_nullNode);
            pragma assert (child2 /= b2_nullNode);

            Self.m_nodes (index).height := 1 + int32'Max (Self.m_nodes (child1).height,
                                                          Self.m_nodes (child2).height);

            Self.m_nodes (index).aabb.Combine (Self.m_nodes (child1).aabb,
                                               Self.m_nodes (child2).aabb);

            index := Self.m_nodes (index).parent;
         end;
      end loop;
   end InsertLeaf;


--  void b2DynamicTree::InsertLeaf(int32 leaf)
--  {
--      ++m_insertionCount;
--
--      if (m_root == b2_nullNode)
--      {
--              m_root = leaf;
--              m_nodes[m_root].parent = b2_nullNode;
--              return;
--      }
--
--      // Find the best sibling for this node
--      b2AABB leafAABB = m_nodes[leaf].aabb;
--      int32 index = m_root;
--      while (m_nodes[index].IsLeaf() == false)
--      {
--              int32 child1 = m_nodes[index].child1;
--              int32 child2 = m_nodes[index].child2;
--
--              float32 area = m_nodes[index].aabb.GetPerimeter();
--
--              b2AABB combinedAABB;
--              combinedAABB.Combine(m_nodes[index].aabb, leafAABB);
--              float32 combinedArea = combinedAABB.GetPerimeter();
--
--              // Cost of creating a new parent for this node and the new leaf
--              float32 cost = 2.0f * combinedArea;
--
--              // Minimum cost of pushing the leaf further down the tree
--              float32 inheritanceCost = 2.0f * (combinedArea - area);
--
--              // Cost of descending into child1
--              float32 cost1;
--              if (m_nodes[child1].IsLeaf())
--              {
--                      b2AABB aabb;
--                      aabb.Combine(leafAABB, m_nodes[child1].aabb);
--                      cost1 = aabb.GetPerimeter() + inheritanceCost;
--              }
--              else
--              {
--                      b2AABB aabb;
--                      aabb.Combine(leafAABB, m_nodes[child1].aabb);
--                      float32 oldArea = m_nodes[child1].aabb.GetPerimeter();
--                      float32 newArea = aabb.GetPerimeter();
--                      cost1 = (newArea - oldArea) + inheritanceCost;
--              }
--
--              // Cost of descending into child2
--              float32 cost2;
--              if (m_nodes[child2].IsLeaf())
--              {
--                      b2AABB aabb;
--                      aabb.Combine(leafAABB, m_nodes[child2].aabb);
--                      cost2 = aabb.GetPerimeter() + inheritanceCost;
--              }
--              else
--              {
--                      b2AABB aabb;
--                      aabb.Combine(leafAABB, m_nodes[child2].aabb);
--                      float32 oldArea = m_nodes[child2].aabb.GetPerimeter();
--                      float32 newArea = aabb.GetPerimeter();
--                      cost2 = newArea - oldArea + inheritanceCost;
--              }
--
--              // Descend according to the minimum cost.
--              if (cost < cost1 && cost < cost2)
--              {
--                      break;
--              }
--
--              // Descend
--              if (cost1 < cost2)
--              {
--                      index = child1;
--              }
--              else
--              {
--                      index = child2;
--              }
--      }
--
--      int32 sibling = index;
--
--      // Create a new parent.
--      int32 oldParent = m_nodes[sibling].parent;
--      int32 newParent = AllocateNode();
--      m_nodes[newParent].parent = oldParent;
--      m_nodes[newParent].userData = NULL;
--      m_nodes[newParent].aabb.Combine(leafAABB, m_nodes[sibling].aabb);
--      m_nodes[newParent].height = m_nodes[sibling].height + 1;
--
--      if (oldParent != b2_nullNode)
--      {
--              // The sibling was not the root.
--              if (m_nodes[oldParent].child1 == sibling)
--              {
--                      m_nodes[oldParent].child1 = newParent;
--              }
--              else
--              {
--                      m_nodes[oldParent].child2 = newParent;
--              }
--
--              m_nodes[newParent].child1 = sibling;
--              m_nodes[newParent].child2 = leaf;
--              m_nodes[sibling].parent = newParent;
--              m_nodes[leaf].parent = newParent;
--      }
--      else
--      {
--              // The sibling was the root.
--              m_nodes[newParent].child1 = sibling;
--              m_nodes[newParent].child2 = leaf;
--              m_nodes[sibling].parent = newParent;
--              m_nodes[leaf].parent = newParent;
--              m_root = newParent;
--      }
--
--      // Walk back up the tree fixing heights and AABBs
--      index = m_nodes[leaf].parent;
--      while (index != b2_nullNode)
--      {
--              index = Balance(index);
--
--              int32 child1 = m_nodes[index].child1;
--              int32 child2 = m_nodes[index].child2;
--
--              b2Assert(child1 != b2_nullNode);
--              b2Assert(child2 != b2_nullNode);
--
--              m_nodes[index].height = 1 + b2Max(m_nodes[child1].height, m_nodes[child2].height);
--              m_nodes[index].aabb.Combine(m_nodes[child1].aabb, m_nodes[child2].aabb);
--
--              index = m_nodes[index].parent;
--      }
--
--      //Validate();
--  }




   procedure RemoveLeaf   (Self : in out b2DynamicTree;    leafId : in int32)
   is
      use impact.d2.Collision;

      parent      : int32;
      grandParent : int32;
      sibling     : int32;

      oldAABB     : b2AABB;

   begin
      if leafId = Self.m_root then
         Self.m_root := b2_nullNode;
         return;
      end if;

      parent      := Self.m_nodes (leafId).parent;
      grandParent := Self.m_nodes (parent).parent;

      if Self.m_nodes (parent).child1 = leafId
      then
         sibling := Self.m_nodes (parent).child2;
      else
         sibling := Self.m_nodes (parent).child1;
      end if;

      if grandParent /= b2_nullNode then
         --  Destroy node2 and connect node1 to sibling.
         if Self.m_nodes (grandParent).child1 = parent then
            Self.m_nodes (grandParent).child1 := sibling;
         else
            Self.m_nodes (grandParent).child2 := sibling;
         end if;

         Self.m_nodes (sibling).parent := grandParent;
         Self.FreeNode (parent);

         --  Adjust ancestor bounds.
         declare
            index   : int32 := grandParent;
            child1,
            child2  : int32;
         begin
            while index /= b2_nullNode
            loop
               index  := Self.Balance (index);
               child1 := Self.m_nodes (index).child1;
               child2 := Self.m_nodes (index).child2;

               Self.m_nodes (index).aabb.combine (Self.m_nodes (child1).aabb,
                                                  Self.m_nodes (child2).aabb);

               Self.m_nodes (index).height := 1 + int32'Max (Self.m_nodes (child1).height,
                                                             Self.m_nodes (child2).height);
               index := Self.m_nodes (index).parent;
            end loop;
         end;
      else
         Self.m_root                   := sibling;
         Self.m_nodes (sibling).parent := b2_nullNode;
         Self.FreeNode (parent);
      end if;

   end RemoveLeaf;


--  void b2DynamicTree::RemoveLeaf(int32 leaf)
--  {
--      if (leaf == m_root)
--      {
--              m_root = b2_nullNode;
--              return;
--      }
--
--      int32 parent = m_nodes[leaf].parent;
--      int32 grandParent = m_nodes[parent].parent;
--      int32 sibling;
--      if (m_nodes[parent].child1 == leaf)
--      {
--              sibling = m_nodes[parent].child2;
--      }
--      else
--      {
--              sibling = m_nodes[parent].child1;
--      }
--
--      if (grandParent != b2_nullNode)
--      {
--              // Destroy parent and connect sibling to grandParent.
--              if (m_nodes[grandParent].child1 == parent)
--              {
--                      m_nodes[grandParent].child1 = sibling;
--              }
--              else
--              {
--                      m_nodes[grandParent].child2 = sibling;
--              }
--              m_nodes[sibling].parent = grandParent;
--              FreeNode(parent);
--
--              // Adjust ancestor bounds.
--              int32 index = grandParent;
--              while (index != b2_nullNode)
--              {
--                      index = Balance(index);
--
--                      int32 child1 = m_nodes[index].child1;
--                      int32 child2 = m_nodes[index].child2;
--
--                      m_nodes[index].aabb.Combine(m_nodes[child1].aabb, m_nodes[child2].aabb);
--                      m_nodes[index].height = 1 + b2Max(m_nodes[child1].height, m_nodes[child2].height);
--
--                      index = m_nodes[index].parent;
--              }
--      }
--      else
--      {
--              m_root = sibling;
--              m_nodes[sibling].parent = b2_nullNode;
--              FreeNode(parent);
--      }
--
--      //Validate();
--  }



   function getHeight (Self : in b2DynamicTree) return int32
   is
   begin
      if Self.m_root = b2_nullNode then
         return 0;
      end if;

      return Self.m_nodes (Self.m_root).height;
   end getHeight;



   function GetMaxBalance (Self : in b2DynamicTree) return int32
   is
      maxBalance : int32 := 0;
   begin
      for i in 0 .. Self.m_nodeCapacity - 1
      loop
         declare
            node    : b2DynamicTreeNode renames Self.m_nodes (i);
            child1,
            child2,
            balance : int32;
         begin
            if node.height > 1
            then
               pragma assert (not IsLeaf (node));

               child1     := node.child1;
               child2     := node.child2;
               balance    := abs (  Self.m_nodes (child2).height
                                  - Self.m_nodes (child1).height);
               maxBalance := int32'Max (maxBalance, balance);
            end if;
         end;
      end loop;

      return maxBalance;
   end GetMaxBalance;



   function GetAreaRatio (Self : in b2DynamicTree) return float32
   is
   begin
      if Self.m_root = b2_nullNode
      then
         return 0.0;
      end if;

      declare
         root      : b2DynamicTreeNode renames Self.m_nodes (Self.m_root);
         rootArea  : float32    :=      root.aabb.GetPerimeter;
         totalArea : float32    :=      0.0;
      begin
         for i in 0 .. Self.m_nodeCapacity - 1
         loop
            declare
               node : b2DynamicTreeNode renames Self.m_nodes (i);
            begin
               if node.height < 0
               then
                  -- Free node in pool
                  null;
               else
                  totalArea := totalArea + node.aabb.GetPerimeter;
               end if;
            end;
         end loop;

         return totalArea / rootArea;
      end;

   end GetAreaRatio;





   function  ComputeHeight (Self : in     b2DynamicTree) return int32
   is
   begin
      return Self.ComputeHeight (Self.m_root);
   end ComputeHeight;






   --  Compute the height of a sub-tree.
   --
   function  ComputeHeight (Self : in     b2DynamicTree;   nodeId : in int32) return int32
   is
      pragma assert (         0      <= nodeId
                     and then nodeId <  Self.m_nodeCapacity);

      node : b2DynamicTreeNode renames Self.m_nodes (nodeId);
   begin
      if isLeaf (node) then
         return 0;
      end if;

      declare
         height1 : constant int32 := Self.computeHeight (node.child1);
         height2 : constant int32 := Self.computeHeight (node.child2);
      begin
         return 1 + int32'Max (height1, height2);
      end;
   end ComputeHeight;


--  int32 b2DynamicTree::ComputeHeight(int32 nodeId) const
--  {
--      b2Assert(0 <= nodeId && nodeId < m_nodeCapacity);
--      b2TreeNode* node = m_nodes + nodeId;
--
--      if (node->IsLeaf())
--      {
--              return 0;
--      }
--
--      int32 height1 = ComputeHeight(node->child1);
--      int32 height2 = ComputeHeight(node->child2);
--      return 1 + b2Max(height1, height2);
--  }



end impact.d2.dynamic_Tree;
