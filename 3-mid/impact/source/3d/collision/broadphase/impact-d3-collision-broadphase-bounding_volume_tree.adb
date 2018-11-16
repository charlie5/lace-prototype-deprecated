with ada.unchecked_Deallocation;
with impact.d3.collision.bounding_volume_Tree;
with impact.d3.collision.Proxy;



package body impact.d3.collision.Broadphase.bounding_volume_Tree
is




   --  Helpers
   --

   procedure listappend (the_item : in Proxy_view;   List : in out Proxy_view)
   is
   begin
      the_item.links (1) := null;
      the_item.links (2) := list;

      if list /= null then
         list.links (1) := the_item;
      end if;

      list := the_item;
   end listappend;





   procedure listremove (the_item : in Proxy_view;   List : in out Proxy_view)
   is
   begin
      if the_item.links (1) /= null then
         the_item.links (1).links (2) := the_item.links (2);
      else
         list := Proxy_view (the_item.links (2));
      end if;


      if the_item.links (2) /= null then
         the_item.links (2).links (1) := the_item.links (1);
      end if;
   end listremove;






   function listcount (the_root : in Proxy_view) return Integer
   is
      root : Proxy_view := the_root;
      n    : Natural          := 0;
   begin
      while root /= null
      loop
         n    := n + 1;
         root := root.links (2).all'Access;
      end loop;

      return n;
   end listcount;







   --- Colliders
   --


   --  Tree collider
   --


   function to_TreeCollider (p : access impact.d3.collision.Broadphase.bounding_volume_Tree.item'Class) return TreeCollider
   is
      Self : TreeCollider;
   begin
      Self.pbp := p;

      return Self;
   end to_TreeCollider;



   overriding procedure Process (Self : in out TreeCollider;   na, nb : access impact.d3.collision.bounding_volume_Tree.Node'Class)
   is
      pa, pb : access Proxy'Class;
      unused : access impact.d3.collision.Proxy.btBroadphasePair;
      pragma Unreferenced (unused);
   begin
      if na /= nb then
         pa := Proxy_view (na.state.data);
         pb := Proxy_view (nb.state.data);

--           if DBVT_BP_SORTPAIRS then
--              if pa.m_uniqueId > pb.m_uniqueId then
--                 btSwap (pa, pb);
--              end if;
--           end if;

         unused := Self.pbp.m_paircache.addOverlappingPair (pa, pb);
         Self.pbp.m_newpairs := Self.pbp.m_newpairs + 1;
      end if;
   end Process;




   overriding procedure Process (Self : in out TreeCollider;   n : access impact.d3.collision.bounding_volume_Tree.Node'Class)
   is
   begin
      Self.Process (n, Self.proxy.leaf);
   end Process;











   --- impact.d3.collision.bounding_volume_TreeProxy
   --

   function to_Proxy (aabbMin, aabbMax     : in     math.Vector_3;
                            userPtr              : access Any'Class;
                            collisionFilterGroup : in     impact.d3.collision.Proxy.CollisionFilterGroups;
                      collisionFilterMask  : in     impact.d3.collision.Proxy.CollisionFilterGroups) return bounding_volume_Tree.Proxy
   is
      use impact.d3.collision.Proxy.Forge;

      Self : constant Proxy := (impact.d3.collision.Proxy.forge.to_Proxy (aabbMin, aabbMax,  userPtr,  collisionFilterGroup, collisionFilterMask)
                       with others => <>);
   begin
      return Self;
   end to_Proxy;













   --- impact.d3.collision.Broadphase.bounding_volume_Tree
   --

   --  Forge
   --
--     function  to_impact.d3.collision.Broadphase.bounding_volume_Tree (paircache : access impact.d3.collision.overlapped_pair_Callback.cached.Item := null) return Item
--     is
--     begin
--        return Self : Item
--        do
--           Self.m_deferedcollide   := False;
--           Self.m_needcleanup      := True;
--
--           if paircache /= null then
--              Self.m_releasepaircache := False;
--           else
--              Self.m_releasepaircache := True;
--           end if;
--
--           Self.m_prediction       := 0.0;
--           Self.m_stageCurrent     := 0;
--           Self.m_fixedleft        := 0;
--           Self.m_fupdates         := 1;
--           Self.m_dupdates         := 0;
--           Self.m_cupdates         := 10;
--           Self.m_newpairs         := 1;
--           Self.m_updates_call     := 0;
--           Self.m_updates_done     := 0;
--           Self.m_updates_ratio    := 0.0;
--
--
--           if paircache /= null then
--              Self.m_paircache     := paircache.all'unchecked_access;
--           else
--  --              Self.m_paircache     := new impact.d3.collision.overlapped_pair_Callback.cached.btHashedOverlappingPairCache' (impact.d3.collision.overlapped_pair_Callback.cached.to_btHashedOverlappingPairCache);
--              Self.m_paircache     := impact.d3.collision.overlapped_pair_Callback.cached.new_btHashedOverlappingPairCache;
--           end if;
--
--           Self.m_gid              := 0;
--           Self.m_pid              := 0;
--           Self.m_cid              := 0;
--
--           for i in 1 .. STAGECOUNT + 1
--           loop
--              Self.m_stageRoots (i) := null;
--           end loop;
--        end return;
--     end;




   function  new_Broadphase (paircache : access impact.d3.collision.overlapped_pair_Callback.cached.Item := null) return View
   is
      Self : constant View := new Item;

   begin
         Self.m_deferedcollide   := False;
         Self.m_needcleanup      := True;

         if paircache /= null then
            Self.m_releasepaircache := False;
         else
            Self.m_releasepaircache := True;
         end if;

         Self.m_prediction       := 0.0;
         Self.m_stageCurrent     := 0;
         Self.m_fixedleft        := 0;
         Self.m_fupdates         := 1;
         Self.m_dupdates         := 0;
         Self.m_cupdates         := 10;
         Self.m_newpairs         := 1;
         Self.m_updates_call     := 0;
         Self.m_updates_done     := 0;
         Self.m_updates_ratio    := 0.0;


         if paircache /= null then
            Self.m_paircache := paircache.all'Unchecked_Access;
         else
            Self.m_paircache := cached_overlapped_pair_Callback_view (impact.d3.collision.overlapped_pair_Callback.cached.new_btHashedOverlappingPairCache);
         end if;

         Self.m_gid              := 0;
         Self.m_pid              := 0;
         Self.m_cid              := 0;

         for i in 1 .. STAGECOUNT + 1
         loop
            Self.m_stageRoots (i) := null;
         end loop;

      return Self;
   end new_Broadphase;







   overriding procedure destruct            (Self      : in out Item)
   is
      procedure free is new ada.unchecked_Deallocation (impact.d3.collision.overlapped_pair_Callback.cached.Item'Class, cached_overlapped_pair_Callback_view);
   begin
      if Self.m_releasepaircache then
         Self.m_paircache.destruct;
         free (Self.m_paircache);
      end if;
   end destruct;





   --  Operations
   --

   procedure collide (Self : in out Item;   dispatcher : access impact.d3.Dispatcher.item'Class)
   is
      use type Interfaces.Unsigned_32;

      current,
      next   : Proxy_view;

   begin
      --  optimize
      --
      Self.m_sets (1).optimizeIncremental (1 + (Self.m_sets (1).Leaves * Self.m_dupdates) / 100);

      if Self.m_fixedleft /= 0 then
         declare
            count : constant Integer := 1  +  (Self.m_sets (2).Leaves * Self.m_fupdates) / 100;
         begin
            Self.m_sets (2).optimizeIncremental (1 + (Self.m_sets (2).Leaves * Self.m_fupdates) / 100);
            Self.m_fixedleft := Integer'Max (0,  Self.m_fixedleft - count);
         end;
      end if;


      --  dynamic -> fixed set
      --
      Self.m_stageCurrent := (Self.m_stageCurrent + 1) mod STAGECOUNT;
      current             := Self.m_stageRoots (Self.m_stageCurrent + 1);

      if current /= null then
         declare
            collider : aliased TreeCollider := to_TreeCollider (Self'Unchecked_Access);
         begin
            loop
               next := Proxy_view (current.links (2));

               listremove (current, Self.m_stageRoots (current.stage + 1));
               listappend (current, Self.m_stageRoots (STAGECOUNT + 1));

               --                 if DBVT_BP_ACCURATESLEEPING then
               --                    Self.m_paircache.removeOverlappingPairsContainingProxy (current, dispatcher);
               --
               --                    collider.proxy := current;
               --
               --                    impact.d3.collision.bounding_volume_Tree.collideTV (Self.m_sets (1).Root, current.aabb, collider'access);
               --                    impact.d3.collision.bounding_volume_Tree.collideTV (Self.m_sets (2).Root, current.aabb, collider'access);
               --                 end if;

               Self.m_sets (1).remove (current.leaf);

               declare
                  curAabb : constant impact.d3.collision.bounding_volume_Tree.Volume := impact.d3.collision.bounding_volume_Tree.FromMM (current.m_aabbMin,  current.m_aabbMax);
               begin
                  current.leaf  := Self.m_sets (2).insert (curAabb, current);
                  current.stage := STAGECOUNT;
                  current       := next;
               end;

               exit when current = null;
            end loop;

            Self.m_fixedleft   := Self.m_sets (2).Leaves;
            Self.m_needcleanup := True;
         end;
      end if;


      --  collide dynamics
      --
      declare
         collider : aliased TreeCollider := to_TreeCollider (Self'Unchecked_Access);
      begin
         if Self.m_deferedcollide then
            Self.m_sets (1).collideTTpersistentStack (Self.m_sets (1).Root,  Self.m_sets (2).Root,  collider'Access);
         end if;

         if Self.m_deferedcollide then
            Self.m_sets (1).collideTTpersistentStack (Self.m_sets (1).Root,  Self.m_sets (1).Root,  collider'Access);
         end if;
      end;


      --  clean up
      --
      if Self.m_needcleanup then
         declare
            use type ada.containers.Count_type;

            pairs : impact.d3.collision.overlapped_pair_Callback.cached.btBroadphasePairArray renames Self.m_paircache.getOverlappingPairArray.all;
            i, ni : Integer;

         begin
            if pairs.Length > 0 then
               ni := Integer'Min (Integer (pairs.Length),
                                  Integer'Max (Self.m_newpairs,  (Integer (pairs.Length) * Self.m_cupdates) / 100));
               i  := 0;

               while i < ni
               loop
                  declare
                     index  : constant Integer := (Self.m_cid + i) mod Integer (pairs.Length) + 1;

                     p  : constant impact.d3.collision.Proxy.btBroadphasePair_view := pairs.Element (index);

                     pa : Proxy_view := Proxy_view (p.m_pProxy0);
                     pb : Proxy_view := Proxy_view (p.m_pProxy1);

                     unused : access Any'Class;
                     pragma Unreferenced (unused);
                  begin
                     if not impact.d3.collision.bounding_volume_Tree.Intersect (pa.leaf.volume,  pb.leaf.volume) then

                        if DBVT_BP_SORTPAIRS then
                           if pa.m_uniqueId > pb.m_uniqueId then
                              declare
                                 Pad : constant Proxy_view := pa;
                              begin
                                 pa := pb;
                                 pb := Pad;   --  btSwap (pa, pb);
                              end;
                           end if;
                        end if;

                        unused := Self.m_paircache.removeOverlappingPair (pa, pb, dispatcher);

                        ni := ni - 1;
                        i  := i  - 1;
                     end if;
                  end;

                  i := i + 1;
               end loop;


               if pairs.Length > 0 then
                  Self.m_cid := (Self.m_cid + ni) mod Integer (pairs.Length);
               else
                  Self.m_cid := 0;
               end if;

            end if;
         end;
      end if;


      Self.m_pid         := Self.m_pid + 1;
      Self.m_newpairs    := 1;
      Self.m_needcleanup := False;

      if Self.m_updates_call > 0 then
         Self.m_updates_ratio := math.Real (Self.m_updates_done) / math.Real (Self.m_updates_call);
      else
         Self.m_updates_ratio := 0.0;
      end if;

      Self.m_updates_done := Self.m_updates_done / 2;
      Self.m_updates_call := Self.m_updates_call / 2;
   end Collide;







   procedure optimize (Self : in out Item)
   is
   begin
      Self.m_sets (1).optimizeTopDown;
      Self.m_sets (2).optimizeTopDown;
   end optimize;







   overriding function createProxy (Self : access Item;    aabbMin, aabbMax     : in     math.Vector_3;
                                                shapeType            : in     impact.d3.collision.Proxy.BroadphaseNativeTypes;
                                                userPtr              : access Any'Class;
                                                collisionFilterGroup : in     impact.d3.collision.Proxy.CollisionFilterGroups;
                                                collisionFilterMask  : in     impact.d3.collision.Proxy.CollisionFilterGroups;
                                                dispatcher           : access impact.d3.Dispatcher.item'Class;
                                                multiSapProxy        : in     System.Address) return access impact.d3.collision.Proxy.item'Class
   is
      pragma Unreferenced (shapeType, dispatcher, multiSapProxy);
      proxy : constant Proxy_view  := new bounding_volume_Tree.Proxy'(to_Proxy (aabbMin, aabbMax,
                                                                        userPtr,
                                                                        collisionFilterGroup,
                                                                        collisionFilterMask));
      aabb  : constant impact.d3.collision.bounding_volume_Tree.AabbMm := impact.d3.collision.bounding_volume_Tree.FromMM (aabbMin, aabbMax);

   begin
      --  bproxy->aabb                        =        impact.d3.collision.bounding_volume_TreeVolume::FromMM(aabbMin,aabbMax);
      proxy.stage      := Self.m_stageCurrent;
      Self.m_gid       := Self.m_gid + 1;
      proxy.m_uniqueId := Self.m_gid;
      proxy.leaf       := Self.m_sets (1).insert (aabb, proxy);

      listappend (proxy,  Self.m_stageRoots (Self.m_stageCurrent + 1));

      if not Self.m_deferedcollide then
         declare
            collider : aliased TreeCollider := to_TreeCollider (Self);
         begin
            collider.proxy := proxy;

            Self.m_sets (1).collideTV (Self.m_sets (1).Root,  aabb,  collider'Access);
            Self.m_sets (2).collideTV (Self.m_sets (2).Root,  aabb,  collider'Access);
         end;
      end if;


      return proxy.all'Access;
   end createProxy;






   overriding procedure destroyProxy (Self : access Item;   proxy      : access impact.d3.collision.Proxy.item'Class;
                           dispatcher : access impact.d3.Dispatcher.item'Class)
   is
--        type Proxy_view is access all Broadphase.bounding_volume_Tree.Proxy'Class;

      procedure free is new ada.unchecked_Deallocation (Broadphase.bounding_volume_Tree.Proxy'Class, Proxy_view);

      the_proxy : Proxy_view := Proxy_view (proxy);

   begin
      if the_proxy.stage = STAGECOUNT then
         Self.m_sets (2).remove (the_proxy.leaf);
      else
         Self.m_sets (1).remove (the_proxy.leaf);
      end if;

      listremove (the_proxy,  Self.m_stageRoots (the_proxy.stage + 1));

      Self.m_paircache.removeOverlappingPairsContainingProxy (the_proxy.all'Access, dispatcher);
      free (the_proxy);
      Self.m_needcleanup := True;
   end destroyProxy;







   overriding procedure setAabb (Self : access Item;   absproxy         : access impact.d3.collision.Proxy.item'Class;
                                            aabbMin, aabbMax : in     math.Vector_3;
                      dispatcher       : access impact.d3.Dispatcher.item'Class)
   is
      pragma Unreferenced (dispatcher);
      use impact.d3.collision.bounding_volume_Tree;
      use type Interfaces.Unsigned_32;

      proxy : constant Proxy_view := Proxy_view (absproxy);
      aabb  : aliased  Volume     := impact.d3.collision.bounding_volume_Tree.FromMM    (aabbMin, aabbMax);

      docollide : Boolean;

      update_Result : Boolean;
   begin
      if DBVT_BP_PREVENTFALSEUPDATE then
         if not NotEqual (aabb,  proxy.leaf.volume) then
            return;
         end if;
      end if;


      docollide := False;

      if proxy.stage = STAGECOUNT then
         --  fixed -> dynamic set
         Self.m_sets (2).remove (proxy.leaf);
         proxy.leaf := Self.m_sets (1).insert (aabb, proxy);
         docollide  := True;

      else
         --  dynamic set
         Self.m_updates_call := Self.m_updates_call + 1;

         if Intersect (proxy.leaf.volume, aabb) then
            --  Moving
            declare
               the_delta : constant math.Vector_3 := aabbMin - proxy.m_aabbMin;
               velocity  :          math.Vector_3 := (proxy.m_aabbMax - proxy.m_aabbMin)/2.0 * Self.m_prediction;
            begin
               if the_delta (1) < 0.0 then   velocity (1) := -velocity (1);   end if;
               if the_delta (2) < 0.0 then   velocity (2) := -velocity (2);   end if;
               if the_delta (3) < 0.0 then   velocity (3) := -velocity (3);   end if;


               --                 if DBVT_BP_MARGIN then
               update_Result := Self.m_sets (1).update (proxy.leaf, aabb'Access, velocity, DBVT_BP_MARGIN);
               --                 else
               --                    update_Result := Self.m_sets (1).update (proxy.leaf, aabb, velocity)
               --                 end if;

               if update_Result then
                  Self.m_updates_done := Self.m_updates_done + 1;
                  docollide           := True;
               end if;
            end;

         else
            --  Teleporting
            Self.m_sets (1).update (proxy.leaf, aabb);
            Self.m_updates_done := Self.m_updates_done + 1;
            docollide           := True;
         end if;
      end if;


      listremove (proxy, Self.m_stageRoots (proxy.stage + 1));

      proxy.m_aabbMin := aabbMin;
      proxy.m_aabbMax := aabbMax;
      proxy.stage     := Self.m_stageCurrent;

      listappend (proxy,  Self.m_stageRoots (Self.m_stageCurrent + 1));


      if docollide then
         Self.m_needcleanup := True;

         if not Self.m_deferedcollide then
            declare
               collider : aliased TreeCollider := to_TreeCollider (Self);
            begin
               Self.m_sets (2).collideTTpersistentStack (Self.m_sets (2).Root,  proxy.leaf,  collider'Access);
               Self.m_sets (1).collideTTpersistentStack (Self.m_sets (1).Root,  proxy.leaf,  collider'Access);
            end;
         end if;
      end if;

   end setAabb;








   overriding procedure getAabb (Self : in Item;   proxy            : access impact.d3.collision.Proxy.item;
                      aabbMin, aabbMax :    out math.Vector_3)
   is
      pragma Unreferenced (Self);
--        the_proxy : impact.d3.collision.bounding_volume_TreeProxy := Proxy_view (proxy);
   begin
      aabbMin := proxy.m_aabbMin;
      aabbMax := proxy.m_aabbMax;
   end getAabb;









   --- Raytest
   --

   type BroadphaseRayTester (m_rayCallback : access impact.d3.collision.Broadphase.btBroadphaseRayCallback'Class) is new impact.d3.collision.bounding_volume_Tree.ICollide with null record;



   procedure Process (Self : in out BroadphaseRayTester;   leaf : in impact.d3.collision.bounding_volume_Tree.Node)
   is
      proxy  : constant access bounding_volume_Tree.Proxy'Class := Proxy_view (leaf.state.data);
      unused : Boolean;
      pragma Unreferenced (unused);
   begin
      unused := Self.m_rayCallback.process (proxy.all);
   end Process;





   overriding procedure rayTest (Self : access Item;   rayFrom, rayTo   : in     math.Vector_3;
                                            rayCallback      : in out impact.d3.collision.Broadphase.btBroadphaseRayCallback'Class;
                                            aabbMin, aabbMax : in     math.Vector_3 := (0.0, 0.0, 0.0))
   is
      callback : aliased BroadphaseRayTester := (m_rayCallback => rayCallback'Access);
   begin
      Self.m_sets (1).rayTestInternal (Self.m_sets (1).Root,
                                       rayFrom,
                                       rayTo,
                                       rayCallback.m_rayDirectionInverse,
                                       rayCallback.m_signs,
                                       rayCallback.m_lambda_max,
                                       aabbMin,
                                       aabbMax,
                                       callback'Access);

      Self.m_sets (2).rayTestInternal (Self.m_sets (2).Root,
                                       rayFrom,
                                       rayTo,
                                       rayCallback.m_rayDirectionInverse,
                                       rayCallback.m_signs,
                                       rayCallback.m_lambda_max,
                                       aabbMin,
                                       aabbMax,
                                       callback'Access);

   end rayTest;







   type BroadphaseAabbTester (m_aabbCallback : access impact.d3.collision.Broadphase.btBroadphaseAabbCallback'Class) is new impact.d3.collision.bounding_volume_Tree.ICollide with null record;


   procedure Process (Self : in out BroadphaseAabbTester;   leaf : in impact.d3.collision.bounding_volume_Tree.Node)
   is
      proxy  : constant access bounding_volume_Tree.Proxy'Class := Proxy_view (leaf.state.data);
      unused : Boolean;
      pragma Unreferenced (unused);
   begin
      unused := Self.m_aabbCallback.process (proxy.all);
   end Process;











   overriding procedure aabbTest (Self : access Item;   aabbMin, aabbMax : in     math.Vector_3;
                                             callback         : in out impact.d3.collision.Broadphase.btBroadphaseAabbCallback'Class)
   is
      the_callback : aliased BroadphaseAabbTester := (m_aabbCallback => callback'Access);
      bounds       : constant impact.d3.collision.bounding_volume_Tree.Volume := impact.d3.collision.bounding_volume_Tree.FromMM (aabbMin, aabbMax);
   begin
      --  process all children, that overlap with  the given AABB bounds
      --
      Self.m_sets (1).collideTV (Self.m_sets (1).Root,  bounds,  the_callback'Access);
      Self.m_sets (2).collideTV (Self.m_sets (2).Root,  bounds,  the_callback'Access);
   end aabbTest;








   --  calculateOverlappingPairs is optional: incremental algorithms (sweep and prune) might do it during the set aabb
   --
   overriding procedure calculateOverlappingPairs (Self : access Item;   dispatcher : access impact.d3.Dispatcher.item'Class)
   is
   begin
      Self.collide                (dispatcher);
      Self.performDeferredRemoval (dispatcher);
   end calculateOverlappingPairs;







   overriding function getOverlappingPairCache    (Self : in Item) return access impact.d3.collision.overlapped_pair_Callback.cached.item'Class
   is
   begin
      return Self.m_paircache;
   end getOverlappingPairCache;





   overriding function getOverlappingPairCache    (Self : in Item) return impact.d3.collision.overlapped_pair_Callback.cached.item'Class
   is
   begin
      return Self.m_paircache.all;
   end getOverlappingPairCache;






   --  getAabb returns the axis aligned bounding box in the 'global' coordinate frame
   --  will add some transform later
   --
   overriding procedure getBroadphaseAabb         (Self : in Item;   aabbMin, aabbMax : in out math.Vector_3)
   is
      bounds : impact.d3.collision.bounding_volume_Tree.Volume;
   begin
      if not Self.m_sets (1).empty then

         if not Self.m_sets (2).empty then
            impact.d3.collision.bounding_volume_Tree.Merge (Self.m_sets (1).Root.volume,
                          Self.m_sets (2).Root.volume,
                          bounds);
         else
            bounds := Self.m_sets (1).Root.volume;
         end if;

      elsif not Self.m_sets (2).empty then
         bounds := Self.m_sets (2).Root.volume;

      else
         bounds := impact.d3.collision.bounding_volume_Tree.FromCR ((0.0, 0.0, 0.0),  0.0);
      end if;


      aabbMin := bounds.Mins;
      aabbMax := bounds.Maxs;
   end getBroadphaseAabb;








   --  Reset broadphase internal structures, to ensure determinism/reproducability.
   --
   overriding procedure resetPool (Self : in out Item;   dispatcher : access impact.d3.Dispatcher.item)
   is
      pragma Unreferenced (dispatcher);
      totalObjects : constant Integer := Self.m_sets (1).Leaves + Self.m_sets (2).Leaves;
   begin
      if totalObjects = 0 then   -- reset internal dynamic tree data structures
         Self.m_sets (1).clear;
         Self.m_sets (2).clear;

         Self.m_deferedcollide := False;
         Self.m_needcleanup    := True;
         Self.m_stageCurrent   := 0;
         Self.m_fixedleft      := 0;
         Self.m_fupdates       := 1;
         Self.m_dupdates       := 0;
         Self.m_cupdates       := 10;
         Self.m_newpairs       := 1;
         Self.m_updates_call   := 0;
         Self.m_updates_done   := 0;
         Self.m_updates_ratio  := 0.0;

         Self.m_gid := 0;
         Self.m_pid := 0;
         Self.m_cid := 0;

         for i in 1 .. STAGECOUNT + 1
         loop
            Self.m_stageRoots (i) := null;
         end loop;
      end if;

   end resetPool;






   procedure performDeferredRemoval (Self : access Item;   dispatcher : access impact.d3.Dispatcher.item'Class)
   is
   begin
      if Self.m_paircache.hasDeferredRemoval then
         declare
            use ada.Containers, impact.d3.collision.Proxy;

            overlappingPairArray : impact.d3.collision.overlapped_pair_Callback.cached.btBroadphasePairArray renames Self.m_paircache.getOverlappingPairArray.all;

            invalidPair          : Integer;
            previousPair         : impact.d3.collision.Proxy.btBroadphasePair;


            function "<" (L, R : in btBroadphasePair_view) return Boolean
            is
            begin
               return btBroadphasePairSortPredicate (L.all, R.all);
            end "<";


            package Sorter is new impact.d3.collision.overlapped_pair_Callback.cached.btBroadphasePair_Vectors.generic_Sorting ("<");

         begin
            --  Perform a sort, to find duplicates and to sort 'invalid' pairs to the end.
            --
            Sorter.sort (overlappingPairArray);


            invalidPair := 0;

            previousPair.m_pProxy0   := null;
            previousPair.m_pProxy1   := null;
            previousPair.m_algorithm := null;


            for i in 1 .. Integer (overlappingPairArray.Length)
            loop
               declare
                  pair         : impact.d3.collision.Proxy.btBroadphasePair renames overlappingPairArray.Element (i).all;
                  isDuplicate  : constant Boolean          :=      (pair = previousPair);
                  needsRemoval : Boolean          :=      False;
               begin
                  previousPair := pair;

                  if not isDuplicate then
                     declare
                        --  important to perform AABB check that is consistent with the broadphase
                        pa : constant Proxy_view := Proxy_view (pair.m_pProxy0);
                        pb : constant Proxy_view := Proxy_view (pair.m_pProxy1);

                        hasOverlap : constant Boolean  := impact.d3.collision.bounding_volume_Tree.Intersect (pa.leaf.volume,  pb.leaf.volume);
                     begin
                        if hasOverlap then
                           needsRemoval := False;
                        else
                           needsRemoval := True;
                        end if;
                     end;

                  else                         -- remove duplicate
                     needsRemoval := True;
                     pragma Assert (pair.m_algorithm = null);   -- should have no algorithm
                  end if;


                  if needsRemoval then
                     Self.m_paircache.cleanOverlappingPair (pair'Access, dispatcher);

                     pair.m_pProxy0 := null;
                     pair.m_pProxy1 := null;
                     invalidPair    := invalidPair + 1;
                  end if;

               end;
            end loop;


            --  perform a sort, to sort 'invalid' pairs to the end
            --
            Sorter.sort                     (overlappingPairArray);
            overlappingPairArray.set_Length (overlappingPairArray.Length  -  Count_type (invalidPair));

         end;
      end if;
   end performDeferredRemoval;









   procedure setVelocityPrediction     (Self : in out Item;   prediction : in     math.Real)
   is
   begin
      Self.m_prediction := prediction;
   end setVelocityPrediction;






   function  getVelocityPrediction     (Self : in     Item)                return math.Real
   is
   begin
      return Self.m_prediction;
   end getVelocityPrediction;








   --          This setAabbForceUpdate is similar to setAabb but always forces the aabb update.
   --          It is not part of the impact.d3.collision.Broadphase but specific to impact.d3.collision.Broadphase.bounding_volume_Tree.
   --          It bypasses certain optimizations that prevent aabb updates (when the aabb shrinks), see
   --          http://code.google.com/p/bullet/issues/detail?id=223
   --
   procedure setAabbForceUpdate (Self : in out Item'Class;   absproxy         : access impact.d3.collision.Proxy.item'Class;
                                                             aabbMin, aabbMax : in     math.Vector_3;
                                 dispatcher       : access impact.d3.Dispatcher.item)
   is
      pragma Unreferenced (dispatcher);
      use type Interfaces.Unsigned_32;
      proxy     : constant Proxy_view    := Proxy_view (absproxy);
      aabb      : constant impact.d3.collision.bounding_volume_Tree.Volume := impact.d3.collision.bounding_volume_Tree.FromMM    (aabbMin, aabbMax);
      docollide : Boolean             := False;

   begin
      if proxy.stage = STAGECOUNT then   -- fixed -> dynamic set
         Self.m_sets (2).remove (proxy.leaf);
         proxy.leaf := Self.m_sets (1).insert (aabb, proxy);
         docollide  := True;

      else                               -- dynamic set
         Self.m_updates_call := Self.m_updates_call + 1;
         --  Teleporting
         Self.m_sets (1).update (proxy.leaf, aabb);
         Self.m_updates_done := Self.m_updates_done + 1;
         docollide           := True;
      end if;


      listremove (proxy, Self.m_stageRoots (proxy.stage + 1));

      proxy.m_aabbMin := aabbMin;
      proxy.m_aabbMax := aabbMax;
      proxy.stage     := Self.m_stageCurrent;

      listappend (proxy, Self.m_stageRoots (Self.m_stageCurrent + 1));


      if docollide then
         Self.m_needcleanup := True;

         if not Self.m_deferedcollide then
            declare
               collider : aliased TreeCollider := to_TreeCollider (Self'Access);
            begin
               Self.m_sets (2).collideTTpersistentStack (Self.m_sets (2).Root,  proxy.leaf,  collider'Access);
               Self.m_sets (1).collideTTpersistentStack (Self.m_sets (1).Root,  proxy.leaf,  collider'Access);
            end;
         end if;

      end if;

   end setAabbForceUpdate;




end impact.d3.collision.Broadphase.bounding_volume_Tree;
