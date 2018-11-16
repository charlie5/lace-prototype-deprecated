with impact.d3.collision.overlapped_pair_Callback.cached;
with impact.d3.collision.Proxy;
--  #include "LinearMath/impact.d3.Scalar.h"
--  #include "impact.d3.simulation_island_Manager.h"
--  #include "BulletCollision/BroadphaseCollision/impact.d3.Dispatcher.h"
--  #include "BulletCollision/NarrowPhaseCollision/impact.d3.Manifold.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.Object.h"
--  #include "BulletCollision/CollisionDispatch/impact.d3.Space.h"
--
--  //#include <stdio.h>
--  #include "LinearMath/btQuickprof.h"


package body impact.d3.simulation_island_Manager
is



   --- Forge
   --


   procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;





   --- Attributes
   --




   procedure initUnionFind (Self : in out Item;   n : in Integer)
   is
   begin
      Self.m_unionFind.reset (n);
   end initUnionFind;






   function  getUnionFind (Self : access Item) return impact.d3.union_Find.view
   is
   begin
      return Self.m_unionFind'Access;
   end getUnionFind;






   procedure findUnions (Self : in out Item;   dispatcher : access impact.d3.Dispatcher.item'Class;
                         colWorld   : access impact.d3.Space.Item    'Class)
   is
      pragma Unreferenced (dispatcher);
      use impact.d3.collision.overlapped_pair_Callback.cached,  impact.d3.collision.overlapped_pair_Callback.cached.btBroadphasePair_Vectors;

      pairCachePtr        : constant access   impact.d3.collision.overlapped_pair_Callback.cached.Item'Class := colWorld.getPairCache;
      numOverlappingPairs : constant Integer                                                        := pairCachePtr.getNumOverlappingPairs;

      pairPtr             : btBroadphasePair_Vectors.Cursor; -- access impact.d3.collision.Proxy.btBroadphasePair;

   begin
      if numOverlappingPairs /= 0 then
         pairPtr := pairCachePtr.getOverlappingPairArrayPtr;

         for i in 1 .. numOverlappingPairs
         loop
            declare
               use type impact.d3.Object.view;

               collisionPair : impact.d3.collision.Proxy.btBroadphasePair renames Element (pairPtr).all; --  (i);

               colObj0 : constant impact.d3.Object.view := impact.d3.Object.view (collisionPair.m_pProxy0.m_clientObject);
               colObj1 : constant impact.d3.Object.view := impact.d3.Object.view (collisionPair.m_pProxy1.m_clientObject);
            begin
               if         (       colObj0 /= null
                           and then (colObj0.mergesSimulationIslands))
                 and then (       colObj1 /= null
                           and then (colObj1.mergesSimulationIslands))
               then

                  Self.m_unionFind.unite (colObj0.getIslandTag,
                                          colObj1.getIslandTag);
               end if;
            end;

            next (pairPtr);
         end loop;

      end if;
   end findUnions;






   procedure updateActivationState (Self : in out Item;   colWorld   : access impact.d3.Space.Item    'Class;
                                                          dispatcher : access impact.d3.Dispatcher.item'Class)
   is
   begin

      if impact.d3.union_Find.STATIC_SIMULATION_ISLAND_OPTIMIZATION then
         declare
            index           :        Integer := 0;   -- tbd: or 1 ?
            collisionObject : access impact.d3.Object.item'Class;

         begin
            --  Put the index into m_controllers into m_tag.
            --
            for i in 1 .. Integer (colWorld.getCollisionObjectArray.Length)
            loop
               collisionObject := colWorld.getCollisionObjectArray.Element (i);

               --  Adding filtering here
               if not collisionObject.isStaticOrKinematicObject then
                  collisionObject.setIslandTag (index);
                  index := index + 1;
               end if;

               collisionObject.setCompanionId (-1);
               collisionObject.setHitFraction (1.0);
            end loop;


            --  Do the union find.
            --
            Self.initUnionFind (index);
            Self.findUnions (dispatcher, colWorld);
         end;


      else
         raise Program_Error; -- tbd: (port this below, if really needed).

         --          initUnionFind( int (colWorld->getCollisionObjectArray().size()));
         --
         --          // put the index into m_controllers into m_tag
         --          {
         --
         --                  int index = 0;
         --                  int i;
         --                  for (i=0;i<colWorld->getCollisionObjectArray().size(); i++)
         --                  {
         --                          impact.d3.Object*        collisionObject= colWorld->getCollisionObjectArray()[i];
         --                          collisionObject->setIslandTag(index);
         --                          collisionObject->setCompanionId(-1);
         --                          collisionObject->setHitFraction(impact.d3.Scalar(1.));
         --                          index++;
         --
         --                  }
         --          }
         --          // do the union find
         --
         --          findUnions(dispatcher,colWorld);
      end if;

   end updateActivationState;








   procedure storeIslandActivationState (Self : in out Item;   World   : access impact.d3.Space.Item'Class)
   is
   begin
      if impact.d3.union_Find.STATIC_SIMULATION_ISLAND_OPTIMIZATION then
         --  put the islandId ('find' value) into m_tag
         declare
            index           :        Integer := 0; -- 1;   -- island index
            collisionObject : access impact.d3.Object.item'Class;
         begin
            for i in 1 .. Integer (World.getCollisionObjectArray.Length)
            loop
               collisionObject := World.getCollisionObjectArray.Element (i);

               if not collisionObject.isStaticOrKinematicObject then
                  collisionObject.setIslandTag (Self.m_unionFind.find (index) - 0);

                  --  Set the correct object offset in Collision Object Array
                  --
                  Self.m_unionFind.getElement (index).m_sz := i - 1;
                  collisionObject.setCompanionId (-1);

                  index := index + 1;

               else
                  collisionObject.setIslandTag   (-1);
                  collisionObject.setCompanionId (-2);
               end if;
            end loop;
         end;


      else
         raise Program_Error; -- tbd: (port this below, if really needed).

         --          // put the islandId ('find' value) into m_tag
         --          {
         --
         --
         --                  int index = 0;
         --                  int i;
         --                  for (i=0;i<colWorld->getCollisionObjectArray().size();i++)
         --                  {
         --                          impact.d3.Object* collisionObject= colWorld->getCollisionObjectArray()[i];
         --                          if (!collisionObject->isStaticOrKinematicObject())
         --                          {
         --                                  collisionObject->setIslandTag( m_unionFind.find(index) );
         --                                  collisionObject->setCompanionId(-1);
         --                          } else
         --                          {
         --                                  collisionObject->setIslandTag(-1);
         --                                  collisionObject->setCompanionId(-2);
         --                          }
         --                          index++;
         --                  }
         --          }

      end if;
   end storeIslandActivationState;







   function  getSplitIslands (Self : in Item) return Boolean
   is
   begin
      return Self.m_splitIslands;
   end getSplitIslands;






   procedure setSplitIslands (Self : in out Item;   doSplitIslands : in Boolean)
   is
   begin
      Self.m_splitIslands := doSplitIslands;
   end setSplitIslands;







   --- Operations
   --


   function getIslandId (lhs : in impact.d3.Manifold.view) return Integer
   is
      islandId : Integer;

      rcolObj0 : constant impact.d3.Object.view := impact.d3.Object.view (lhs.getBody0);
      rcolObj1 : constant impact.d3.Object.view := impact.d3.Object.view (lhs.getBody1);

   begin
      if rcolObj0.getIslandTag >= 0 then
         islandId := rcolObj0.getIslandTag;
      else
         islandId := rcolObj1.getIslandTag;
      end if;


      return islandId;
   end getIslandId;






--  /// function object that routes calls to operator<
--  class impact.d3.ManifoldSortPredicate
--  {
--          public:
--
--                  SIMD_FORCE_INLINE bool operator() ( const impact.d3.Manifold* lhs, const impact.d3.Manifold* rhs )
--                  {
--                          return getIslandId(lhs) < getIslandId(rhs);
--                  }
--  };







   --  todo: this is random access, it can be walked 'cache friendly'!


   procedure buildAndProcessIslands (Self : access Item;   dispatcher : access impact.d3.Dispatcher.item'Class;
                                                           colWorld   : access impact.d3.Space.Item    'Class;
                                     callback   : access IslandCallback           'Class)
   is
      collisionObjects : constant access impact.d3.Object.Vector := colWorld.getCollisionObjectArray;

      endIslandIndex   : Integer := 1; -- or 2 ?
      startIslandIndex : Integer;
      numElem          : Integer;

   begin
      Self.buildIslands (dispatcher, colWorld);

      numElem := Self.getUnionFind.getNumElements;


      if not Self.m_splitIslands then
         declare
            manifold        : constant access impact.d3.Manifold.Vector := dispatcher.getInternalManifoldPointer;
            maxNumManifolds :        Integer                     := dispatcher.getNumManifolds;
         begin
            callback.ProcessIsland (collisionObjects,
--                                      collisionObjects.Length,
                                    manifold,
--                                      maxNumManifolds,
                                    -1);
         end;

      else
         declare
            --  Sort manifolds, based on islands
            --  Sort the vector using predicate and std::sort
            --  std::sort(islandmanifold.begin(), islandmanifold.end(), impact.d3.ManifoldSortPredicate);

            numManifolds : constant Integer := Integer (Self.m_islandmanifold.Length);


            --  now process all active islands (sets of manifolds for now)

            startManifoldIndex : Integer := 1;
            endManifoldIndex   : Integer := 2;

            islandId       : Integer;
            islandSleeping : Boolean;


            function "<" (L, R : in impact.d3.Manifold.View) return Boolean
            is
            begin
               return getIslandId (l) < getIslandId (r);
            end "<";

            package Sorter is new impact.d3.Manifold.Vectors.generic_Sorting ("<");


         begin
            --  we should do radix sort, it it much faster (O(n) instead of O (n log2(n))
            Sorter.sort (Self.m_islandmanifold);


            --  traverse the simulation islands, and call the solver, unless all objects are sleeping/deactivated
            startIslandIndex := 0;  -- or 1 ?

            while startIslandIndex < numElem        -- for ( startIslandIndex=0;   startIslandIndex<numElem;   startIslandIndex = endIslandIndex)
            loop
               islandId       := Self.getUnionFind.getElement (startIslandIndex).m_id;
               islandSleeping := True;
               endIslandIndex := startIslandIndex;

               while      endIslandIndex < numElem
                 and then Self.getUnionFind.getElement (endIslandIndex).m_id = islandId

--                 for (endIslandIndex = startIslandIndex;
--                      (endIslandIndex<numElem) && (getUnionFind().getElement(endIslandIndex).m_id == islandId);
--                      endIslandIndex++)
               loop
                  declare
                     i       : constant Integer                := Self.getUnionFind.getElement (endIslandIndex).m_sz;
                     colObj0 : constant impact.d3.Object.view := collisionObjects.Element (i + 1); -- or +0 ?
                  begin
                     Self.m_islandBodies.append (colObj0);

                     if colObj0.isActive then
                        islandSleeping := False;
                     end if;
                  end;

                  endIslandIndex := endIslandIndex + 1;
               end loop;


               --  Find the accompanying contact manifold for this islandId.
               --
               declare
                  numIslandManifolds : Integer := 0;
                  startManifold      : impact.d3.Manifold.Vector; -- := 0;
                  curIslandId        : Integer;

               begin
                  if startManifoldIndex < numManifolds then
                     curIslandId := getIslandId (Self.m_islandmanifold.Element (startManifoldIndex));

                     if curIslandId = islandId then
--                          startManifold    := Self.m_islandmanifold (startManifoldIndex)'access;
                        endManifoldIndex := startManifoldIndex + 1;

                        while      endManifoldIndex < numManifolds
                          and then islandId = getIslandId (Self.m_islandmanifold.Element (endManifoldIndex))
                        loop
                           endManifoldIndex := endManifoldIndex + 1;
                        end loop;

                        --  Process the actual simulation, only if not sleeping/deactivated
                        numIslandManifolds := endManifoldIndex - startManifoldIndex;
                     end if;

                  end if;


                  if not islandSleeping then
                     declare
                        the_Manifolds : aliased impact.d3.Manifold.Vector;
                     begin
                        if numManifolds > 0 then
                           for i in 1 .. numManifolds   --  startManifoldIndex .. endManifoldIndex - 1  -- tbd: ?
                           loop
                              the_Manifolds.append (Self.m_islandmanifold.Element (i));
                           end loop;
                        end if;

                        callback.ProcessIsland (Self.m_islandBodies'Access,
--                                               Self.m_islandBodies.Length,
                                                the_Manifolds'Access, -- startManifold,
--                                               numIslandManifolds,
                                                islandId);
                     end;
                  end if;


                  if numIslandManifolds /= 0 then
                     startManifoldIndex := endManifoldIndex;
                  end if;

                  Self.m_islandBodies.reserve_Capacity (0); -- resize (0);
                  Self.m_islandBodies.clear;

                  startIslandIndex := endIslandIndex;
               end;
            end loop;
         end;
      end if;

   end buildAndProcessIslands;








   procedure buildIslands (Self : access Item;   dispatcher : access impact.d3.Dispatcher.item'Class;
                                                 colWorld   : access impact.d3.Space.Item    'Class)
   is
      use impact.d3.Manifold.Vectors;

      collisionObjects : constant access impact.d3.Object.Vector := colWorld.getCollisionObjectArray;

      numElem,
      endIslandIndex,
      startIslandIndex : Integer;
   begin

      Self.m_islandmanifold.reserve_Capacity (0); -- resize(0);
      Self.m_islandmanifold.clear;                -- resize(0);

      --  we are going to sort the unionfind array, and store the element id in the size
      --  afterwards, we clean unionfind, to make sure no-one uses it anymore

      Self.getUnionFind.sortIslands;

      numElem        := Self.getUnionFind.getNumElements;
      endIslandIndex := 1;  -- or 2 ?


      --  update the sleeping state for bodies, if all are sleeping

      startIslandIndex := 0;  -- or 1 ?

      while startIslandIndex < numElem
      loop
         declare
            islandId    : constant Integer := Self.getUnionFind.getElement (startIslandIndex).m_id - 0;
            allSleeping : Boolean := True;
         begin
            endIslandIndex := startIslandIndex + 1;

            while      endIslandIndex < numElem
              and then Self.getUnionFind.getElement (endIslandIndex).m_id = islandId
            loop
               endIslandIndex := endIslandIndex + 1;
            end loop;

            --  int numSleeping = 0;


            for idx in startIslandIndex .. endIslandIndex - 1
            loop
               declare
                  i       : constant Integer                := Self.getUnionFind.getElement (idx).m_sz;
                  colObj0 : constant impact.d3.Object.view := collisionObjects.Element (i + 1);  -- or +0 ?
               begin
                  if         colObj0.getIslandTag  /= islandId
                    and then colObj0.getIslandTag  /= -1
                  then
                     raise Constraint_Error with "error in island management";
                  end if;

                  pragma Assert (      colObj0.getIslandTag = islandId
                                 or else colObj0.getIslandTag = -1);

                  if colObj0.getIslandTag = islandId then

                     if colObj0.getActivationState = impact.d3.Object.ACTIVE_TAG then
                        allSleeping := False;   -- tbd: add an 'exit' here for performance ?
                     end if;

                     if colObj0.getActivationState = impact.d3.Object.DISABLE_DEACTIVATION then
                        allSleeping := False;   -- tbd: add an 'exit' here for performance ?
                     end if;

                  end if;

               end;
            end loop;


            if allSleeping then
               for idx in startIslandIndex .. endIslandIndex - 1
               loop
                  declare
                     i       : constant Integer                := Self.getUnionFind.getElement (idx).m_sz;
                     colObj0 : constant impact.d3.Object.view := collisionObjects.Element (i + 1);   -- or +0 ?
                  begin
                     if         colObj0.getIslandTag /= islandId
                       and then colObj0.getIslandTag /= -1
                     then
                        raise Constraint_Error with "error in island management";
                     end if;

                     pragma Assert (      colObj0.getIslandTag = islandId
                                    or else colObj0.getIslandTag = -1);

                     if colObj0.getIslandTag = islandId then
                        colObj0.setActivationState (impact.d3.Object.ISLAND_SLEEPING);
                     end if;
                  end;
               end loop;


            else
               for idx in startIslandIndex .. endIslandIndex - 1
               loop
                  declare
                     i       : constant Integer               := Self.getUnionFind.getElement (idx).m_sz;
                     colObj0 : constant impact.d3.Object.view := collisionObjects.Element (i + 1);   -- or +0 ?
                  begin
                     if         colObj0.getIslandTag /= islandId
                       and then colObj0.getIslandTag /= -1
                     then
                        raise Constraint_Error with "error in island management";
                     end if;

                     pragma Assert (      colObj0.getIslandTag = islandId
                                    or else colObj0.getIslandTag = -1);

                     if colObj0.getIslandTag = islandId then
                        if colObj0.getActivationState = impact.d3.Object.ISLAND_SLEEPING then
                           colObj0.setActivationState  (impact.d3.Object.WANTS_DEACTIVATION);
                           colObj0.setDeactivationTime (0.0);
                        end if;
                     end if;

                  end;
               end loop;
            end if;


            startIslandIndex := endIslandIndex;
         end;
      end loop;



      declare
         maxNumManifolds : constant Integer := dispatcher.getNumManifolds;
      begin
         for i in 1 .. maxNumManifolds
         loop
            declare
               use type impact.d3.Object.view;
               manifold : constant impact.d3.Manifold.view := dispatcher.getManifoldByIndexInternal (i);

               colObj0 : constant impact.d3.Object.view := impact.d3.Object.view (manifold.getBody0);
               colObj1 : constant impact.d3.Object.view := impact.d3.Object.view (manifold.getBody1);

            begin
               --  @todo: check sleeping conditions!
               if        (colObj0 /= null and then colObj0.getActivationState /= impact.d3.Object.ISLAND_SLEEPING)
                 or else (colObj1 /= null and then colObj1.getActivationState /= impact.d3.Object.ISLAND_SLEEPING)
               then
                  --  kinematic objects don't merge islands, but wake up all connected objects
                  --
                  if         colObj0.isKinematicObject
                    and then colObj0.getActivationState /= impact.d3.Object.ISLAND_SLEEPING
                  then
                     colObj1.activate;
                  end if;

                  if         colObj1.isKinematicObject
                    and then colObj1.getActivationState /= impact.d3.Object.ISLAND_SLEEPING
                  then
                     colObj0.activate;
                  end if;

                  if Self.m_splitIslands then
                     --  filtering for response
                     if dispatcher.needsResponse (colObj0, colObj1) then
                        Self.m_islandmanifold.append (manifold);
                     end if;
                  end if;

               end if;

            end;
         end loop;
      end;

   end buildIslands;



end impact.d3.simulation_island_Manager;
