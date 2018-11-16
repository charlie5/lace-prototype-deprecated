with impact.d3.Vector;



package body impact.d3.Manifold
is





--     --- Containers
--     --
--
--     function Length (Self : in Vector) return ada.Containers.Count_Type
--     is
--     begin
--        return vectors.Vector (Self).Length;
--     end;
--
--
--
--
--     procedure append (Self : in out Vector;   New_Item  : impact.d3.Manifold.Item'Class;
--                                               Count     : Ada.Containers.Count_Type := 1)
--     is
--        use Vectors;
--     begin
--        append (vectors.Vector (Self),  impact.d3.Manifold.Item (new_item),  Count);
--     end;






   --- Forge
   --

   function to_Manifold (body0,
                                     body1                      : in     Containers.Any_view;
                                     unused                     : in     Integer;
                                     contactBreakingThreshold   : in     Real;
                         contactProcessingThreshold : in     Real) return Item
   is
      pragma Unreferenced (unused);
      Self : impact.d3.Manifold.item;
   begin
      Self.m_body0        := body0;
      Self.m_body1        := body1;
      Self.m_cachedPoints := 0;

      Self.m_contactBreakingThreshold   := contactBreakingThreshold;
      Self.m_contactProcessingThreshold := contactProcessingThreshold;


      return Self;
   end to_Manifold;




   procedure destruct (Self : in out Item)
   is
   begin
      null;
   end destruct;










   function validContactDistance (Self : in Item;   pt : in impact.d3.manifold_Point.item'Class) return Boolean
   is
   begin
      return pt.m_distance1 <= Self.getContactBreakingThreshold;
   end validContactDistance;





   function  getBody0 (Self : in Item) return  Containers.Any_view
   is
   begin
      return Self.m_body0;
   end getBody0;



   function  getBody1 (Self : in Item) return  Containers.Any_view
   is
   begin
      return Self.m_body1;
   end getBody1;




   procedure setBodies (Self : in out Item;   body0,
                        body1 : in Containers.Any_view)
   is
   begin
      Self.m_body0 := body0;
      Self.m_body1 := body1;
   end setBodies;



   use type impact.d3.Containers.Any_view;


   procedure clearUserCache (Self : in out Item;   pt : access manifold_Point.item'Class)
   is
      pragma Unreferenced (Self);
      oldPtr : constant impact.d3.Containers.Any_view := pt.m_userPersistentData;
      unused : Boolean;
      pragma Unreferenced (unused);
   begin
      if oldPtr /= null then
         if         pt.m_userPersistentData   /= null
           and then gContactDestroyedCallback /= null
         then
            unused := gContactDestroyedCallback (pt.m_userPersistentData);
            pt.m_userPersistentData := null;
         end if;
      end if;
   end clearUserCache;





   function  getNumContacts (Self : in Item) return Integer
   is
   begin
      return Self.m_cachedPoints;
   end getNumContacts;




   function  getContactPoint (Self : access Item;   index : in Integer) return access manifold_Point.item'Class
   is
   begin
      pragma Assert (index <= Self.m_cachedPoints);

      return Self.m_pointCache (index)'Access;
   end getContactPoint;




   function  getContactPoint (Self : in     Item;   index : in Integer) return        manifold_Point.item'Class
   is
   begin
      pragma Assert (index <= Self.m_cachedPoints);

      return Self.m_pointCache (index);
   end getContactPoint;


   function  getContactBreakingThreshold   (Self : in Item) return Real
   is
   begin
      return Self.m_contactBreakingThreshold;
   end getContactBreakingThreshold;



   function  getContactProcessingThreshold (Self : in Item) return Real
   is
   begin
      return Self.m_contactProcessingThreshold;
   end getContactProcessingThreshold;




   function  getCacheEntry (Self : in Item;   newPoint : in manifold_Point.item'Class) return Integer
   is
      use impact.d3.Vector;

      shortestDist : Real  := Self.getContactBreakingThreshold * Self.getContactBreakingThreshold;
      size         : constant Integer := Self.getNumContacts;
      nearestPoint : Integer := -1;

      mp : access constant impact.d3.manifold_Point.item;

      diffA           : Vector_3;
      distToManiPoint : Real;
   begin
      for i in 1 .. size loop
         mp              := Self.m_pointCache (i)'Access;

         diffA           := mp.m_localPointA - newPoint.m_localPointA;
         distToManiPoint := dot (diffA, diffA);

         if distToManiPoint < shortestDist then
            shortestDist := distToManiPoint;
            nearestPoint := i;
         end if;
      end loop;

      return nearestPoint;
   end getCacheEntry;






   function  addManifoldPoint (Self : access Item;   newPoint : in manifold_Point.item) return Integer
   is
      pragma Assert (validContactDistance (Self.all, newPoint));

      insertIndex : Integer := Self.getNumContacts + 1;
   begin
      if insertIndex > MANIFOLD_CACHE_SIZE then

         if MANIFOLD_CACHE_SIZE >= 4 then
            insertIndex := sortCachedPoints (Self.all, newPoint);  -- sort cache so best points come first, based on area
         else
            insertIndex := 1;
         end if;

         clearUserCache (Self.all, Self.m_pointCache (insertIndex)'Access);

      else
         Self.m_cachedPoints := Self.m_cachedPoints + 1;
      end if;

      if insertIndex < 1 then
         insertIndex := 1;
      end if;


      pragma Assert (Self.m_pointCache (insertIndex).m_userPersistentData  =  null);

      Self.m_pointCache (insertIndex) := newPoint;

      return insertIndex;
   end addManifoldPoint;





   procedure removeContactPoint  (Self : in out Item;   index       : in Integer)
   is
      lastUsedIndex : Integer;
   begin
      clearUserCache (Self,  Self.m_pointCache (index)'Access);

      lastUsedIndex := Self.getNumContacts;
      --                m_pointCache[index] = m_pointCache[lastUsedIndex];

      if index /= lastUsedIndex then
         Self.m_pointCache (index) := Self.m_pointCache (lastUsedIndex);

         --  get rid of duplicated userPersistentData pointer
         Self.m_pointCache (lastUsedIndex).m_userPersistentData              := null;
         Self.m_pointCache (lastUsedIndex).mConstraintRow (1).m_accumImpulse := 0.0;
         Self.m_pointCache (lastUsedIndex).mConstraintRow (2).m_accumImpulse := 0.0;
         Self.m_pointCache (lastUsedIndex).mConstraintRow (3).m_accumImpulse := 0.0;

         Self.m_pointCache (lastUsedIndex).m_appliedImpulse             := 0.0;
         Self.m_pointCache (lastUsedIndex).m_lateralFrictionInitialized := False;
         Self.m_pointCache (lastUsedIndex).m_appliedImpulseLateral1     := 0.0;
         Self.m_pointCache (lastUsedIndex).m_appliedImpulseLateral2     := 0.0;
         Self.m_pointCache (lastUsedIndex).m_lifeTime                   := 0;
      end if;


      pragma Assert (Self.m_pointCache (lastUsedIndex).m_userPersistentData = null);

      Self.m_cachedPoints := Self.m_cachedPoints - 1;
   end removeContactPoint;



--     type Any_view is access all Any'Class;


   procedure replaceContactPoint (Self : in out Item;   newPoint    : in impact.d3.manifold_Point.item;
                                                                        insertIndex : in Integer)
   is
      MAINTAIN_PERSISTENCY : constant Boolean := True;
   begin
      pragma Assert (validContactDistance (Self, newPoint));

      if MAINTAIN_PERSISTENCY then
         declare
            lifeTime               : constant Integer := Self.m_pointCache (insertIndex).getLifeTime;                         pragma Assert (lifeTime >= 0);
            appliedImpulse         : constant Real  := Self.m_pointCache (insertIndex).mConstraintRow (1).m_accumImpulse;
            appliedLateralImpulse1 : constant Real  := Self.m_pointCache (insertIndex).mConstraintRow (2).m_accumImpulse;
            appliedLateralImpulse2 : constant Real  := Self.m_pointCache (insertIndex).mConstraintRow (3).m_accumImpulse;

            --                bool isLateralFrictionInitialized = m_pointCache[insertIndex].m_lateralFrictionInitialized;

            cache : constant impact.d3.Containers.Any_view := impact.d3.Containers.Any_view (Self.m_pointCache (insertIndex).m_userPersistentData); -- .all'access;
         begin
            Self.m_pointCache (insertIndex) := newPoint;

            Self.m_pointCache (insertIndex).m_userPersistentData := cache;
            Self.m_pointCache (insertIndex).m_appliedImpulse := appliedImpulse;
            Self.m_pointCache (insertIndex).m_appliedImpulseLateral1 := appliedLateralImpulse1;
            Self.m_pointCache (insertIndex).m_appliedImpulseLateral2 := appliedLateralImpulse2;

            Self.m_pointCache (insertIndex).mConstraintRow (1).m_accumImpulse :=  appliedImpulse;
            Self.m_pointCache (insertIndex).mConstraintRow (2).m_accumImpulse := appliedLateralImpulse1;
            Self.m_pointCache (insertIndex).mConstraintRow (3).m_accumImpulse := appliedLateralImpulse2;

            Self.m_pointCache (insertIndex).m_lifeTime := lifeTime;
         end;
      else
         Self.clearUserCache (Self.m_pointCache (insertIndex)'Access);
         Self.m_pointCache (insertIndex) := newPoint;
      end if;

   end replaceContactPoint;







   procedure refreshContactPoints (Self : in out Item;   trA, trB : in Transform_3d)
   is
      use impact.d3.Vector;

      distance2d          : Real;
      projectedDifference,
      projectedPoint      : Vector_3;
   begin
      --  first refresh worldspace positions and distance
      --
      for i in reverse 1 .. Self.getNumContacts loop
         declare
            use linear_Algebra_3d;
            manifoldPoint : impact.d3.manifold_Point.item renames Self.m_pointCache (i);
         begin
            manifoldPoint.m_positionWorldOnA := trA * manifoldPoint.m_localPointA;
            manifoldPoint.m_positionWorldOnB := trB * manifoldPoint.m_localPointB;
            manifoldPoint.m_distance1        := dot (manifoldPoint.m_positionWorldOnA - manifoldPoint.m_positionWorldOnB,  manifoldPoint.m_normalWorldOnB);
            manifoldPoint.m_lifeTime         := manifoldPoint.m_lifeTime + 1;
         end;
      end loop;

      --  ... then ...
      --
      for i in reverse 1 .. Self.getNumContacts loop
         declare
            manifoldPoint : impact.d3.manifold_Point.item renames Self.m_pointCache (i);
            unused        : Boolean;
            pragma Unreferenced (unused);
         begin

            if not validContactDistance (Self, manifoldPoint) then   -- contact becomes invalid when signed distance exceeds margin (projected on contactnormal direction)
               Self.removeContactPoint (i);
            else                                                     -- contact also becomes invalid when relative movement orthogonal to normal exceeds margin
               projectedPoint      := manifoldPoint.m_positionWorldOnA  -  manifoldPoint.m_normalWorldOnB * manifoldPoint.m_distance1;
               projectedDifference := manifoldPoint.m_positionWorldOnB  -  projectedPoint;
               distance2d          := dot (projectedDifference, projectedDifference);

               if distance2d  >  Self.getContactBreakingThreshold * Self.getContactBreakingThreshold then
                  Self.removeContactPoint (i);
               else
                  if gContactProcessedCallback /= null then   -- contact point processed callback
                     unused := gContactProcessedCallback (manifoldPoint,  Self.m_body0, Self.m_body1);
                  end if;
               end if;

            end if;

         end;
      end loop;

   end refreshContactPoints;





   procedure clearManifold  (Self : in out Item)
   is
   begin
      for i in 1 .. Self.m_cachedPoints loop
         clearUserCache (Self, Self.m_pointCache (i)'Access);
      end loop;

      Self.m_cachedPoints := 0;
   end clearManifold;






   function sortCachedPoints (Self : in Item;   pt : in manifold_Point.item'Class) return Integer
   is
      KEEP_DEEPEST_POINT : constant Boolean := True;

      maxPenetrationIndex : Integer := -1;
      maxPenetration      : Real;

      res0, res1,
      res2, res3 : Real;

      a0, b0,
      a1, b1,
      a2, b2,
      a3, b3,
      cross : Vector_3;

   begin
      --  calculate 4 possible cases areas, and take biggest area
      --  also need to keep 'deepest'

      if KEEP_DEEPEST_POINT then
         maxPenetration := pt.getDistance;

         for i in 1 .. 4 loop
            if Self.m_pointCache (i).getDistance < maxPenetration then
               maxPenetrationIndex := i;
               maxPenetration      := Self.m_pointCache (i).getDistance;
            end if;
         end loop;
      end if;

      res0 := 0.0;
      res1 := 0.0;
      res2 := 0.0;
      res3 := 0.0;

      if maxPenetrationIndex /= 1 then
         a0    := pt.m_localPointA - Self.m_pointCache (2).m_localPointA;
         b0    := Self.m_pointCache (4).m_localPointA  -  Self.m_pointCache (3).m_localPointA;
         cross := impact.d3.Vector.cross (a0, b0);
         res0  := impact.d3.Vector.length2 (cross);
      end if;

      if maxPenetrationIndex /= 2 then
         a1    := pt.m_localPointA - Self.m_pointCache (1).m_localPointA;
         b1    := Self.m_pointCache (4).m_localPointA  -  Self.m_pointCache (3).m_localPointA;
         cross := impact.d3.Vector.cross (a1, b1);
         res1  := impact.d3.Vector.length2 (cross);
      end if;

      if maxPenetrationIndex /= 3 then
         a2    := pt.m_localPointA - Self.m_pointCache (1).m_localPointA;
         b2    := Self.m_pointCache (4).m_localPointA  -  Self.m_pointCache (2).m_localPointA;
         cross := impact.d3.Vector.cross (a2, b2);
         res2  := impact.d3.Vector.length2 (cross);
      end if;

      if maxPenetrationIndex /= 4 then
         a3    := pt.m_localPointA - Self.m_pointCache (1).m_localPointA;
         b3    := Self.m_pointCache (3).m_localPointA  -  Self.m_pointCache (2).m_localPointA;
         cross := impact.d3.Vector.cross (a3, b3);
         res3  := impact.d3.Vector.length2 (cross);
      end if;

      declare
         maxvec      : constant Vector_4 := (res0, res1, res2, res3);
         biggestarea : constant Integer  := impact.d3.Vector.closestAxis4 (maxvec);
      begin
         return biggestarea;
      end;

   end sortCachedPoints;





--     function findContactPoint (Self : in impact.d3.Manifold;   unUsed    : access impact.d3.manifold_Point.impact.d3.manifold_Point;
--                                                                  numUnused : in     Integer;
--                                                                  pt        : in     impact.d3.manifold_Point.impact.d3.manifold_Point) return Integer
--     is
--     begin
--
--     end;


end impact.d3.Manifold;
