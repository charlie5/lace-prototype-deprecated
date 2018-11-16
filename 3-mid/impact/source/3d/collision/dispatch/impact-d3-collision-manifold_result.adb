with impact.d3.Transform;
with impact.d3.Containers;



package body impact.d3.collision.manifold_Result
is



   package body Forge
   is


      function  to_manifold_Result (body0, body1 : access Object.item'Class) return Item
      is
         Self : manifold_Result.item;
      begin
         Self.m_body0 := body0;
         Self.m_body1 := body1;

         Self.m_rootTransA := body0.getWorldTransform.all;
         Self.m_rootTransB := body1.getWorldTransform.all;

         return Self;
      end to_manifold_Result;

   end Forge;








   procedure setPersistentManifold (Self : in out Item;   manifoldPtr : access impact.d3.Manifold.item'Class)
   is
   begin
      self.m_manifoldPtr := manifoldPtr;
   end setPersistentManifold;



   function  getPersistentManifold (Self : in     Item)          return access impact.d3.Manifold.item'Class
   is
   begin
      return Self.m_manifoldPtr;
   end getPersistentManifold;





   overriding procedure setShapeIdentifiersA (Self : in out Item;   partId0 : in Integer;
                                   index0  : in Integer)
   is
   begin
      Self.m_partId0 := partId0;
      Self.m_index0  := index0;
   end setShapeIdentifiersA;



   overriding procedure setShapeIdentifiersB (Self : in out Item;   partId1 : in Integer;
                                   index1  : in Integer)
   is
   begin
      Self.m_partId1 := partId1;
      Self.m_index1  := index1;
   end setShapeIdentifiersB;




   use type impact.d3.Containers.Any_view;





   procedure refreshContactPoints (Self : in out Item)
   is
      isSwapped : Boolean;
   begin
      pragma Assert (Self.m_manifoldPtr /= null);


      if Self.m_manifoldPtr.getNumContacts = 0 then
         return;
      end if;


      isSwapped := Self.m_manifoldPtr.getBody0 /= impact.d3.Containers.Any_view (Self.m_body0);

      if isSwapped then
         Self.m_manifoldPtr.refreshContactPoints (Self.m_rootTransB,  Self.m_rootTransA);
      else
         Self.m_manifoldPtr.refreshContactPoints (Self.m_rootTransA,  Self.m_rootTransB);
      end if;

   end refreshContactPoints;




   function getBody0Internal (Self : in Item) return access Object.item'Class
   is
   begin
      return Self.m_body0;
   end getBody0Internal;



   function getBody1Internal (Self : in Item) return access Object.item'Class
   is
   begin
      return Self.m_body1;
   end getBody1Internal;








   --  User can override this material combiner by implementing gContactAddedCallback and setting body0->m_collisionFlags |= impact.d3.Object::customMaterialCallback;
   --

   function calculateCombinedFriction (body0, body1 : access impact.d3.Object.item'Class) return math.Real
   is
      friction     : math.Real := body0.getFriction * body1.getFriction;
      MAX_FRICTION : constant  := 10.0;
   begin
      if friction < -MAX_FRICTION then
         friction := -MAX_FRICTION;
      end if;

      if friction > MAX_FRICTION then
         friction := MAX_FRICTION;
      end if;


      return friction;
   end calculateCombinedFriction;





   function calculateCombinedRestitution (body0, body1 : access impact.d3.Object.item'Class) return math.Real
   is
   begin
      return body0.getRestitution * body1.getRestitution;
   end calculateCombinedRestitution;







   overriding procedure addContactPoint (Self : in out Item;   normalOnBInWorld : in math.Vector_3;
                                                    pointInWorld     : in math.Vector_3;
                                                    depth            : in math.Real  )
   is
      use impact.d3.manifold_Point, impact.d3.Transform, math.Vectors;

      isSwapped   : Boolean;

      pointA,
      localA,
      localB      : math.Vector_3;

      newPt       : impact.d3.manifold_Point.item;
      insertIndex : Integer;

      unused      : Boolean;
      pragma Unreferenced (unused);
   begin
      pragma Assert (Self.m_manifoldPtr /= null);

      --  order in manifold needs to match

      --        if (depth > m_manifoldPtr->getContactProcessingThreshold())
      if depth > Self.m_manifoldPtr.getContactBreakingThreshold then
         return;
      end if;


      isSwapped := Self.m_manifoldPtr.getBody0 /= impact.d3.Containers.Any_view (Self.m_body0);
      pointA    := pointInWorld + normalOnBInWorld * depth;


      if isSwapped then
         localA := invXform (Self.m_rootTransB, pointA);
         localB := invXform (Self.m_rootTransA, pointInWorld);
      else
         localA := invXform (Self.m_rootTransA, pointA);
         localB := invXform (Self.m_rootTransB, pointInWorld);
      end if;


      newPt := to_manifold_Point (localA, localB, normalOnBInWorld, depth);
      newPt.m_positionWorldOnA := pointA;
      newPt.m_positionWorldOnB := pointInWorld;

      insertIndex := Self.m_manifoldPtr.getCacheEntry (newPt);

      newPt.m_combinedFriction    := calculateCombinedFriction    (Self.m_body0, Self.m_body1);
      newPt.m_combinedRestitution := calculateCombinedRestitution (Self.m_body0, Self.m_body1);


      --  BP mod, store contact triangles.
      if isSwapped then
         newPt.m_partId0 := Self.m_partId1;
         newPt.m_partId1 := Self.m_partId0;
         newPt.m_index0  := Self.m_index1;
         newPt.m_index1  := Self.m_index0;
      else
         newPt.m_partId0 := Self.m_partId0;
         newPt.m_partId1 := Self.m_partId1;
         newPt.m_index0  := Self.m_index0;
         newPt.m_index1  := Self.m_index1;
      end if;


      --  printf("depth=%f\n",depth);
      --  todo, check this for any side effects
      if insertIndex >= 0 then
         --  const impact.d3.manifold_Point& oldPoint = m_manifoldPtr->getContactPoint(insertIndex);
         Self.m_manifoldPtr.replaceContactPoint (newPt, insertIndex);
      else
         insertIndex := Self.m_manifoldPtr.addManifoldPoint (newPt);
      end if;


      --  User can override friction and/or restitution
      if         gContactAddedCallback /= null
        and then (      (Self.m_body0.getCollisionFlags and impact.d3.Object.CF_CUSTOM_MATERIAL_CALLBACK) /= 0     -- and if either of the two bodies requires custom material
                  or else (Self.m_body1.getCollisionFlags and impact.d3.Object.CF_CUSTOM_MATERIAL_CALLBACK) /= 0)
      then
         --  experimental feature info, for per-triangle material etc.
         declare
            obj0,
            obj1 : access impact.d3.Object.item'Class;
         begin
            if isSwapped then
               obj0 := Self.m_body1;
               obj1 := Self.m_body0;
            else
               obj0 := Self.m_body0;
               obj1 := Self.m_body1;
            end if;

            unused := gContactAddedCallback (Self.m_manifoldPtr.getContactPoint (insertIndex),
                                             obj0.all,
                                             newPt.m_partId0,
                                             newPt.m_index0,
                                             obj1.all,
                                             newPt.m_partId1,
                                             newPt.m_index1);
         end;
      end if;
   end addContactPoint;


end impact.d3.collision.manifold_Result;
