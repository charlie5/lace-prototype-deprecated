with impact.d3.collision.Algorithm,
     ada.Unchecked_Conversion,
     System;



package body impact.d3.collision.Proxy
is



   package body Forge
   is

      function to_Proxy (aabbMin, aabbMax     : in     math.Vector_3;
                         userPtr              : access Any'Class;
                         collisionFilterGroup : in     CollisionFilterGroups;
                         collisionFilterMask  : in     CollisionFilterGroups;
                         multiSapParentProxy  : access Any'Class            := null) return impact.d3.collision.Proxy.item
      is
         Self : Proxy.item;
      begin
         Self.m_clientObject := userPtr;
         Self.m_collisionFilterGroup := collisionFilterGroup;
         Self.m_collisionFilterMask  := collisionFilterMask;
         Self.m_aabbMin              := aabbMin;
         Self.m_aabbMax              := aabbMax;
         Self.m_multiSapParentProxy  := multiSapParentProxy;

         return Self;
      end to_Proxy;

   end Forge;






   function isPolyhedral (proxyType : in BroadphaseNativeTypes) return Boolean
   is
   begin
      return proxyType < IMPLICIT_CONVEX_SHAPES_START_HERE;
   end isPolyhedral;


   function isConvex     (proxyType : in BroadphaseNativeTypes) return Boolean
   is
   begin
      return proxyType < CONCAVE_SHAPES_START_HERE;
   end isConvex;





   function isNonMoving  (proxyType : in BroadphaseNativeTypes) return Boolean
   is
   begin
      return     isConcave (proxyType)
        and then not (proxyType = GIMPACT_SHAPE_PROXYTYPE);
   end isNonMoving;





   function isConcave    (proxyType : in BroadphaseNativeTypes) return Boolean
   is
   begin
      return     proxyType > CONCAVE_SHAPES_START_HERE
        and then proxyType < CONCAVE_SHAPES_END_HERE;
   end isConcave;








   function isCompound   (proxyType : in BroadphaseNativeTypes) return Boolean
   is
   begin
      return proxyType = COMPOUND_SHAPE_PROXYTYPE;
   end isCompound;





   function isSoftBody   (proxyType : in BroadphaseNativeTypes) return Boolean
   is
   begin
      return proxyType = SOFTBODY_SHAPE_PROXYTYPE;
   end isSoftBody;





   function isInfinite   (proxyType : in BroadphaseNativeTypes) return Boolean
   is
   begin
      return proxyType = STATIC_PLANE_PROXYTYPE;
   end isInfinite;





   function isConvex2d   (proxyType : in BroadphaseNativeTypes) return Boolean
   is
   begin
      return    proxyType = BOX_2D_SHAPE_PROXYTYPE
        or else proxyType = CONVEX_2D_SHAPE_PROXYTYPE;
   end isConvex2d;







   function getUid (Self : in impact.d3.collision.Proxy.item) return Integer
   is
   begin
      return Self.m_uniqueId;
   end getUid;








   --- btBroadphasePair
   --


   function to_btBroadphasePair return btBroadphasePair
   is
      Self : btBroadphasePair;
   begin
      Self.internals.m_internalInfo1 := null;

      return Self;
   end to_btBroadphasePair;





   function to_btBroadphasePair (other : in btBroadphasePair) return btBroadphasePair
   is
      Self : btBroadphasePair;
   begin
      Self := Other;

      return Self;
   end to_btBroadphasePair;





   function to_btBroadphasePair (proxy0, proxy1 : access Item'Class)        return btBroadphasePair
   is
      Self : btBroadphasePair;
   begin
      --  Keep them sorted, so the std::set operations work.
      --
      if proxy0.m_uniqueId < proxy1.m_uniqueId then
         Self.m_pProxy0 := proxy0;
         Self.m_pProxy1 := proxy1;
      else
         Self.m_pProxy0 := proxy1;
         Self.m_pProxy1 := proxy0;
      end if;

      Self.m_algorithm               := null;
      Self.internals.m_internalInfo1 := null;


      return Self;
   end to_btBroadphasePair;






   function btBroadphasePairSortPredicate (a, b : in btBroadphasePair) return Boolean
   is
      function to_long_long_Integer is new ada.Unchecked_Conversion (system.Address, Long_Long_Integer);
      uidA0, uidB0,
      uidA1, uidB1 : Integer;
   begin
      if a.m_pProxy0 /= null then   uidA0 := a.m_pProxy0.m_uniqueId;
      else   uidA0 := -1;
      end if;

      if b.m_pProxy0 /= null then   uidB0 := b.m_pProxy0.m_uniqueId;
      else   uidB0 := -1;
      end if;

      if a.m_pProxy1 /= null then   uidA1 := a.m_pProxy1.m_uniqueId;
      else   uidA1 := -1;
      end if;

      if b.m_pProxy1 /= null then   uidB1 := b.m_pProxy1.m_uniqueId;
      else   uidB1 := -1;
      end if;

      return    uidA0 > uidB0
        or else (a.m_pProxy0 = b.m_pProxy0 and then uidA1 > uidB1)
        or else (a.m_pProxy0 = b.m_pProxy0 and then a.m_pProxy1 = b.m_pProxy1 and then to_long_long_Integer (a.m_algorithm.all'Address) > to_long_long_Integer (b.m_algorithm.all'Address));
   end btBroadphasePairSortPredicate;





   overriding function "=" (a, b : in btBroadphasePair) return Boolean
   is
   begin
      return     a.m_pProxy0 = b.m_pProxy0
        and then a.m_pProxy1 = b.m_pProxy1;
   end;



end impact.d3.collision.Proxy;
