limited
with impact.d3.collision.Algorithm;



package impact.d3.collision.Proxy
--
--  The impact.d3.collision.Proxy is the main class that can be used with the Bullet broadphases.
--
--  It stores collision shape type information, collision filter information and a client object, typically a impact.d3.Object or impact.d3.Object.rigid.
--
is
   use Math;



   type BroadphaseNativeTypes is (BOX_SHAPE_PROXYTYPE,                    -- polyhedral convex shapes
                                  TRIANGLE_SHAPE_PROXYTYPE,
                                  TETRAHEDRAL_SHAPE_PROXYTYPE,
                                  CONVEX_TRIANGLEMESH_SHAPE_PROXYTYPE,
                                  CONVEX_HULL_SHAPE_PROXYTYPE,
                                  CONVEX_POINT_CLOUD_SHAPE_PROXYTYPE,
                                  CUSTOM_POLYHEDRAL_SHAPE_TYPE,

                                  IMPLICIT_CONVEX_SHAPES_START_HERE,      -- implicit convex shapes
                                  SPHERE_SHAPE_PROXYTYPE,
                                  MULTI_SPHERE_SHAPE_PROXYTYPE,
                                  CAPSULE_SHAPE_PROXYTYPE,
                                  CONE_SHAPE_PROXYTYPE,
                                  CONVEX_SHAPE_PROXYTYPE,
                                  CYLINDER_SHAPE_PROXYTYPE,
                                  UNIFORM_SCALING_SHAPE_PROXYTYPE,
                                  MINKOWSKI_SUM_SHAPE_PROXYTYPE,
                                  MINKOWSKI_DIFFERENCE_SHAPE_PROXYTYPE,
                                  BOX_2D_SHAPE_PROXYTYPE,
                                  CONVEX_2D_SHAPE_PROXYTYPE,
                                  CUSTOM_CONVEX_SHAPE_TYPE,

                                  CONCAVE_SHAPES_START_HERE,                -- concave shapes

                                  -- keep all the convex shapetype below here, for the check IsConvexShape in broadphase proxy!

                                  TRIANGLE_MESH_SHAPE_PROXYTYPE,
                                  SCALED_TRIANGLE_MESH_SHAPE_PROXYTYPE,

                                  FAST_CONCAVE_MESH_PROXYTYPE,              -- used for demo integration FAST/Swift collision library and Bullet

                                  TERRAIN_SHAPE_PROXYTYPE,                  -- terrain

                                  GIMPACT_SHAPE_PROXYTYPE,                  -- Used for GIMPACT Trimesh integration

                                  MULTIMATERIAL_TRIANGLE_MESH_PROXYTYPE,    -- Multimaterial mesh

                                  EMPTY_SHAPE_PROXYTYPE,
                                  STATIC_PLANE_PROXYTYPE,
                                  CUSTOM_CONCAVE_SHAPE_TYPE,
                                  CONCAVE_SHAPES_END_HERE,

                                  COMPOUND_SHAPE_PROXYTYPE,

                                  SOFTBODY_SHAPE_PROXYTYPE,
                                  HFFLUID_SHAPE_PROXYTYPE,
                                  HFFLUID_BUOYANT_CONVEX_SHAPE_PROXYTYPE,
                                  INVALID_SHAPE_PROXYTYPE,

                                  MAX_BROADPHASE_COLLISION_TYPES);
   --
   --  impact.d3.Dispatcher uses these types
   --  Nb:   The types are ordered polyhedral, implicit convex and concave to facilitate type checking.
   --  CUSTOM_POLYHEDRAL_SHAPE_TYPE, CUSTOM_CONVEX_SHAPE_TYPE and CUSTOM_CONCAVE_SHAPE_TYPE can be used to extend Bullet without modifying source code.


   function isPolyhedral (proxyType : in BroadphaseNativeTypes) return Boolean;
   function isConvex     (proxyType : in BroadphaseNativeTypes) return Boolean;
   function isNonMoving  (proxyType : in BroadphaseNativeTypes) return Boolean;
   function isConcave    (proxyType : in BroadphaseNativeTypes) return Boolean;
   function isCompound   (proxyType : in BroadphaseNativeTypes) return Boolean;
   function isSoftBody   (proxyType : in BroadphaseNativeTypes) return Boolean;
   function isInfinite   (proxyType : in BroadphaseNativeTypes) return Boolean;
   function isConvex2d   (proxyType : in BroadphaseNativeTypes) return Boolean;




   --  optional filtering to cull potential collisions
   --
   type CollisionFilterGroups is mod 2**16;

   DefaultFilter   : constant CollisionFilterGroups :=  1;
   StaticFilter    : constant CollisionFilterGroups :=  2;
   KinematicFilter : constant CollisionFilterGroups :=  4;
   DebrisFilter    : constant CollisionFilterGroups :=  8;
   SensorTrigger   : constant CollisionFilterGroups := 16;
   CharacterFilter : constant CollisionFilterGroups := 32;
   AllFilter       : constant CollisionFilterGroups := -1;   -- all bits sets: DefaultFilter | StaticFilter | KinematicFilter | DebrisFilter | SensorTrigger | CharacterFilter




   type Item is new Any with
      record
         m_clientObject         : access Any'Class;       -- Usually the client impact.d3.Object or Rigidbody class
         m_collisionFilterGroup : CollisionFilterGroups;
         m_collisionFilterMask  : CollisionFilterGroups;
         m_multiSapParentProxy  : access Any'Class;
         m_uniqueId             : Integer;                -- m_uniqueId is introduced for paircache. could get rid of this, by calculating the address offset etc.

         m_aabbMin              : math.Vector_3;
         m_aabbMax              : math.Vector_3;
      end record;




   package Forge
   is
      function to_Proxy (aabbMin, aabbMax     : in     math.Vector_3;
                                     userPtr              : access Any'Class;
                                     collisionFilterGroup : in     CollisionFilterGroups;
                                     collisionFilterMask  : in     CollisionFilterGroups;
                                     multiSapParentProxy  : access Any'Class            := null) return impact.d3.collision.Proxy.item;
   end Forge;




   function getUid (Self : in impact.d3.collision.Proxy.item) return Integer;




--     type internal_Kind is (Info1, tmpValue);
--
--     type internal (Kind : internal_Kind := Info1) is
--        record
--           case Kind is when Info1    =>    m_internalInfo1    : access Any'Class;
--                        when tmpValue =>    m_internalTmpValue : Integer;
--           end case;
--        end record;

   type internal is
      record
         m_internalInfo1    : access Any'Class;
         m_internalTmpValue : Integer;
      end record;


   --  The btBroadphasePair class contains a pair of aabb-overlapping objects.
   --  A impact.d3.Dispatcher can search a impact.d3.collision.Algorithm that performs exact/narrowphase collision detection on the actual collision shapes.
   --
   type btBroadphasePair is
      record
         m_pProxy0   : access Item'Class;
         m_pProxy1   : access Item'Class;

         m_algorithm : access impact.d3.collision.Algorithm.item'Class;

         internals   : internal;     -- don't use this data, it will be removed in future version.
      end record;

   type btBroadphasePair_view is access all btBroadphasePair;


   function to_btBroadphasePair                                             return btBroadphasePair;
   function to_btBroadphasePair (other          : in     btBroadphasePair) return btBroadphasePair;
   function to_btBroadphasePair (proxy0, proxy1 : access Item'Class)        return btBroadphasePair;

   function btBroadphasePairSortPredicate (a, b : in btBroadphasePair) return Boolean;
   overriding function "="                           (a, b : in btBroadphasePair) return Boolean;


end impact.d3.collision.Proxy;





--  class btBroadphasePairSortPredicate
--  {
--          public:
--
--                  bool operator() ( const btBroadphasePair& a, const btBroadphasePair& b )
--                  {
--                          const int uidA0 = a.m_pProxy0 ? a.m_pProxy0->m_uniqueId : -1;
--                          const int uidB0 = b.m_pProxy0 ? b.m_pProxy0->m_uniqueId : -1;
--                          const int uidA1 = a.m_pProxy1 ? a.m_pProxy1->m_uniqueId : -1;
--                          const int uidB1 = b.m_pProxy1 ? b.m_pProxy1->m_uniqueId : -1;
--
--                           return uidA0 > uidB0 ||
--                                  (a.m_pProxy0 == b.m_pProxy0 && uidA1 > uidB1) ||
--                                  (a.m_pProxy0 == b.m_pProxy0 && a.m_pProxy1 == b.m_pProxy1 && a.m_algorithm > b.m_algorithm);
--                  }
--  };




--  SIMD_FORCE_INLINE bool operator==(const btBroadphasePair& a, const btBroadphasePair& b)
--  {
--           return (a.m_pProxy0 == b.m_pProxy0) && (a.m_pProxy1 == b.m_pProxy1);
--  }

