with impact.d2.Math;

limited
with impact.d2.Shape;


package impact.d2.Collision
--
--  Types and subprograms used for computing contact points, distance queries, and Time of Impact (TOI) queries.
--
is
   use impact.d2.Math;


--  class b2Shape;
--  class b2CircleShape;
--  class b2PolygonShape;

   b2_nullFeature : constant uint8 := uint8'Last;


   --- Contact ids to facilitate warm starting.
   --
   type b2ContactID_Features is
      record
         referenceEdge  : uint8;        -- The edge that defines the outward contact normal.
         incidentEdge   : uint8;        -- The edge most anti-parallel to the reference edge.
         incidentVertex : uint8;        -- The vertex (0 or 1) on the incident edge that was clipped.
         flip           : uint8;        -- A value of 1 indicates that the reference edge is on shape2.
      end record;


   type b2ContactID_Kind is (with_features, with_key);

   type b2ContactID (Kind : b2ContactID_Kind := with_features) is
      record
         case Kind is
            when with_features => Features : b2ContactID_Features;
            when with_key      => Key      : uint32;                   -- Used to quickly compare contact ids.
         end case;
      end record;
   pragma Unchecked_Union (b2ContactID);




   --   A manifold point is a contact point belonging to a contact
   --   manifold. It holds details related to the geometry and dynamics
   --   of the contact points.
   --   The local point usage depends on the manifold type:
   --   -e_circles: the local center of circleB
   --   -e_faceA: the local center of cirlceB or the clip point of polygonB
   --   -e_faceB: the clip point of polygonA
   --   This structure is stored across time steps, so we keep it small.
   --   Note: the impulses are used for internal caching and may not
   --   provide reliable contact forces, especially for high speed collisions.
   --
   type b2ManifoldPoint is
      record
         localPoint     : b2Vec2;        -- usage depends on manifold type
         normalImpulse  : float32;       -- the non-penetration impulse
         tangentImpulse : float32;       -- the friction impulse
         id             : b2ContactID;   -- uniquely identifies a contact point between two shapes
      end record;

   type manifold_Points is array (int32 range <>) of aliased b2ManifoldPoint;



   --   A manifold for two touching convex shapes.
   --
   --   Box2D supports multiple types of contact:
   --   - clip point versus plane with radius
   --   - point versus point with radius (circles)
   --
   --   The local point usage depends on the manifold type:
   --   -e_circles: the local center of circleA
   --   -e_faceA: the center of faceA
   --   -e_faceB: the center of faceB
   --
   --   Similarly the local normal usage:
   --   -e_circles: not used
   --   -e_faceA: the normal on polygonA
   --   -e_faceB: the normal on polygonB
   --
   --   We store contacts in this way so that position correction can
   --   account for movement, which is critical for continuous physics.
   --   All contact scenarios must be expressed in one of these types.
   --
   --   This structure is stored across time steps, so we keep it small.
   --

   type b2Manifold_Kind is (e_circles,  e_faceA,  e_faceB);

   type b2Manifold is
      record
         points      : manifold_Points (1 .. b2_maxManifoldPoints);        -- the points of contact
         localNormal : b2Vec2;                                                -- not use for Type::e_points
         localPoint  : b2Vec2;                                                -- usage depends on manifold type
         Kind        : b2Manifold_Kind;
         pointCount  : int32;                                           -- the number of manifold points
      end record;



   --   This is used to compute the current state of a contact manifold.
   --
   type b2WorldManifold is
      record
         normal : b2Vec2;                                        -- world vector pointing from A to B
         points : b2Vec2_array (1 .. b2_maxManifoldPoints);        -- world contact point (point of intersection)
      end record;

   --           Evaluate the manifold with supplied transforms. This assumes
   --           modest motion from the original state. This does not change the
   --           point count, impulses, etc. The radii must come from the shapes
   --           that generated the manifold.
   --
   procedure Initialize (Self : in out b2WorldManifold;   manifold : in b2Manifold;
                                                          xfA      : in b2Transform;   radiusA : in float32;
                                                          xfB      : in b2Transform;   radiusB : in float32);





   --   This is used for determining the state of contact points.
   --
   type b2PointState is (b2_nullState,                -- point does not exist
                         b2_addState,                -- point was added in the update
                         b2_persistState,        -- point persisted across the update
                         b2_removeState);        -- point was removed in the update

   type b2PointStates is array (int32 range 1 .. b2_maxManifoldPoints) of b2PointState;


   --   Compute the point states given two manifolds. The states pertain to the transition from manifold1
   --   to manifold2. So state1 is either persist or remove while state2 is either add or persist.
   --
   procedure b2GetPointStates (state1,    state2    : access          b2PointStates;
                               manifold1, manifold2 : access constant b2Manifold);








   ---   Used for computing contact manifolds.
   --
   type b2ClipVertex is
      record
         v  : b2Vec2;
         id : b2ContactID;
      end record;

   type b2ClipVertices is array (int32 range 1 .. 2) of b2ClipVertex;



   ---   Ray-cast input data. The ray extends from p1 to p1 + maxFraction * (p2 - p1).
   --
   type b2RayCastInput is
      record
         p1, p2      : b2Vec2;
         maxFraction : float32;
      end record;




   ---   Ray-cast output data. The ray hits at p1 + fraction * (p2 - p1), where p1 and p2 come from b2RayCastInput.
   --
   type b2RayCastOutput is
      record
         normal   : b2Vec2;
         fraction : float32;
      end record;





   ---   An axis aligned bounding box.
   --
   type b2AABB is tagged
      record
         lowerBound : b2Vec2;        -- the lower vertex
         upperBound : b2Vec2;        -- the upper vertex
      end record;


   function  IsValid    (Self : in     b2AABB) return Boolean;                         -- Verify that the bounds are sorted.

   function  getCenter    (Self : in     b2AABB) return b2Vec2;
   function  getExtents   (Self : in     b2AABB) return b2Vec2;                          --  Get the extents of the AABB (half-widths).
   function  getPerimeter (Self : in     b2AABB) return float32;                      -- Get the perimeter length

   procedure combine    (Self : in out b2AABB;   aabb1, aabb2 : in b2AABB);            -- Combine two AABBs into this one.

   function  Contains   (Self : in     b2AABB;   aabb : in b2AABB) return Boolean;     --  Does this aabb contain the provided AABB.

   function  rayCast    (Self : in     b2AABB;   output : access b2RayCastOutput;
                                                 input  : in     b2RayCastInput) return Boolean;

   function b2TestOverlap (a, b : in b2AABB) return Boolean;

   function b2TestOverlap (shapeA, shapeB : access Shape.b2Shape'Class;
                           xfA,    xfB    : in     b2Transform      ) return Boolean;


   --  Clipping for contact manifolds.
   --
   function b2ClipSegmentToLine (vOut   : access b2ClipVertices;
                                 vIn    : in     b2ClipVertices;
                                 normal : in     b2Vec2      ;
                                 offset : in     float32     ) return int32;


end impact.d2.Collision;
