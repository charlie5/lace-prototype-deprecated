with impact.d2.Shape,
     impact.d2.Math,

     interfaces.c.Pointers;


package impact.d2.Distance
--
--
--
is
   use impact.d2.Math;


   type b2Vec2_array is array (uint32 range <>) of aliased b2Vec2;

   package b2Vec2_Pointers is new Interfaces.C.Pointers (uint32, b2Vec2,
                                                         b2Vec2_array,
                                                         b2Vec2'(0.0, 0.0));

   subtype b2Vec2_view is b2Vec2_Pointers.Pointer;


   --  A distance proxy is used by the GJK algorithm.
   --  It encapsulates any shape.
   --
   type b2DistanceProxy is tagged
      record
        m_vertices : b2Vec2_view;
        m_count    : int32   := 0;
        m_radius   : float32 := 0.0;
      end record;



   --          Initialize the proxy using the given shape. The shape
   --          must remain in scope while the proxy is in use.
   --
   procedure set (Self : in out b2DistanceProxy;   shape : impact.d2.Shape.view;
                                                   index : int32);


   --  Get the supporting vertex index in the given direction.
   --
   function  getSupport (Self : in b2DistanceProxy;   d : b2Vec2) return int32;


   --  Get the supporting vertex in the given direction.
   --
   function  getSupportVertex (Self : in b2DistanceProxy;   d : b2Vec2) return b2Vec2;


   --  Get the vertex count.
   --
   function  getVertexCount (Self : in b2DistanceProxy) return int32;


   --  Get a vertex by index. Used by b2Distance.
   --
   function  GetVertex (Self : in b2DistanceProxy;   index : int32) return b2Vec2;







   type cache_Indices is array (int32 range 1 .. 3) of uint8;

   --  Used to warm start b2Distance.
   --  Set count to zero on first call.
   --
   type b2SimplexCache is
      record
         metric : float32;                -- length or area
         count  : uint16;
         indexA : cache_Indices;        -- vertices on shape A
         indexB : cache_Indices;        -- vertices on shape B
      end record;




   --   Input for b2Distance.
   --   You have to option to use the shape radii
   --   in the computation. Even
   --
   type b2DistanceInput is
      record
         proxyA     : b2DistanceProxy;
         proxyB     : b2DistanceProxy;

         transformA : b2Transform;
         transformB : b2Transform;

         useRadii   : Boolean;
      end record;



   --  Output for b2Distance.
   --
   type b2DistanceOutput is
      record
         pointA     : aliased b2Vec2;        -- closest point on shapeA
         pointB     : aliased b2Vec2;        -- closest point on shapeB

         distance   : float32;
         iterations : int32;        -- number of GJK iterations used
      end record;



   --   Compute the closest points between two shapes. Supports any combination of:
   --   b2CircleShape, b2PolygonShape, b2EdgeShape. The simplex cache is input/output.
   --   On the first call set b2SimplexCache.count to zero.
   --
   procedure b2Distance (output : access b2DistanceOutput;
                         cache  : access b2SimplexCache;
                         input  : in     b2DistanceInput);


end impact.d2.Distance;
