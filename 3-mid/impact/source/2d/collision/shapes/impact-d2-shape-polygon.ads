package impact.d2.Shape.polygon
--
--  A convex polygon. It is assumed that the interior of the polygon is to the left of each edge.
--
is

   type b2PolygonShape is new b2Shape with
      record
         m_centroid    :         b2Vec2;
         m_vertices    : aliased b2Vec2_array (1 .. b2_maxPolygonVertices);
         m_normals     :         b2Vec2_array (1 .. b2_maxPolygonVertices);
         m_vertexCount :         int32;
      end record;

   type View is access all b2PolygonShape'Class;



   function to_Polygon return b2PolygonShape;

   overriding
   procedure destruct    (Self : in out b2PolygonShape) is null;


   overriding
   function Clone        (Self : in b2PolygonShape) return Shape.view;

   overriding
   function TestPoint    (Self : in b2PolygonShape;  transform : in b2Transform;
                                                     p         : in b2Vec2   ) return Boolean;

   overriding
   function RayCast      (Self : in b2PolygonShape;   output    : access collision.b2RayCastOutput;
                                                     input     : in     collision.b2RayCastInput;
                                                     transform : in     b2Transform            ) return Boolean;

   overriding
   procedure ComputeAABB (Self : in b2PolygonShape;   aabb      : access collision.b2AABB;
                                                     transform : in     b2Transform);

   overriding
   procedure ComputeMass (Self : in b2PolygonShape;   massData : access b2MassData;
                                                     density  : in     float32);


   function GetSupport       (Self : in b2PolygonShape;   d : in b2Vec2) return int32;      -- Get the supporting vertex index in the given direction.
   function GetSupportVertex (Self : in b2PolygonShape;   d : in b2Vec2) return b2Vec2;     -- Get the supporting vertex in the given direction.

   function GetVertexCount   (Self : in b2PolygonShape                  ) return int32;      -- Get the vertex count.
   function GetVertex        (Self : in b2PolygonShape;   index : in int32) return b2Vec2;     -- Get a vertex by index. Used by b2Distance.



   --  Copy vertices. This assumes the vertices define a convex polygon.
   --  It is assumed that the exterior is the the right of each edge.
   --
   procedure set (Self : in out b2PolygonShape;   vertices : in b2Vec2_array);
   --        void Set(const b2Vec2* vertices, int32 vertexCount);



   --  Build vertices to represent an axis-aligned box.
   --  'hx' the half-width.
   --  'hy' the half-height.
   --
   procedure setAsBox (Self : in out b2PolygonShape;   hx, hy : in  float32);


   --  Build vertices to represent an oriented box.
   --  'hx'      the half-width.
   --  'hy'     the half-height.
   --  'center' the center of the box in local coordinates.
   --  'angle'  the rotation of the box in local coordinates.
   --
   procedure setAsBox (Self : in out b2PolygonShape;   hx, hy : in float32;
                                                       center : in b2Vec2;
                                                       angle  : in float32);


   --  Set Self as a single edge.
   --
   procedure setAsEdge (Self : in out b2PolygonShape;   v1, v2 : in b2Vec2);


end impact.d2.Shape.polygon;
