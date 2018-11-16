
package impact.d2.Shape.circle
--
--
--
is

   type b2CircleShape is new b2Shape with
      record
          m_p : aliased b2Vec2;      -- Position
      end record;

   type View is access all b2CircleShape'Class;


   function to_Circle return b2CircleShape;

   overriding procedure destruct    (Self : in out b2CircleShape) is null;

   overriding function Clone        (Self : in b2CircleShape) return Shape.view;

   overriding function TestPoint    (Self : in b2CircleShape;   transform : in b2Transform;
                                                     p         : in b2Vec2   ) return Boolean;

   overriding function RayCast      (Self : in b2CircleShape;   output    : access collision.b2RayCastOutput;
                                                     input     : in     collision.b2RayCastInput;
                                                     transform : in     b2Transform            ) return Boolean;

   overriding procedure ComputeAABB (Self : in b2CircleShape;   aabb      : access collision.b2AABB;
                                                     transform : in     b2Transform);

   overriding procedure ComputeMass (Self : in b2CircleShape;   massData : access b2MassData;
                                                     density  : in     float32);


   function GetSupport       (Self : in b2CircleShape;   d : in b2Vec2) return int32;      -- Get the supporting vertex index in the given direction.
   function GetSupportVertex (Self : in b2CircleShape;   d : in b2Vec2) return b2Vec2;     -- Get the supporting vertex in the given direction.

   function GetVertexCount   (Self : in b2CircleShape                  ) return int32;      -- Get the vertex count.
   function GetVertex        (Self : in b2CircleShape;   index : in int32) return b2Vec2;     -- Get a vertex by index. Used by b2Distance.





end impact.d2.Shape.circle;
