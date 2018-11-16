with impact.d2.orbs.Collision,
     impact.d2.Math;



package impact.d2.orbs.Shape
--
--  Our only shape is a circle.
--
--  A shape is used for collision detection. You can create a shape however you like.
--  Shapes used for simulation in b2World are created automatically when a b2Fixture is created.
--
is
--     use impact.d2.Math;


   type Item is tagged
      record
         m_radius : float32;
      end record;

   type View is access all Item'Class;




   ----------
   --- Forge
   --

   function  to_Circle (Radius : in float_math.Real) return Shape.item;

   function  Clone     (Self : in     Item) return Shape.item;            -- Clone the concrete shape using the provided allocator.
   procedure destruct  (Self : in out Item) is null;





   ---------------
   --- Operations
   --


   --  Test a point for containment in this shape. This only works for convex shapes.
   --  'xf' the shape world transform.
   --  'p'  a point in world coordinates.
   --
   function test_Point (Self : in Item;   xf : in impact.d2.Math.b2Transform;
                                          p  : in impact.d2.Math.b2Vec2   ) return Boolean;



   --  Cast a ray against this shape.
   --  'output'    the ray-cast results.
   --  'input'     the ray-cast input parameters.
   --  'transform' the transform to be applied to the shape.
   --
   function ray_Cast (Self : in Item;   output    : access collision.b2RayCastOutput;
                                          input     : in     collision.b2RayCastInput;
                                          transform : in     impact.d2.Math.b2Transform            ) return Boolean;



   --  Given a transform, compute the associated axis aligned bounding box for this shape.
   --  'aabb' returns the axis aligned box.
   --  'xf'   the world transform of the shape.
   --
   procedure compute_AABB (Self : in Item;   aabb : access collision.b2AABB;
                                             xf   : in     impact.d2.Math.b2Transform);





   ---------
   --- Mass
   --

   --  This holds the mass data computed for a shape.
   --
   type mass_Data is
      record
         mass   : float32;     -- The mass of the shape, usually in kilograms.
         center : impact.d2.Math.b2Vec2;      -- The position of the shapes centroid relative to the shapes origin.
         I      : float32;     -- The rotational inertia of the shape about the local origin.
      end record;


   --  Compute the mass properties of this shape using its dimensions and density.
   --
   --  The inertia tensor is computed about the local origin.
   --   'massData'   returns the mass data for this shape.
   --   'density'    the density in kilograms per meter squared.
   --
   procedure compute_Mass (Self : in Item;   massData : access mass_Data;
                                             density  : in     float32);






   ------------
   --- Support
   --

   function get_Support        (Self : in Item;   d : in impact.d2.Math.b2Vec2) return int32;      -- Get the supporting vertex index in the given direction.
   function get_support_Vertex (Self : in Item;   d : in impact.d2.Math.b2Vec2) return impact.d2.Math.b2Vec2;     -- Get the supporting vertex in the given direction.

   function get_vertex_Count   (Self : in Item                  ) return int32;      -- Get the vertex count.
   function get_Vertex         (Self : in Item;   index : in int32) return impact.d2.Math.b2Vec2;     -- Get a vertex by index. Used by b2Distance.





end impact.d2.orbs.Shape;
