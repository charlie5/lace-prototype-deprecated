with impact.d2.Collision,
     impact.d2.Math;



package impact.d2.Shape
--
--
--
is
   use impact.d2.Math;

   --- This holds the mass data computed for a shape.
   --
   type b2MassData is
      record
         mass   : float32;     -- The mass of the shape, usually in kilograms.
         center : b2Vec2;      -- The position of the shape's centroid relative to the shape's origin.
         I      : float32;     -- The rotational inertia of the shape about the local origin.
      end record;


   --  A shape is used for collision detection. You can create a shape however you like.
   --  Shapes used for simulation in b2World are created automatically when a b2Fixture is created.
   --

   type Kind is  (e_unknown,  e_circle,  e_polygon,  e_typeCount);
   for  Kind use (e_unknown   => -1,
                  e_circle    =>  0,
                  e_polygon   =>  1,
                  e_typeCount =>  2);



   type b2Shape is abstract tagged
      record
         m_kind   : Kind   := e_unknown;
         m_radius : float32;
      end record;

   type View is access all b2Shape'Class;


   procedure destruct    (Self : in out b2Shape) is null;


   function Clone (Self : in b2Shape) return Shape.view            -- Clone the concrete shape using the provided allocator.
     is abstract;

   function getKind (Self : in b2Shape'Class) return shape.Kind;   -- Get the type of this shape. You can use this to down cast to the concrete shape.


   --  Test a point for containment in this shape. This only works for convex shapes.
   --  'xf' the shape world transform.
   --  'p'  a point in world coordinates.
   --
   function TestPoint (Self : in b2Shape;   xf : in b2Transform;
                                            p  : in b2Vec2   ) return Boolean
     is abstract;



   --  Cast a ray against this shape.
   --  'output'    the ray-cast results.
   --  'input'     the ray-cast input parameters.
   --  'transform' the transform to be applied to the shape.
   --
   function RayCast (Self : in b2Shape;   output    : access collision.b2RayCastOutput;
                                          input     : in     collision.b2RayCastInput;
                                          transform : in     b2Transform            ) return Boolean
     is abstract;



   --  Given a transform, compute the associated axis aligned bounding box for this shape.
   --  'aabb' returns the axis aligned box.
   --  'xf'   the world transform of the shape.
   --
   procedure ComputeAABB (Self : in b2Shape;   aabb : access collision.b2AABB;
                                               xf   : in     b2Transform)
     is abstract;



   --  Compute the mass properties of this shape using its dimensions and density.
   --  The inertia tensor is computed about the local origin.
   --  'massData' returns the mass data for this shape.
   --  'density' the density in kilograms per meter squared.
   --
   procedure ComputeMass (Self : in b2Shape;   massData : access b2MassData;
                                               density  : in     float32)
     is abstract;


end impact.d2.Shape;
