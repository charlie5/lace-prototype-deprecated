with impact.d2.orbs.Collision,
     impact.d2.orbs.Contact,
     impact.d2.orbs.Solid,
     impact.d2.Math;


package impact.d2.orbs.toi_Solver
--
--
--
is
   use impact.d2.Math;


--
--  class b2Contact;
--  class b2Body;
--  struct b2TOIConstraint;
--  class b2StackAllocator;



   --  This is a pure position solver for a single movable body in contact with
   --  multiple non-moving bodies.
   --
   type b2TOISolver is tagged private;

   procedure destruct (Self : in out b2TOISolver);


   procedure Initialize (Self : in out b2TOISolver;   contacts : in     Contact.views;
                                                      toiBody  : access Solid.item'Class);

   procedure Clear (Self : in out b2TOISolver);


   --  Perform one solver iteration. Returns true if converged.
   --
   function  Solve (Self : in b2TOISolver;   baumgarte : float32) return Boolean;




private

   type Solid_view is access all Solid.item'Class;



   type b2TOIConstraint is
      record
         localPoints : b2Vec2_array (1 .. b2_maxManifoldPoints);
         localNormal : b2Vec2;
         localPoint  : b2Vec2;
         kind        : collision.b2Manifold_Kind;
         radius      : float32;
         pointCount  : int32;
         bodyA       : Solid_view;
         bodyB       : Solid_view;
      end record;

   type b2TOIConstraints      is array (int32 range <>) of aliased b2TOIConstraint;
   type b2TOIConstraints_view is access all b2TOIConstraints;




   type b2TOISolver is tagged
      record
        m_constraints :        b2TOIConstraints_view;
        m_count       :        int32             := 0;
        m_toiBody     : access Solid.item'Class;
      end record;



--  class b2TOISolver
--  {
--  public:
--          ~b2TOISolver();
--
--          void Initialize(b2Contact** contacts, int32 contactCount, b2Body* toiBody);
--          void Clear();
--
--          // Perform one solver iteration. Returns true if converged.
--          bool Solve(float32 baumgarte);
--
--  private:
--
--          b2TOIConstraint* m_constraints;
--          int32 m_count;
--          b2Body* m_toiBody;
--          b2StackAllocator* m_allocator;
--  };
--
--  #endif


end impact.d2.orbs.toi_Solver;
