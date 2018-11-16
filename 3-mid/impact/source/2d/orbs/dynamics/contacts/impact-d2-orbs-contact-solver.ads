with impact.d2.orbs.Solid;



package impact.d2.orbs.contact.Solver
--
--
--
is

   type Solid_view    is access all Solid.item'Class;
   type Manifold_view is access all collision.b2Manifold;



   type b2ContactConstraintPoint is
      record
         localPoint    : b2Vec2;
         rA, rB        : b2Vec2;

         normalImpulse,
         tangentImpulse,
         normalMass,
         tangentMass,
         velocityBias  : float32;
      end record;

   type b2ContactConstraintPoints is array (uint32 range 1 .. b2_maxManifoldPoints) of b2ContactConstraintPoint;



   type b2ContactConstraint is
      record
         points      : b2ContactConstraintPoints;
         localNormal : b2Vec2;
         localPoint  : b2Vec2;
         normal      : b2Vec2;
         normalMass  : b2Mat22;
         K           : b2Mat22;

         bodyA,
         bodyB       : Solid_view;

         kind        : collision.b2Manifold_Kind;
         radius      : float32;
         friction    : float32;
         pointCount  : int32;

         manifold    : Manifold_view;
      end record;

   type b2ContactConstraints      is array (int32 range <>) of aliased b2ContactConstraint;
   type b2ContactConstraints_view is access all b2ContactConstraints;




   type b2ContactSolver is tagged
      record
         m_constraints     : b2ContactConstraints_view;
         m_constraintCount : int32;
         --          b2StackAllocator* m_allocator;
      end record;


   function to_b2ContactSolver (contacts : in Contact.views;
                                --                                          b2StackAllocator* allocator,
                                impulseRatio : float32) return b2ContactSolver;

   procedure destruct (Self : in out b2ContactSolver);


   procedure WarmStart                (Self : in out b2ContactSolver);
   procedure SolveVelocityConstraints (Self : in out b2ContactSolver);
   procedure StoreImpulses            (Self : in out b2ContactSolver);

   function SolvePositionConstraints  (Self : in     b2ContactSolver;   baumgarte : float32) return Boolean;



end impact.d2.orbs.contact.Solver;
