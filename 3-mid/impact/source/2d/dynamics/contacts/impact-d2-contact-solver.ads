with
     impact.d2.Solid,
     impact.d2.Types;



package impact.d2.contact.Solver
--
--
--
is
   use Impact.d2.Types;


   type Solid_view    is access all Solid.b2Body'Class;
   type Manifold_view is access all collision.b2Manifold;



   type b2ContactPositionConstraint is
      record
          localPoints                : b2Vec2_array (1 .. b2_maxManifoldPoints);
          localNormal                : b2Vec2;
          localPoint                 : b2Vec2;
          indexA,       indexB       : int32;
          invMassA,     invMassB     : float32;
          localCenterA, localCenterB : b2Vec2;
          invIA,        invIB        : float32;
          Kind                       : collision.b2Manifold_Kind;
          radiusA,      radiusB      : float32;
          pointCount                 : int32;
      end record;

   type b2ContactPositionConstraints      is array (int32 range <>) of aliased b2ContactPositionConstraint;
   type b2ContactPositionConstraints_view is access all b2ContactPositionConstraints;



--     type b2ContactConstraintPoint is
   type b2VelocityConstraintPoint is
      record
--           localPoint    : b2Vec2;
         rA, rB        : b2Vec2;

         normalImpulse,
         tangentImpulse,
         normalMass,
         tangentMass,
         velocityBias  : float32;
      end record;

   type b2VelocityConstraintPoints is array (uint32 range 1 .. b2_maxManifoldPoints) of b2VelocityConstraintPoint;



--     type b2ContactConstraint is
   type b2ContactVelocityConstraint is
      record
         points       : b2VelocityConstraintPoints;
         normal       : b2Vec2;
         normalMass   : b2Mat22;
         K            : b2Mat22;

         indexA,
         indexB       : int32;

         invMassA,
         invMassB     : float32;

         invIA,
         invIB        : float32;

         friction,
         restitution,
         tangentSpeed : float32;

         pointCount,
         contactIndex : int32;
      end record;

   type b2ContactVelocityConstraints      is array (int32 range <>) of aliased b2ContactVelocityConstraint;
   type b2ContactVelocityConstraints_view is access all b2ContactVelocityConstraints;



   type b2ContactSolverDef is
      record
         step       :        b2TimeStep;

         contacts   : access Contact.views;
         count      :        int32;

         positions  : access Position_views;
         velocities : access Velocity_views;
      end record;





   type b2ContactSolver is tagged
      record
         step                :        b2TimeStep;

         positions           : access Position_views;
         velocities          : access Velocity_views;

         positionConstraints :        b2ContactPositionConstraints_view;
         velocityConstraints :        b2ContactVelocityConstraints_view;

         contacts            : access Contact.views;
         count               :        int32;
      end record;


   function  to_b2ContactSolver (def : in b2ContactSolverDef) return b2ContactSolver;

   procedure destruct (Self : in out b2ContactSolver);


   procedure WarmStart                (Self : in out b2ContactSolver);
   procedure SolveVelocityConstraints (Self : in out b2ContactSolver);
   procedure StoreImpulses            (Self : in out b2ContactSolver);

   function  SolvePositionConstraints  (Self : in     b2ContactSolver;   baumgarte : float32) return Boolean;



end impact.d2.contact.Solver;
