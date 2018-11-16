with
     impact.d2.orbs.Fixture,
     impact.d2.orbs.Shape,

     ada.unchecked_Deallocation;



package body impact.d2.orbs.toi_Solver
is

   use type int32;


   procedure destruct (Self : in out b2TOISolver)
   is
   begin
      Self.clear;
   end destruct;



   procedure Initialize (Self : in out b2TOISolver;   contacts : in     Contact.views;
                                                      toiBody  : access Solid.item'Class)
   is
   begin
      Self.clear;

      Self.m_count   := contacts'Length;
      Self.m_toiBody := toiBody;

      Self.m_constraints := new b2TOIConstraints (1 .. Self.m_count);

      for i in 1 .. Self.m_count loop
         declare
            use impact.d2.orbs.Fixture;

            contact  : constant impact.d2.orbs.Contact.view := contacts (i);

            fixtureA : constant access item := contact.GetFixtureA;
            fixtureB : constant access item := contact.GetFixtureB;

            shapeA   : constant Shape.view := fixtureA.GetShape.all'Access;
            shapeB   : constant Shape.view := fixtureB.GetShape.all'Access;

            radiusA  : float32 := shapeA.m_radius;
            radiusB  : float32 := shapeB.m_radius;

            bodyA    : constant Solid_view := fixtureA.GetBody.all'Access;
            bodyB    : constant Solid_view := fixtureB.GetBody.all'Access;

            manifold : access collision.b2Manifold := contact.GetManifold;
            pragma Assert (manifold.pointCount > 0);

            constraint :        b2TOIConstraint          renames Self.m_constraints (i);
            cp         : access collision.b2ManifoldPoint;
         begin
            constraint.bodyA       := bodyA;
            constraint.bodyB       := bodyB;
            constraint.localNormal := manifold.localNormal;
            constraint.localPoint  := manifold.localPoint;
            constraint.kind        := manifold.Kind;
            constraint.pointCount  := manifold.pointCount;
            constraint.radius      := radiusA + radiusB;

            for j in 1 .. constraint.pointCount loop
               cp                         := manifold.points (j)'Access;
               constraint.localPoints (j) := cp.localPoint;
            end loop;
         end;
      end loop;

   end Initialize;








   procedure Clear (Self : in out b2TOISolver)
   is
      procedure free is new ada.unchecked_Deallocation (b2TOIConstraints, b2TOIConstraints_view);
   begin
      if Self.m_constraints /= null then
         free (Self.m_constraints);
      end if;
   end Clear;







   type b2TOISolverManifold is
      record
         normal     : b2Vec2;
         point      : b2Vec2;
         separation : float32;
      end record;



   procedure initialize (Self : in out b2TOISolverManifold;  cc    : access b2TOIConstraint;
                                                             index : in     int32        )
   is
      use Collision;
      pointA,
      pointB,
      planePoint,
      clipPoint : b2Vec2;
   begin
      pragma Assert (cc.pointCount > 0);

      case cc.Kind is
      when e_circles =>
         pointA := cc.bodyA.GetWorldPoint (cc.localPoint);
         pointB := cc.bodyB.GetWorldPoint (cc.localPoints (1));

         if b2DistanceSquared (pointA, pointB)  >  b2_epsilon * b2_epsilon then
            Self.normal := pointB - pointA;
            Normalize (Self.normal);
         else
            Self.normal := (1.0, 0.0);
         end if;

         Self.point      := 0.5 * (pointA + pointB);
         Self.separation := b2Dot (pointB - pointA, Self.normal) - cc.radius;

      when e_faceA =>
         Self.normal := cc.bodyA.GetWorldVector (cc.localNormal);
         planePoint  := cc.bodyA.GetWorldPoint (cc.localPoint);

         clipPoint       := cc.bodyB.GetWorldPoint (cc.localPoints (index));
         Self.separation := b2Dot (clipPoint - planePoint,  Self.normal)  -  cc.radius;
         Self.point      := clipPoint;

      when e_faceB =>
         Self.normal := cc.bodyB.GetWorldVector (cc.localNormal);
         planePoint  := cc.bodyB.GetWorldPoint (cc.localPoint);

         clipPoint       := cc.bodyA.GetWorldPoint (cc.localPoints (index));
         Self.separation := b2Dot (clipPoint - planePoint,  Self.normal)  -  cc.radius;
         Self.point      := clipPoint;

         --  Ensure normal points from A to B
         Self.normal := -Self.normal;
      end case;

   end initialize;




   --  Push out the toi body to provide clearance for further simulation.
   --
   function  Solve (Self : in b2TOISolver;   baumgarte : float32) return Boolean
   is
      minSeparation   : float32 := 0.0;

      c               : access b2TOIConstraint;

      bodyA,
      bodyB           : access Solid.item'Class;

      massA,    massB,
      invMassA, invIA,
      invMassB, invIB : float32;

      psm             : b2TOISolverManifold;
   begin

      for i in 1 .. Self.m_count loop
         c := Self.m_constraints (i)'Access;
         bodyA := c.bodyA;
         bodyB := c.bodyB;

         massA := bodyA.getMass;
         massB := bodyB.getMass;

         --  Only the TOI body should move.
         if bodyA = Self.m_toiBody then
            massB := 0.0;
         else
            massA := 0.0;
         end if;

         invMassA := massA * bodyA.m_invMass.all;
         invIA    := massA * bodyA.m_InvI.all;
         invMassB := massB * bodyB.m_invMass.all;
         invIB    := massB * bodyB.m_invI.all;

         --  Solve normal constraints
         for j in 1 .. c.pointCount loop
            Initialize (psm,  c, j);
            declare
               normal     : constant b2Vec2  := psm.normal;

               point      : constant b2Vec2  := psm.point;
               separation : constant float32 := psm.separation;

               rA         : constant b2Vec2 := point - bodyA.m_sweep.c;
               rB         : constant b2Vec2 := point - bodyB.m_sweep.c;

               --  Prevent large corrections and allow slop.
               C          : constant float32 := b2Clamp (baumgarte * (separation + b2_linearSlop),  -b2_maxLinearCorrection, 0.0);

               --  Compute the effective mass.
               rnA        : constant float32 := b2Cross (rA, normal);
               rnB        : constant float32 := b2Cross (rB, normal);
               K          : constant float32 := invMassA  +  invMassB  +  invIA * rnA * rnA  +  invIB * rnB * rnB;

               impulse    : float32;

               P          : b2Vec2;
            begin
               --  Compute normal impulse
               if K > 0.0 then impulse := -C / K;
               else            impulse := 0.0;
               end if;

               P := impulse * normal;

               --  Track max constraint error.
               minSeparation := float32'Min (minSeparation, separation);

               bodyA.m_sweep.c := bodyA.m_sweep.c - invMassA * P;
               bodyA.m_sweep.a := bodyA.m_sweep.a - invIA * b2Cross (rA, P);
               bodyA.SynchronizeTransform;

               bodyB.m_sweep.c := bodyB.m_sweep.c + invMassB * P;
               bodyB.m_sweep.a := bodyB.m_sweep.a + invIB * b2Cross (rB, P);
               bodyB.SynchronizeTransform;
            end;
         end loop;
      end loop;

      --  We can't expect minSpeparation >= -b2_linearSlop because we don't
      --  push the separation above -b2_linearSlop.
      return minSeparation >= -1.5 * b2_linearSlop;

   end Solve;


end impact.d2.orbs.toi_Solver;
